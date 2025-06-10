use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::rc::Rc;

use anyhow::Result;
use bumpalo::{collections::Vec as BVec, Bump};

use crate::expr::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::Literal;

type InterpretResult<'a, T> = Result<T, InterpretError<'a>>;

trait Callable<'a> {
    fn arity(&self) -> usize;
    fn call(
        &'a self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> InterpretResult<'a, &'a Value<'a>>;
}

#[derive(Clone, Debug)]
struct NativeFunction<'a> {
    callable:
        fn(&mut TreewalkInterpreter<'a>, &'a [&'a Value<'a>]) -> InterpretResult<'a, &'a Value<'a>>,
    arit: usize,
}

impl<'a> Callable<'a> for NativeFunction<'a> {
    fn call(
        &self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> InterpretResult<'a, &'a Value<'a>> {
        (self.callable)(interpreter, args)
    }

    fn arity(&self) -> usize {
        self.arit
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum ClassType {
    None,
    Class,
    SubClass,
}

#[derive(Clone, Debug)]
struct LoxClass<'a> {
    name: &'a Token<'a>,
    methods: HashMap<&'a str, &'a Value<'a>>,
    super_class: Option<&'a Value<'a>>,
}

impl<'a> LoxClass<'a> {
    fn find_method(&self, name: &str) -> Option<&&'a Value<'a>> {
        self.methods.get(name).or_else(|| {
            self.super_class.and_then(|super_class| {
                if let Value::Class(c) = super_class {
                    c.find_method(name)
                } else {
                    panic!("Expected Value::Class for super_class, but got another variant for method: {}", name)
                }
            })
        })
    }
}

impl<'a> Callable<'a> for LoxClass<'a> {
    fn arity(&self) -> usize {
        if let Some(Value::Function(initializer)) = self.find_method("init") {
            return initializer.arity();
        }
        0
    }

    fn call(
        &'a self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> InterpretResult<'a, &'a Value<'a>> {
        let instance = interpreter.bump.alloc(LoxInstance::new(self));
        if let Some(Value::Function(initializer)) = self.find_method("init") {
            initializer
                .bind(instance, interpreter)
                .call(interpreter, args)?;
        }
        Ok(interpreter.bump.alloc(Value::Instance(instance)))
    }
}

#[derive(Clone, Debug)]
struct LoxInstance<'a> {
    class: &'a LoxClass<'a>,
    fields: RefCell<HashMap<&'a str, &'a Value<'a>>>,
}

impl<'a> LoxInstance<'a> {
    fn new(class: &'a LoxClass<'a>) -> Self {
        Self {
            class,
            fields: RefCell::new(HashMap::default()),
        }
    }

    fn get(
        &'a self,
        name: &'a Token<'a>,
        interpreter: &mut TreewalkInterpreter<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        if self.fields.borrow().contains_key(name.lexeme) {
            return Ok(self.fields.borrow().get(name.lexeme).unwrap());
        }

        if let Some(m) = self.class.find_method(name.lexeme) {
            match m {
                Value::Function(method) => {
                    return Ok(interpreter
                        .bump
                        .alloc(Value::Function(method.bind(self, interpreter))));
                }
                _ => return Ok(m),
            }
        }

        Err(RuntimeError {
            token: name,
            message: format!("Undefined property '{}'.", name.lexeme),
        }
        .into())
    }

    fn set(&self, name: &'a Token<'a>, value: &'a Value<'a>) {
        self.fields.borrow_mut().insert(name.lexeme, value);
    }
}

#[derive(Clone, Debug)]
struct LoxFunction<'a> {
    name: &'a Token<'a>,
    params: &'a [&'a Token<'a>],
    body: &'a Stmt<'a>,
    closure: Rc<RefCell<Environment<'a>>>,
    is_initializer: bool,
}

impl<'a> LoxFunction<'a> {
    pub fn new(
        name: &'a Token<'a>,
        params: &'a [&'a Token<'a>],
        body: &'a Stmt<'a>,
        closure: Rc<RefCell<Environment<'a>>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            name,
            params,
            body,
            closure,
            is_initializer,
        }
    }

    pub fn bind(
        &self,
        instance: &'a LoxInstance<'a>,
        interpreter: &mut TreewalkInterpreter<'a>,
    ) -> &'a LoxFunction<'a> {
        let mut environment = Environment::with_enclosing(Rc::clone(&self.closure));

        let this = interpreter.bump.alloc(Value::Instance(instance));
        environment.define("this", this);

        interpreter.bump.alloc(LoxFunction::new(
            self.name,
            self.params,
            self.body,
            Rc::new(RefCell::new(environment)),
            self.is_initializer,
        ))
    }
}

impl<'a> Callable<'a> for LoxFunction<'a> {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> InterpretResult<'a, &'a Value<'a>> {
        let mut env = Environment::with_enclosing(Rc::clone(&self.closure));
        for i in 0..self.params.len() {
            env.define(self.params.get(i).unwrap().lexeme, args.get(i).unwrap());
        }
        match self.body {
            Stmt::Block(stmts) => match interpreter.execute_block(stmts, env) {
                Ok(v) => {
                    if self.is_initializer {
                        return Ok(get_at(self.closure.clone(), 0, "this").unwrap());
                    }
                    Ok(v)
                }
                Err(err) => match err {
                    InterpretError::Return(v) => {
                        if self.is_initializer {
                            return Ok(get_at(self.closure.clone(), 0, "this").unwrap());
                        }
                        Ok(v.value)
                    }
                    _ => Err(err),
                },
            },
            _ => panic!("Expect Block"),
        }
    }
}

#[derive(Debug)]
pub enum InterpretError<'a> {
    Runtime(RuntimeError<'a>),
    Break(Break<'a>),
    Return(Return<'a>),
}

impl<'a> From<RuntimeError<'a>> for InterpretError<'a> {
    fn from(err: RuntimeError<'a>) -> Self {
        InterpretError::Runtime(err)
    }
}

#[derive(Debug)]
pub struct Return<'a> {
    value: &'a Value<'a>,
}

#[derive(Debug)]
pub struct Break<'a> {
    token: &'a Token<'a>,
}

#[derive(Debug)]
pub struct RuntimeError<'a> {
    pub token: &'a Token<'a>,
    pub message: String,
}

impl<'a> From<InterpretError<'a>> for RuntimeError<'a> {
    fn from(value: InterpretError<'a>) -> Self {
        match value {
            InterpretError::Runtime(runtime_error) => runtime_error,
            _ => panic!("Cannot convert to RuntimeError"),
        }
    }
}

#[derive(Clone, Debug)]
enum Value<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Instance(&'a LoxInstance<'a>),
    NativeFunction(&'a NativeFunction<'a>),
    Function(&'a LoxFunction<'a>),
    Class(&'a LoxClass<'a>),
    Nil,
}

impl<'a> Value<'a> {
    fn is_truthy(&self) -> bool {
        match self {
            // Value::Number(v) => *v != 0.0,
            // Value::String(v) => !v.is_empty(),
            Value::Boolean(false) | Value::Nil => false,
            _ => true,
        }
    }

    fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) | (_, Value::Nil) => false,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            _ => false,
        }
    }
}

impl<'a> From<&Literal<'a>> for Value<'a> {
    fn from(value: &Literal<'a>) -> Self {
        match value {
            Literal::Number(v) => Value::Number(*v),
            Literal::String(v) => Value::String(v),
            Literal::True => Value::Boolean(true),
            Literal::False => Value::Boolean(false),
            Literal::Nil => Value::Nil,
        }
    }
}

impl<'a> fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => {
                let text = n.to_string();
                if text.ends_with(".0") {
                    f.write_str(&text[..text.len() - 2])?;
                } else {
                    f.write_str(&text)?;
                }
            }
            Value::String(s) => {
                f.write_str(s)?;
            }
            Value::Boolean(b) => {
                f.write_str(&b.to_string())?;
            }
            Value::Nil => {
                f.write_str("nil")?;
            }
            Value::NativeFunction(_native_fn) => {
                write!(f, "<native fn>")?;
            }
            Value::Function(lox_fn) => {
                write!(f, "<fn {}>", lox_fn.name.lexeme)?;
            }
            Value::Class(lox_class) => {
                write!(f, "{}", lox_class.name.lexeme)?;
            }
            Value::Instance(lox_instance) => {
                write!(f, "{} instance", lox_instance.class.name.lexeme)?;
            }
        };
        Ok(())
    }
}

#[derive(Default, Debug)]
struct Environment<'a> {
    values: HashMap<&'a str, &'a Value<'a>>,
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
}

impl<'a> Display for Environment<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let values_str = format!("{:?}", self.values);
        write!(f, "{}", values_str)?;
        if let Some(enclosing) = &self.enclosing {
            write!(f, " -> {}", enclosing.borrow())?;
        }
        Ok(())
    }
}

impl<'a> Environment<'a> {
    fn with_enclosing(enclosing: Rc<RefCell<Environment<'a>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    fn define(&mut self, name: &'a str, value: &'a Value<'a>) {
        self.values.insert(name, value);
    }

    fn get(&self, name: &'a str) -> Result<&'a Value<'a>, String> {
        if self.values.contains_key(name) {
            return Ok(self.values.get(name).unwrap());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(format!("Undefined variable '{}'.", name))
    }

    fn assign(
        &mut self,
        name: &'a Token<'a>,
        val: &'a Value<'a>,
    ) -> Result<(), InterpretError<'a>> {
        if self.values.contains_key(name.lexeme) {
            self.values.insert(name.lexeme, val);
            return Ok(());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, val);
        }

        Err(RuntimeError {
            message: format!("Undefined variable '{}'.", name.lexeme),
            token: name,
        }
        .into())
    }
}

pub struct TreewalkInterpreter<'a> {
    env: Rc<RefCell<Environment<'a>>>,
    pub bump: &'a Bump,
    locals: HashMap<*const Expr<'a>, usize>,
}

impl<'a> TreewalkInterpreter<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        let env = Rc::new(RefCell::new(Environment::default()));
        env.borrow_mut().define(
            "clock",
            bump.alloc(Value::NativeFunction(bump.alloc(NativeFunction {
                arit: 0,
                callable: |interpreter: &mut TreewalkInterpreter<'a>,
                           _args: &'a [&'a Value<'a>]| {
                    use std::time::SystemTime;
                    let now = SystemTime::now();
                    let duration = now.duration_since(SystemTime::UNIX_EPOCH).unwrap();
                    let seconds = duration.as_secs_f64();
                    Ok(interpreter.bump.alloc(Value::Number(seconds)))
                },
            }))),
        );
        Self {
            env,
            bump,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmts: &BVec<'a, &'a Stmt<'a>>) -> Result<(), RuntimeError<'a>> {
        for stmt in stmts {
            match self.execute_stmt(stmt) {
                Ok(()) => {}
                Err(InterpretError::Break(b)) => {
                    return Err(RuntimeError {
                        token: b.token,
                        message: "Break statement must be inside a loop.".to_string(),
                    })
                }
                Err(e) => return Err(e.into()),
            }
        }
        Ok(())
    }

    fn resolve(&mut self, expr: &'a Expr<'a>, depth: usize) {
        self.locals.insert(expr as *const Expr<'a>, depth);
    }

    fn check_number_operand(
        &self,
        token: &'a Token<'a>,
        operand: &'a Value<'a>,
    ) -> InterpretResult<'a, ()> {
        if let Value::Number(_) = operand {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operand must be a number.".to_string(),
            }
            .into())
        }
    }

    fn check_number_operands(
        &self,
        token: &'a Token<'a>,
        left: &'a Value<'a>,
        right: &'a Value<'a>,
    ) -> InterpretResult<'a, ()> {
        if let (Value::Number(_), Value::Number(_)) = (left, right) {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            }
            .into())
        }
    }

    fn binary_op<F>(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
        op: F,
    ) -> InterpretResult<'a, &'a Value<'a>>
    where
        F: Fn(f64, f64) -> f64,
    {
        self.check_number_operands(token, a, b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(self.bump.alloc(Value::Number(op(*a_val, *b_val))))
        } else {
            // This should be unreachable due to the check above
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            }
            .into())
        }
    }

    fn comparison_op<F>(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
        op: F,
    ) -> InterpretResult<'a, &'a Value<'a>>
    where
        F: Fn(f64, f64) -> bool,
    {
        self.check_number_operands(token, a, b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(self.bump.alloc(Value::Boolean(op(*a_val, *b_val))))
        } else {
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            }
            .into())
        }
    }

    fn handle_plus(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => Ok(self.bump.alloc(Value::Number(n1 + n2))),
            (Value::String(s1), Value::String(s2)) => Ok(self
                .bump
                .alloc(Value::String(self.bump.alloc_str(&format!("{}{}", s1, s2))))),
            _ => Err(RuntimeError {
                token,
                message: "Operands must be two numbers or two strings.".to_string(),
            }
            .into()),
        }
    }

    fn execute_stmt(&mut self, stmt: &'a Stmt<'a>) -> InterpretResult<'a, ()> {
        match stmt {
            Stmt::Expression(expr) => {
                let _value = self.evaluate_expr(expr)?;
            }
            Stmt::Print(expr) => {
                let value = self.evaluate_expr(expr)?;
                println!("{}", value);
            }
            Stmt::Var(token, initializer) => {
                let value = match initializer {
                    Some(v) => self.evaluate_expr(v)?,
                    None => self.bump.alloc(Value::Nil),
                };
                self.env.borrow_mut().define(token.lexeme, value);
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.evaluate_expr(condition)?.is_truthy() {
                    self.execute_stmt(then_stmt)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.execute_stmt(else_stmt)?;
                }
            }
            Stmt::Block(stmts) => {
                let block_env = Environment::with_enclosing(Rc::clone(&self.env));
                self.execute_block(stmts, block_env)?;
            }
            Stmt::While(condition, body) => {
                while self.evaluate_expr(condition)?.is_truthy() {
                    match self.execute_stmt(body) {
                        Ok(()) => {}
                        Err(InterpretError::Break(_)) => break,
                        Err(e) => return Err(e),
                    };
                }
            }
            Stmt::Break(token) => {
                return Err(InterpretError::Break(Break { token }));
            }
            Stmt::Function(name, params, body) => {
                let f = self.bump.alloc(LoxFunction::new(
                    name,
                    params,
                    body,
                    Rc::clone(&self.env),
                    false,
                ));
                self.env
                    .borrow_mut()
                    .define(name.lexeme, self.bump.alloc(Value::Function(f)));
            }
            Stmt::Return(_keyword, value) => {
                let val = match value {
                    Some(v) => self.evaluate_expr(v)?,
                    None => self.bump.alloc(Value::Nil),
                };
                return Err(InterpretError::Return(Return { value: val }));
            }
            Stmt::Class(name, super_class, methods) => {
                let super_class_val = if let Some(super_class) = super_class {
                    let super_class_variable = if let Expr::Variable(v) = super_class {
                        v
                    } else {
                        panic!("Expected Expr::Variable, but got another variant")
                    };
                    let x = self.evaluate_expr(super_class)?;
                    if !matches!(x, Value::Class(_)) {
                        return Err(RuntimeError {
                            token: super_class_variable,
                            message: "Superclass must be a class.".to_string(),
                        }
                        .into());
                    }
                    Some(x)
                } else {
                    None
                };

                self.env
                    .borrow_mut()
                    .define(name.lexeme, self.bump.alloc(Value::Nil));

                if super_class_val.is_some() {
                    let new_env =
                        Rc::new(RefCell::new(Environment::with_enclosing(self.env.clone())));
                    new_env
                        .borrow_mut()
                        .define("super", super_class_val.unwrap());
                    self.env = new_env;
                }

                let mut c_methods = HashMap::new();
                for method in methods {
                    if let Stmt::Function(name, params, body) = method {
                        let f =
                            self.bump
                                .alloc(Value::Function(self.bump.alloc(LoxFunction::new(
                                    name,
                                    params,
                                    body,
                                    Rc::clone(&self.env),
                                    name.lexeme.eq("init"),
                                ))));
                        c_methods.insert(name.lexeme, &*f);
                    } else {
                        panic!("expected function statement");
                    }
                }

                let class = self.bump.alloc(Value::Class(self.bump.alloc(LoxClass {
                    name,
                    super_class: super_class_val,
                    methods: c_methods,
                })));

                if super_class_val.is_some() {
                    let new_env = {
                        let env = self.env.borrow();
                        env.enclosing.clone().unwrap()
                    };
                    self.env = new_env;
                }

                self.env.borrow_mut().assign(name, class)?;
            }
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        stmts: &'a [&Stmt<'a>],
        block_env: Environment<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        let previous = Rc::clone(&self.env);

        self.env = Rc::new(RefCell::new(block_env));

        let mut result: InterpretResult<'a, &'a Value<'a>> = Ok(self.bump.alloc(Value::Nil));
        for stmt in stmts {
            match self.execute_stmt(stmt) {
                Ok(()) => {}
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
        }

        self.env = previous;
        result
    }

    fn look_up_variable(
        &mut self,
        name: &'a Token,
        expr: &'a Expr<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        let result = if let Some(&depth) = self.locals.get(&(expr as *const Expr<'a>)) {
            get_at(Rc::clone(&self.env), depth, name.lexeme)
        } else {
            self.env.borrow().get(name.lexeme)
        };

        result.map_err(|msg| {
            InterpretError::Runtime(RuntimeError {
                token: name,
                message: msg,
            })
        })
    }

    #[allow(unused)]
    fn evaluate_expr(&mut self, expr: &'a Expr<'a>) -> InterpretResult<'a, &'a Value<'a>> {
        match expr {
            Expr::Assign(token, value) => {
                let val = self.evaluate_expr(value)?;
                if let Some(&depth) = self.locals.get(&(expr as *const Expr<'a>)) {
                    assign_at(self.env.clone(), depth, token.lexeme, val, token)?;
                } else {
                    self.env.borrow_mut().assign(token, val)?;
                    return Ok(val);
                }
                Ok(val)
            }
            Expr::Binary(left, operator, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match operator.typ {
                    TokenType::Plus => self.handle_plus(operator, left_val, right_val),
                    TokenType::Minus => self.binary_op(operator, left_val, right_val, |a, b| a - b),
                    TokenType::Slash => self.binary_op(operator, left_val, right_val, |a, b| a / b),
                    TokenType::Star => self.binary_op(operator, left_val, right_val, |a, b| a * b),
                    TokenType::Greater => {
                        self.comparison_op(operator, left_val, right_val, |a, b| a > b)
                    }
                    TokenType::GreaterEqual => {
                        self.comparison_op(operator, left_val, right_val, |a, b| a >= b)
                    }
                    TokenType::Less => {
                        self.comparison_op(operator, left_val, right_val, |a, b| a < b)
                    }
                    TokenType::LessEqual => {
                        self.comparison_op(operator, left_val, right_val, |a, b| a <= b)
                    }
                    TokenType::BangEqual => Ok(self
                        .bump
                        .alloc(Value::Boolean(!left_val.is_equal(right_val)))),
                    TokenType::EqualEqual => Ok(self
                        .bump
                        .alloc(Value::Boolean(left_val.is_equal(right_val)))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported operator.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Call(callee, paren, args) => {
                let callee = self.evaluate_expr(callee)?;

                let arguments: Vec<_> = args
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Result<_, _>>()?;

                let arguments_slice = self.bump.alloc_slice_copy(&arguments);

                let callable: &dyn Callable<'a> = match callee {
                    Value::NativeFunction(native_fn) => *native_fn,
                    Value::Function(lox_fn) => *lox_fn,
                    Value::Class(lox_class) => *lox_class,
                    _ => {
                        return Err(RuntimeError {
                            token: paren,
                            message: "Can only call functions and classes.".to_string(),
                        }
                        .into())
                    }
                };

                self.call_with_arity_check(callable, arguments_slice, paren)
            }
            Expr::Get(expr, name) => {
                let obj = self.evaluate_expr(expr)?;
                match obj {
                    Value::Instance(lox_instance) => lox_instance.get(name, self),
                    _ => Err(RuntimeError {
                        token: name,
                        message: "Only instances have properties.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Literal(literal) => Ok(self.bump.alloc(Value::from(literal))),
            Expr::Logical(left_expr, operator, right_expr) => {
                let left = self.evaluate_expr(left_expr)?;

                match operator.typ {
                    TokenType::Or => {
                        if left.is_truthy() {
                            return Ok(left);
                        }
                    }
                    _ => {
                        if !left.is_truthy() {
                            return Ok(left);
                        }
                    }
                };

                self.evaluate_expr(right_expr)
            }
            Expr::Set(obj, name, value) => {
                let obj = self.evaluate_expr(obj)?;

                match obj {
                    Value::Instance(lox_instance) => {
                        let value = self.evaluate_expr(value)?;
                        lox_instance.set(name, value);
                        Ok(value)
                    }
                    _ => Err(RuntimeError {
                        token: name,
                        message: "Only instances have properties.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Super(keyword, method) => {
                let distance = *self.locals.get(&(expr as *const Expr<'a>)).unwrap();
                let super_class = get_at(self.env.clone(), distance, "super").unwrap();
                let object = match get_at(self.env.clone(), distance - 1, "this").unwrap() {
                    Value::Instance(lox_instance) => lox_instance,
                    _ => panic!("unexpected error"),
                };

                let method = if let Value::Class(f) = super_class {
                    match f.find_method(method.lexeme) {
                        Some(v) => v,
                        None => {
                            return Err(RuntimeError {
                                token: method,
                                message: format!("Undefined property '{}'.", method.lexeme)
                                    .to_string(),
                            }
                            .into())
                        }
                    }
                } else {
                    panic!("unexpected error");
                };

                match method {
                    Value::Function(lox_function) => Ok(self
                        .bump
                        .alloc(Value::Function(lox_function.bind(object, self)))),
                    _ => panic!("unexpected error"),
                }
            }
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate_expr(right)?;
                match operator.typ {
                    TokenType::Minus => {
                        self.check_number_operand(operator, right_val)?;
                        match right_val {
                            Value::Number(n) => Ok(self.bump.alloc(Value::Number(-n))),
                            // FIXME: ERROR
                            _ => Ok(self.bump.alloc(Value::Nil)),
                        }
                    }
                    TokenType::Bang => Ok(self.bump.alloc(Value::Boolean(!right_val.is_truthy()))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported unary operator.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Variable(token) => self.look_up_variable(token, expr),
            Expr::This(token) => self.look_up_variable(token, expr),
        }
    }

    fn call_with_arity_check(
        &mut self,
        callable: &'a dyn Callable<'a>,
        args: &'a [&'a Value<'a>],
        paren: &'a Token<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        if callable.arity() != args.len() {
            return Err(RuntimeError {
                token: paren,
                message: format!(
                    "Expected {} arguments but got {}.",
                    callable.arity(),
                    args.len()
                ),
            }
            .into());
        }
        callable.call(self, args)
    }
}

fn get_at<'a>(
    env: Rc<RefCell<Environment<'a>>>,
    distance: usize,
    name: &'a str,
) -> Result<&'a Value<'a>, String> {
    let mut env = env.clone();
    for _ in 0..distance {
        let enclosing = env.borrow().enclosing.clone().unwrap();
        env = enclosing;
    }
    let v = env.borrow().get(name)?;
    Ok(v)
}

fn assign_at<'a>(
    env: Rc<RefCell<Environment<'a>>>,
    distance: usize,
    name: &'a str,
    value: &'a Value<'a>,
    token: &'a Token<'a>,
) -> InterpretResult<'a, ()> {
    let mut env = env.clone();
    for _ in 0..distance {
        let enclosing = env
            .borrow()
            .enclosing
            .clone()
            .ok_or(InterpretError::Runtime(RuntimeError {
                token,
                message: "No enclosing environment".to_string(),
            }))?;
        env = enclosing;
    }
    let mut borrowed = env.borrow_mut();
    if borrowed.values.contains_key(name) {
        borrowed.values.insert(name, value);
        Ok(())
    } else {
        Err(InterpretError::Runtime(RuntimeError {
            token,
            message: format!("Undefined variable '{}'.", name),
        }))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut TreewalkInterpreter<'a>,
    scopes: Vec<HashMap<&'a str, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    pub had_error: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut TreewalkInterpreter<'a>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            had_error: false,
            current_class: ClassType::None,
        }
    }

    fn error(&mut self, token: &'a Token<'a>, message: &str) {
        eprintln!("[line {}] Error: {}", token.line, message);
        self.had_error = true;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &'a str, token: &'a Token<'a>) {
        let alread_declare = self
            .scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name));

        if alread_declare {
            self.error(token, "Already a variable with this name in this scope.");
            return;
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, false);
        }
    }

    fn define(&mut self, name: &'a str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }

    fn resolve_local(&mut self, expr: &'a Expr<'a>, token: &'a Token<'a>) {
        for (depth, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(token.lexeme) {
                self.interpreter.resolve(expr, depth);
                return;
            }
        }
    }

    pub fn resolve(&mut self, stmts: &BVec<'a, &'a Stmt<'a>>) {
        self.resolve_stmts(stmts);
    }

    fn resolve_stmts(&mut self, stmts: &BVec<'a, &'a Stmt<'a>>) {
        for stmt in stmts {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &'a Stmt<'a>) {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve_stmts(stmts);
                self.end_scope();
            }
            Stmt::Expression(expr) => self.resolve_expr(expr),
            Stmt::Function(name, params, body) => {
                self.declare(name.lexeme, name);
                self.define(name.lexeme);
                self.resolve_function(params, body, FunctionType::Function);
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_stmt);
                if let Some(else_stmt) = else_stmt {
                    self.resolve_stmt(else_stmt);
                }
            }
            Stmt::Print(expr) => self.resolve_expr(expr),
            Stmt::Return(keyword, value) => {
                if self.current_function == FunctionType::None {
                    self.error(keyword, "Can't return from top-level code.");
                }
                if let Some(expr) = value {
                    if self.current_function == FunctionType::Initializer {
                        self.error(keyword, "Can't return a value from an initializer.");
                    }
                    self.resolve_expr(expr);
                }
            }
            Stmt::Var(token, initializer) => {
                self.declare(token.lexeme, token);
                if let Some(init) = initializer {
                    self.resolve_expr(init);
                }
                self.define(token.lexeme);
            }
            Stmt::While(condition, body) => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
            Stmt::Break(_) => {}
            Stmt::Class(name, super_class, methods) => {
                let enclosing_class = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(name.lexeme, name);
                self.define(name.lexeme);

                if let Some(super_class) = super_class {
                    if let Expr::Variable(s) = super_class {
                        if name.lexeme.eq(s.lexeme) {
                            self.error(s, "A class can't inherit from itself.");
                        }
                    } else {
                        panic!("unexpected error");
                    }

                    self.resolve_expr(super_class);
                }

                if super_class.is_some() {
                    self.current_class = ClassType::SubClass;
                    self.begin_scope();
                    self.scopes.last_mut().unwrap().insert("super", true);
                }

                self.begin_scope();
                self.scopes.last_mut().unwrap().insert("this", true);

                for method in methods {
                    if let Stmt::Function(name, params, body) = method {
                        let mut declaration = FunctionType::Method;
                        if name.lexeme.eq("init") {
                            declaration = FunctionType::Initializer;
                        }
                        self.resolve_function(params, body, declaration);
                    } else {
                        panic!("unexpected method type");
                    }
                }

                self.end_scope();
                if super_class.is_some() {
                    self.end_scope();
                }

                self.current_class = enclosing_class;
            }
        }
    }

    fn resolve_function(
        &mut self,
        params: &'a [&'a Token<'a>],
        body: &'a Stmt<'a>,
        f_type: FunctionType,
    ) {
        let enclosing_function = self.current_function;
        self.current_function = f_type;
        self.begin_scope();
        for &param in params {
            self.declare(param.lexeme, param);
            self.define(param.lexeme);
        }
        if let Stmt::Block(stmts) = body {
            self.resolve_stmts(stmts);
        } else {
            panic!("Function body must be a block");
        }
        self.end_scope();
        self.current_function = enclosing_function;
    }

    fn resolve_expr(&mut self, expr: &'a Expr<'a>) {
        match expr {
            Expr::Variable(token) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some(&false) = scope.get(token.lexeme) {
                        self.error(token, "Can't read local variable in its own initializer.");
                    }
                }
                self.resolve_local(expr, token);
            }
            Expr::Assign(token, value) => {
                self.resolve_expr(value);
                self.resolve_local(expr, token);
            }
            Expr::Binary(left, _op, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Call(callee, _paren, args) => {
                self.resolve_expr(callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Grouping(expr) => self.resolve_expr(expr),
            Expr::Literal(_) => {}
            Expr::Logical(left, _op, right) => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Unary(_op, right) => self.resolve_expr(right),
            Expr::Get(object, _name) => {
                self.resolve_expr(object);
            }
            Expr::Set(obj, _name, value) => {
                self.resolve_expr(value);
                self.resolve_expr(obj);
            }
            Expr::This(keyword) => {
                if self.current_class == ClassType::None {
                    self.error(keyword, "Can't use 'this' outside of a class.");
                    return;
                }
                self.resolve_local(expr, keyword);
            }
            Expr::Super(keyword, _method) => {
                if self.current_class == ClassType::None {
                    self.error(keyword, "Can't use 'super' outside of a class.");
                    return;
                } else if self.current_class != ClassType::SubClass {
                    self.error(keyword, "Can't use 'super' in a class with no superclass.");
                    return;
                }
                self.resolve_local(expr, keyword);
            }
        }
    }
}
