use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::rc::Rc;

use anyhow::Result;
use bumpalo::{collections::Vec as BVec, Bump};

use crate::expr::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::Literal;

type InterpretResult<'a, T> = Result<T, InterpretError<'a>>;

pub trait Callable<'a>: ToString + Debug {
    fn arity(&self) -> usize;
    fn call(
        &self,
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

impl<'a> ToString for NativeFunction<'a> {
    fn to_string(&self) -> String {
        "<native fn>".to_string()
    }
}

#[derive(Clone, Debug)]
struct LoxFunction<'a> {
    name: &'a Token<'a>,
    params: &'a [&'a Token<'a>],
    body: &'a Stmt<'a>,
    closure: Rc<RefCell<Environment<'a>>>,
}

impl<'a> LoxFunction<'a> {
    pub fn new(
        name: &'a Token<'a>,
        params: &'a [&'a Token<'a>],
        body: &'a Stmt<'a>,
        closure: Rc<RefCell<Environment<'a>>>,
    ) -> Self {
        Self {
            name,
            params,
            body,
            closure,
        }
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
            Stmt::Block(stmts) => match interpreter.execute_block(&stmts, env) {
                Ok(v) => {
                    return Ok(v);
                }
                Err(err) => match err {
                    InterpretError::Return(v) => {
                        return Ok(v.value);
                    }
                    _ => {
                        return Err(err);
                    }
                },
            },
            _ => panic!("Expect Block"),
        }
    }
}

impl<'a> ToString for LoxFunction<'a> {
    fn to_string(&self) -> String {
        format!("<fn {}>", self.name.lexeme)
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
pub enum Value<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
    Callable(&'a dyn Callable<'a>),
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
            Value::Callable(callable) => {
                f.write_str(&callable.to_string())?;
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

    fn get(&self, name: &str) -> Result<&'a Value<'a>, String> {
        if self.values.contains_key(name) {
            return Ok(self.values.get(name).unwrap());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(format!("Undefined variable '{}'.", name))
    }

    fn assign(&mut self, name: &'a str, val: &'a Value<'a>) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name, val);
            return Ok(());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, val);
        }

        Err(format!("Undefined variable '{}'.", name))
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
            bump.alloc(Value::Callable(bump.alloc(NativeFunction {
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
        self.check_number_operands(token, &a, &b)?;
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
        self.check_number_operands(token, &a, &b)?;
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
            Stmt::Class => todo!(),
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
                let f =
                    self.bump
                        .alloc(LoxFunction::new(name, params, &body, Rc::clone(&self.env)));
                self.env
                    .borrow_mut()
                    .define(name.lexeme, self.bump.alloc(Value::Callable(f)));
            }
            Stmt::Return(_keyword, value) => {
                let val = match value {
                    Some(v) => self.evaluate_expr(v)?,
                    None => self.bump.alloc(Value::Nil),
                };
                return Err(InterpretError::Return(Return { value: val }));
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

    #[allow(unused)]
    fn evaluate_expr(&mut self, expr: &'a Expr<'a>) -> InterpretResult<'a, &'a Value<'a>> {
        match expr {
            Expr::Assign(token, value) => {
                let val = self.evaluate_expr(value)?;
                if let Some(&depth) = self.locals.get(&(expr as *const Expr<'a>)) {
                    self.assign_at(depth, token.lexeme, val, token)?;
                } else {
                    return match self.env.borrow_mut().assign(token.lexeme, val) {
                        Ok(()) => Ok(val),
                        Err(msg) => Err(RuntimeError {
                            token,
                            message: msg,
                        }
                        .into()),
                    };
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
                        .alloc(Value::Boolean(!left_val.is_equal(&right_val)))),
                    TokenType::EqualEqual => Ok(self
                        .bump
                        .alloc(Value::Boolean(left_val.is_equal(&right_val)))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported operator.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Call(callee, paren, args) => {
                let callee = self.evaluate_expr(&callee)?;

                let arguments: Vec<_> = args
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Result<_, _>>()?;

                let arguments_slice = self.bump.alloc_slice_copy(&arguments);

                match callee {
                    Value::Callable(callable) => {
                        // Call the function
                        if callable.arity() != arguments.len() {
                            return Err(RuntimeError {
                                token: paren,
                                message: format!(
                                    "Expected {} arguments but got {}.",
                                    callable.arity(),
                                    arguments.len()
                                ),
                            }
                            .into());
                        }

                        callable.call(self, arguments_slice)
                    }
                    _ => Err(RuntimeError {
                        token: paren,
                        message: "Can only call functions and classes.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Get(expr, token) => todo!(),
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
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
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
            Expr::Variable(token) => {
                if let Some(&depth) = self.locals.get(&(expr as *const Expr<'a>)) {
                    return self.get_at(depth, token.lexeme, token);
                }
                match self.env.borrow().get(token.lexeme) {
                    Ok(v) => Ok(v),
                    Err(msg) => Err(RuntimeError {
                        token,
                        message: msg,
                    }
                    .into()),
                }
            }
        }
    }

    fn get_at(
        &self,
        distance: usize,
        name: &str,
        token: &'a Token<'a>,
    ) -> InterpretResult<'a, &'a Value<'a>> {
        let mut env = self.env.clone();
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
        let x = match env.borrow().get(name) {
            Ok(v) => Ok(v),
            Err(msg) => Err(RuntimeError {
                token,
                message: msg,
            }
            .into()),
        };
        x
    }

    fn assign_at(
        &self,
        distance: usize,
        name: &'a str,
        value: &'a Value<'a>,
        token: &'a Token<'a>,
    ) -> InterpretResult<'a, ()> {
        let mut env = self.env.clone();
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
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum FunctionType {
    None,
    Function,
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut TreewalkInterpreter<'a>,
    scopes: Vec<HashMap<&'a str, bool>>,
    current_function: FunctionType,
    pub had_error: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut TreewalkInterpreter<'a>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None,
            had_error: false,
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
            .map_or(false, |scope| scope.contains_key(name));

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
            Stmt::Class => todo!(),
            Stmt::Expression(expr) => self.resolve_expr(expr),
            Stmt::Function(name, params, body) => {
                self.declare(name.lexeme, name);
                self.define(name.lexeme);
                self.resolve_function(params, body);
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
        }
    }

    fn resolve_function(&mut self, params: &'a [&'a Token<'a>], body: &'a Stmt<'a>) {
        let enclosing_function = self.current_function;
        self.current_function = FunctionType::Function;
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
            Expr::Get(_, _) | Expr::Set(_, _, _) | Expr::Super(_, _) | Expr::This(_) => todo!(),
        }
    }
}
