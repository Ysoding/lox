use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self};
use std::rc::Rc;

use anyhow::Result;
use bumpalo::{collections::Vec as BVec, Bump};

use crate::expr::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::{Interpreter, Literal, LoxError, Parser, Scanner};

type InterpretResult<'a, T> = Result<T, InterpretError<'a>>;

pub trait Callable<'a>: ToString {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> Result<&'a Value<'a>, String>;
}

#[derive(Clone)]
struct NativeFunction<'a> {
    callable:
        fn(&mut TreewalkInterpreter<'a>, &'a [&'a Value<'a>]) -> Result<&'a Value<'a>, String>,
    arit: usize,
}

impl<'a> Callable<'a> for NativeFunction<'a> {
    fn call(
        &self,
        interpreter: &mut TreewalkInterpreter<'a>,
        args: &'a [&'a Value<'a>],
    ) -> Result<&'a Value<'a>, String> {
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

#[derive(Clone)]
struct LoxFunction<'a> {
    name: &'a Token<'a>,
    params: &'a [&'a Token<'a>],
    body: &'a Stmt<'a>,
}

impl<'a> LoxFunction<'a> {
    pub fn new(name: &'a Token<'a>, params: &'a [&'a Token<'a>], body: &'a Stmt<'a>) -> Self {
        Self { name, params, body }
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
    ) -> Result<&'a Value<'a>, String> {
        let mut env = Environment::with_enclosing(interpreter.env.clone());
        for i in 0..self.params.len() {
            env.define(self.params.get(i).unwrap().lexeme, args.get(i).unwrap());
        }
        match self.body {
            Stmt::Block(stmts) => {
                interpreter.execute_block(&stmts, env).unwrap();
            }
            _ => panic!("Expect Block"),
        }
        Ok(interpreter.bump.alloc(Value::Nil))
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
}

impl<'a> From<RuntimeError<'a>> for InterpretError<'a> {
    fn from(err: RuntimeError<'a>) -> Self {
        InterpretError::Runtime(err)
    }
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

#[derive(Clone)]
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

#[derive(Default)]
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
    bump: &'a Bump,
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
        Self { env, bump }
    }

    pub fn interpret(&mut self, stmts: BVec<'a, &'a Stmt<'a>>) -> Result<(), RuntimeError<'a>> {
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
            Stmt::Return => todo!(),
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
                let f = self.bump.alloc(LoxFunction::new(name, params, &body));
                self.env
                    .borrow_mut()
                    .define(name.lexeme, self.bump.alloc(Value::Callable(f)));
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
                match self.env.borrow_mut().assign(token.lexeme, val) {
                    Ok(()) => Ok(val),
                    Err(msg) => Err(RuntimeError {
                        token,
                        message: msg,
                    }
                    .into()),
                }
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

                        callable.call(self, arguments_slice).map_err(|message| {
                            RuntimeError {
                                token: paren,
                                message,
                            }
                            .into()
                        })
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
            Expr::Variable(token) => match self.env.borrow().get(token.lexeme) {
                Ok(v) => Ok(v),
                Err(msg) => Err(RuntimeError {
                    token,
                    message: msg,
                }
                .into()),
            },
        }
    }
}

impl<'a> Interpreter<'a> for TreewalkInterpreter<'a> {
    fn run(&mut self, source_code: &'a str) -> Result<(), LoxError> {
        let mut scanner = Scanner::new(&source_code, &self.bump);

        scanner.scan_tokens();

        for token in &scanner.tokens {
            if token.typ == TokenType::Error {
                println!("[line {}] Error: Unexpected character.", token.line);
                return Err(LoxError::CompileError);
            }
        }

        let parser = Parser::new(&scanner.tokens, &self.bump);
        match parser.parse() {
            Ok(stmts) => {
                let res = self.interpret(stmts);
                match res {
                    Ok(v) => Ok(v),
                    Err(e) => {
                        eprintln!("[line {}] {}", e.token.line, e.message);
                        Err(LoxError::RuntimeError)
                    }
                }
            }
            Err(e) => {
                println!("{}", e);
                Err(LoxError::CompileError)
            }
        }
    }
}
