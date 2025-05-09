use std::collections::HashMap;
use std::fmt;
use std::ops::Neg;

use anyhow::Result;

use crate::expr::{self, Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::{LoxError, Parser, Scanner};

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Value {
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

impl From<&expr::Literal> for Value {
    fn from(value: &expr::Literal) -> Self {
        match value {
            expr::Literal::Number(v) => Value::Number(*v),
            expr::Literal::String(v) => Value::String(v.clone()),
            expr::Literal::True => Value::Boolean(true),
            expr::Literal::False => Value::Boolean(false),
            expr::Literal::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let v = match self {
            Value::Number(n) => {
                let text = n.to_string();
                if text.ends_with(".0") {
                    text[..text.len() - 2].to_string()
                } else {
                    text
                }
            }
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
        };
        f.write_str(&v)
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Value::Number(-n),
            // FIXME: ERROR
            _ => Value::Nil,
        }
    }
}

#[derive(Clone)]
struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            values: Default::default(),
            enclosing: None,
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_enclosing(enclosing: Box<Environment>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values.get(&name.lexeme).unwrap().clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(name);
        }

        Err(RuntimeError {
            token: name.clone(),
            message: format!("Undefined variable '{}'.", name.lexeme),
        })
    }

    pub fn assign(&mut self, name: &Token, val: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.to_string(), val);
            return Ok(());
        }

        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.assign(name, val);
        }

        return Err(RuntimeError {
            token: name.clone(),
            message: format!("Undefined variable '{}'.", name.lexeme),
        });
    }
}

#[derive(Default)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), RuntimeError> {
        for stmt in &stmts {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_number_operand(&self, token: &Token, operand: &Value) -> Result<(), RuntimeError> {
        if let Value::Number(_) = operand {
            Ok(())
        } else {
            Err(RuntimeError {
                token: token.clone(),
                message: "Operand must be a number.".to_string(),
            })
        }
    }

    fn check_number_operands(
        &self,
        token: &Token,
        left: &Value,
        right: &Value,
    ) -> Result<(), RuntimeError> {
        if let (Value::Number(_), Value::Number(_)) = (left, right) {
            Ok(())
        } else {
            Err(RuntimeError {
                token: token.clone(),
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn binary_op<F>(
        &self,
        token: &Token,
        a: &Value,
        b: &Value,
        op: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(f64, f64) -> f64,
    {
        self.check_number_operands(token, a, b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(Value::Number(op(*a_val, *b_val)))
        } else {
            // This should be unreachable due to the check above
            Err(RuntimeError {
                token: token.clone(),
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn comparison_op<F>(
        &self,
        token: &Token,
        a: &Value,
        b: &Value,
        op: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(f64, f64) -> bool,
    {
        self.check_number_operands(token, a, b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(Value::Boolean(op(*a_val, *b_val)))
        } else {
            Err(RuntimeError {
                token: token.clone(),
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn handle_plus(&self, token: &Token, a: &Value, b: &Value) -> Result<Value, RuntimeError> {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(format!("{}{}", s1, s2))),
            _ => Err(RuntimeError {
                token: token.clone(),
                message: "Operands must be two numbers or two strings.".to_string(),
            }),
        }
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Class => todo!(),
            Stmt::Expression(expr) => {
                let _value = self.evaluate_expr(expr)?;
                // println!("{}", value);
            }
            Stmt::Function => todo!(),
            Stmt::Print(expr) => {
                let value = self.evaluate_expr(expr)?;
                println!("{}", value);
            }
            Stmt::Return => todo!(),
            Stmt::While => todo!(),
            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(v) => self.evaluate_expr(v)?,
                    None => Value::Nil,
                };
                self.env.define(&name.lexeme, value);
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.evaluate_expr(condition)?.is_truthy() {
                    self.execute_stmt(then_stmt)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.execute_stmt(else_stmt)?;
                }
            }
            Stmt::Block(stmts) => {
                let block_env = Environment::with_enclosing(Box::new(self.env.clone()));
                self.execute_block(stmts, block_env)?;
            }
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        stmts: &[Box<Stmt>],
        block_env: Environment,
    ) -> Result<Value, RuntimeError> {
        let outer_env = self.env.clone();

        self.env = block_env.clone();

        let mut err = None;
        for stmt in stmts {
            if let Err(e) = self.execute_stmt(stmt) {
                err = Some(e);
                break;
            }
        }

        self.env = outer_env;
        if let Some(e) = err {
            return Err(e);
        }

        Ok(Value::Nil)
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Assign(name, value) => {
                let val = self.evaluate_expr(value)?;
                self.env.assign(name, val.clone())?;
                Ok(val)
            }
            Expr::Binary(left, operator, right) => {
                let left_val = self.evaluate_expr(left)?;
                let right_val = self.evaluate_expr(right)?;
                match operator.typ {
                    TokenType::Plus => self.handle_plus(operator, &left_val, &right_val),
                    TokenType::Minus => {
                        self.binary_op(operator, &left_val, &right_val, |a, b| a - b)
                    }
                    TokenType::Slash => {
                        self.binary_op(operator, &left_val, &right_val, |a, b| a / b)
                    }
                    TokenType::Star => {
                        self.binary_op(operator, &left_val, &right_val, |a, b| a * b)
                    }
                    TokenType::Greater => {
                        self.comparison_op(operator, &left_val, &right_val, |a, b| a > b)
                    }
                    TokenType::GreaterEqual => {
                        self.comparison_op(operator, &left_val, &right_val, |a, b| a >= b)
                    }
                    TokenType::Less => {
                        self.comparison_op(operator, &left_val, &right_val, |a, b| a < b)
                    }
                    TokenType::LessEqual => {
                        self.comparison_op(operator, &left_val, &right_val, |a, b| a <= b)
                    }
                    TokenType::BangEqual => Ok(Value::Boolean(!left_val.is_equal(&right_val))),
                    TokenType::EqualEqual => Ok(Value::Boolean(left_val.is_equal(&right_val))),
                    _ => Err(RuntimeError {
                        token: operator.clone(),
                        message: "Unsupported operator.".to_string(),
                    }),
                }
            }
            Expr::Call(expr, token, exprs) => todo!(),
            Expr::Get(expr, token) => todo!(),
            Expr::Grouping(expr) => self.evaluate_expr(&expr),
            Expr::Literal(literal) => Ok(Value::from(literal)),
            Expr::Logical(expr, token, expr1) => todo!(),
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate_expr(&right)?;
                match operator.typ {
                    TokenType::Minus => {
                        self.check_number_operand(operator, &right_val)?;
                        Ok(-right_val)
                    }
                    TokenType::Bang => Ok(Value::Boolean(!right_val.is_truthy())),
                    _ => Err(RuntimeError {
                        token: operator.clone(),
                        message: "Unsupported unary operator.".to_string(),
                    }),
                }
            }
            Expr::Variable(name) => self.env.get(name),
        }
    }
}

pub fn run_interpreter(source_code: &str, interpreter: &mut Interpreter) -> Result<(), LoxError> {
    print!("{}", source_code);
    let mut scanner = Scanner::new(source_code);
    scanner.scan_tokens();

    for token in &scanner.tokens {
        if token.typ == TokenType::Error {
            eprintln!("[line {}] Error: Unexpected character.", token.line);
            return Err(LoxError::CompileError);
        }
        println!("{}", token);
    }

    let mut parser = Parser::new(scanner.tokens);
    match parser.parse() {
        Ok(stmts) => {
            let res = interpreter.interpret(stmts);
            match res {
                Ok(v) => Ok(v),
                Err(e) => {
                    runtime_error(e);
                    Err(LoxError::RuntimeError)
                }
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(LoxError::CompileError)
        }
    }
}

fn runtime_error(e: RuntimeError) {
    eprintln!("[line {}] {}", e.token.line, e.message);
}
