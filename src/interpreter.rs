use std::collections::HashMap;
use std::fmt::{self};
use std::ops::Neg;

use anyhow::Result;
use bumpalo::{collections::Vec as BVec, Bump};

use crate::expr::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::{Literal, LoxError, Parser, Scanner};

#[derive(Debug, Clone)]
pub struct RuntimeError<'a> {
    pub token: &'a Token<'a>,
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

impl<'a> From<&Literal<'a>> for Value {
    fn from(value: &Literal<'a>) -> Self {
        match value {
            Literal::Number(v) => Value::Number(*v),
            Literal::String(v) => Value::String(v.to_string()),
            Literal::True => Value::Boolean(true),
            Literal::False => Value::Boolean(false),
            Literal::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
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
        };
        Ok(())
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

#[derive(Clone, Default)]
struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn with_enclosing(enclosing: &Environment) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing.clone())),
        }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &str) -> Result<Value, String> {
        if self.values.contains_key(name) {
            return Ok(self.values.get(name).unwrap().clone());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.get(name);
        }

        Err(format!("Undefined variable '{}'.", name))
    }

    pub fn assign(&mut self, name: &str, val: Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val);
            return Ok(());
        }

        if let Some(enclosing) = &mut self.enclosing {
            return enclosing.assign(name, val);
        }

        Err(format!("Undefined variable '{}'.", name))
    }
}

#[derive(Default)]
pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn interpret<'a>(&mut self, stmts: BVec<'a, &'a Stmt<'a>>) -> Result<(), RuntimeError<'a>> {
        for stmt in stmts {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_number_operand<'b>(
        &self,
        token: &'b Token<'b>,
        operand: &Value,
    ) -> Result<(), RuntimeError<'b>> {
        if let Value::Number(_) = operand {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operand must be a number.".to_string(),
            })
        }
    }

    fn check_number_operands<'b>(
        &self,
        token: &'b Token<'b>,
        left: &Value,
        right: &Value,
    ) -> Result<(), RuntimeError<'b>> {
        if let (Value::Number(_), Value::Number(_)) = (left, right) {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn binary_op<'b, F>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
        op: F,
    ) -> Result<Value, RuntimeError<'b>>
    where
        F: Fn(f64, f64) -> f64,
    {
        self.check_number_operands(token, &a, &b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(Value::Number(op(a_val, b_val)))
        } else {
            // This should be unreachable due to the check above
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn comparison_op<'b, F>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
        op: F,
    ) -> Result<Value, RuntimeError<'b>>
    where
        F: Fn(f64, f64) -> bool,
    {
        self.check_number_operands(token, &a, &b)?;
        if let (Value::Number(a_val), Value::Number(b_val)) = (a, b) {
            Ok(Value::Boolean(op(a_val, b_val)))
        } else {
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn handle_plus<'b>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
    ) -> Result<Value, RuntimeError<'b>> {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(format!("{}{}", s1, s2))),
            _ => Err(RuntimeError {
                token,
                message: "Operands must be two numbers or two strings.".to_string(),
            }),
        }
    }

    fn execute_stmt<'a>(&mut self, stmt: &'a Stmt<'a>) -> Result<(), RuntimeError<'a>> {
        match stmt {
            Stmt::Class => todo!(),
            Stmt::Expression(expr) => {
                let _value = self.evaluate_expr(expr)?;
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
                self.env.define(name.lexeme, value);
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.evaluate_expr(condition)?.is_truthy() {
                    self.execute_stmt(then_stmt)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.execute_stmt(else_stmt)?;
                }
            }
            Stmt::Block(stmts) => {
                let block_env = Environment::with_enclosing(&self.env);
                self.execute_block(stmts, block_env)?;
            }
        }
        Ok(())
    }

    fn execute_block<'a>(
        &mut self,
        stmts: &'a [&Stmt<'a>],
        block_env: Environment,
    ) -> Result<Value, RuntimeError<'a>> {
        let previous = self.env.clone();

        self.env = block_env;

        let mut err = None;
        for stmt in stmts {
            if let Err(e) = self.execute_stmt(stmt) {
                err = Some(e);
                break;
            }
        }

        self.env = previous;
        if let Some(e) = err {
            return Err(e);
        }

        Ok(Value::Nil)
    }

    #[allow(unused)]
    fn evaluate_expr<'a>(&mut self, expr: &'a Expr<'a>) -> Result<Value, RuntimeError<'a>> {
        match expr {
            Expr::Assign(token, value) => {
                let val = self.evaluate_expr(value)?;
                match self.env.assign(token.lexeme, val.clone()) {
                    Ok(_) => Ok(val),
                    Err(msg) => Err(RuntimeError {
                        token,
                        message: msg,
                    }),
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
                    TokenType::BangEqual => Ok(Value::Boolean(!left_val.is_equal(&right_val))),
                    TokenType::EqualEqual => Ok(Value::Boolean(left_val.is_equal(&right_val))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported operator.".to_string(),
                    }),
                }
            }
            Expr::Call(expr, token, exprs) => todo!(),
            Expr::Get(expr, token) => todo!(),
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Literal(literal) => Ok(Value::from(literal)),
            Expr::Logical(expr, token, expr1) => todo!(),
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate_expr(right)?;
                match operator.typ {
                    TokenType::Minus => {
                        self.check_number_operand(operator, &right_val)?;
                        Ok(-right_val)
                    }
                    TokenType::Bang => Ok(Value::Boolean(!right_val.is_truthy())),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported unary operator.".to_string(),
                    }),
                }
            }
            Expr::Variable(token) => match self.env.get(token.lexeme) {
                Ok(v) => Ok(v),
                Err(msg) => Err(RuntimeError {
                    token,
                    message: msg,
                }),
            },
        }
    }
}

pub fn run_interpreter<'a>(
    source_code: &'a str,
    interpreter: &mut Interpreter,
    bump: &'a Bump,
) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source_code, bump);

    scanner.scan_tokens();

    for token in &scanner.tokens {
        if token.typ == TokenType::Error {
            println!("[line {}] Error: Unexpected character.", token.line);
            return Err(LoxError::CompileError);
        }
    }

    let parser = Parser::new(&scanner.tokens, bump);
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
            println!("{}", e);
            Err(LoxError::CompileError)
        }
    }
}

fn runtime_error(e: RuntimeError) {
    eprintln!("[line {}] {}", e.token.line, e.message);
}
