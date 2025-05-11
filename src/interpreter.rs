use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self};
use std::ops::Neg;
use std::rc::Rc;

use anyhow::Result;
use bumpalo::{collections::Vec as BVec, Bump};

use crate::expr::{Expr, Stmt};
use crate::token::{Token, TokenType};
use crate::{Literal, LoxError, Parser, Scanner};

type InterpretResult<'a, T> = Result<T, InterpretError<'a>>;

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

#[derive(Default)]
struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
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
            return enclosing.borrow().get(name);
        }

        Err(format!("Undefined variable '{}'.", name))
    }

    pub fn assign(&mut self, name: &str, val: Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), val);
            return Ok(());
        }

        if let Some(enclosing) = &self.enclosing {
            return enclosing.borrow_mut().assign(name, val);
        }

        Err(format!("Undefined variable '{}'.", name))
    }
}

#[derive(Default)]
pub struct Interpreter {
    env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn interpret<'a>(&mut self, stmts: BVec<'a, &'a Stmt<'a>>) -> Result<(), RuntimeError<'a>> {
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

    fn check_number_operand<'b>(
        &self,
        token: &'b Token<'b>,
        operand: &Value,
    ) -> InterpretResult<'b, ()> {
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

    fn check_number_operands<'b>(
        &self,
        token: &'b Token<'b>,
        left: &Value,
        right: &Value,
    ) -> InterpretResult<'b, ()> {
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

    fn binary_op<'b, F>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
        op: F,
    ) -> InterpretResult<'b, Value>
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
            }
            .into())
        }
    }

    fn comparison_op<'b, F>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
        op: F,
    ) -> InterpretResult<'b, Value>
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
            }
            .into())
        }
    }

    fn handle_plus<'b>(
        &self,
        token: &'b Token<'b>,
        a: Value,
        b: Value,
    ) -> InterpretResult<'b, Value> {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
            (Value::String(s1), Value::String(s2)) => Ok(Value::String(format!("{}{}", s1, s2))),
            _ => Err(RuntimeError {
                token,
                message: "Operands must be two numbers or two strings.".to_string(),
            }
            .into()),
        }
    }

    fn execute_stmt<'a>(&mut self, stmt: &'a Stmt<'a>) -> InterpretResult<'a, ()> {
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
            Stmt::Var(token, initializer) => {
                let value = match initializer {
                    Some(v) => self.evaluate_expr(v)?,
                    None => Value::Nil,
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
        }
        Ok(())
    }

    fn execute_block<'a>(
        &mut self,
        stmts: &'a [&Stmt<'a>],
        block_env: Environment,
    ) -> InterpretResult<'a, Value> {
        let previous = Rc::clone(&self.env);

        self.env = Rc::new(RefCell::new(block_env));

        let mut result = Ok(Value::Nil);
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
    fn evaluate_expr<'a>(&mut self, expr: &'a Expr<'a>) -> InterpretResult<'a, Value> {
        match expr {
            Expr::Assign(token, value) => {
                let val = self.evaluate_expr(value)?;
                match self.env.borrow_mut().assign(token.lexeme, val.clone()) {
                    Ok(_) => Ok(val),
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
                    TokenType::BangEqual => Ok(Value::Boolean(!left_val.is_equal(&right_val))),
                    TokenType::EqualEqual => Ok(Value::Boolean(left_val.is_equal(&right_val))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported operator.".to_string(),
                    }
                    .into()),
                }
            }
            Expr::Call(expr, token, exprs) => todo!(),
            Expr::Get(expr, token) => todo!(),
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Literal(literal) => Ok(Value::from(literal)),
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
                        self.check_number_operand(operator, &right_val)?;
                        Ok(-right_val)
                    }
                    TokenType::Bang => Ok(Value::Boolean(!right_val.is_truthy())),
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
