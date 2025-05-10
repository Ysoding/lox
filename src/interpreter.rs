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

#[derive(Debug, Clone)]
pub struct RuntimeError<'a> {
    pub token: &'a Token<'a>,
    pub message: String,
}

#[derive(Clone)]
pub enum Value<'a> {
    Number(f64),
    String(&'a str),
    Boolean(bool),
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
            Literal::String(v) => Value::String(*v),
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
                    f.write_str(&text[..text.len() - 2]);
                } else {
                    f.write_str(&text);
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

impl<'a> Neg for &'a Value<'a> {
    type Output = Value<'a>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Value::Number(-n),
            // FIXME: ERROR
            _ => Value::Nil,
        }
    }
}

#[derive(Default)]
struct Environment<'a> {
    values: HashMap<&'a str, &'a Value<'a>>,
    enclosing: Option<Rc<RefCell<Environment<'a>>>>,
}

impl<'a> Environment<'a> {
    pub fn with_enclosing(enclosing: Rc<RefCell<Environment<'a>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &'a str, value: &'a Value<'a>) {
        self.values.insert(name, value);
    }

    pub fn get(&'a self, name: &'a Token<'a>) -> Result<&'a Value<'a>, RuntimeError<'a>> {
        if self.values.contains_key(&name.lexeme) {
            return Ok(self.values.get(&name.lexeme).unwrap().clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(RuntimeError {
            token: name,
            message: format!("Undefined variable '{}'.", name.lexeme),
        })
    }

    pub fn assign(
        &mut self,
        name: &'a Token<'a>,
        val: &'a Value<'a>,
    ) -> Result<(), RuntimeError<'a>> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, val);
            return Ok(());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, val);
        }

        Err(RuntimeError {
            token: name,
            message: format!("Undefined variable '{}'.", name.lexeme),
        })
    }
}

pub struct Interpreter<'a> {
    env: Rc<RefCell<Environment<'a>>>,
    bump: &'a Bump,
}

impl<'a> Interpreter<'a> {
    pub fn new(bump: &'a Bump) -> Self {
        Self {
            env: Rc::new(RefCell::new(Environment::default())),
            bump,
        }
    }

    pub fn interpret(&mut self, stmts: BVec<'a, &'a Stmt<'a>>) -> Result<(), RuntimeError<'a>> {
        for stmt in stmts {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_number_operand(
        &self,
        token: &'a Token<'a>,
        operand: &'a Value<'a>,
    ) -> Result<(), RuntimeError> {
        if let Value::Number(_) = operand {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operand must be a number.".to_string(),
            })
        }
    }

    fn check_number_operands(
        &self,
        token: &'a Token<'a>,
        left: &'a Value<'a>,
        right: &'a Value<'a>,
    ) -> Result<(), RuntimeError> {
        if let (Value::Number(_), Value::Number(_)) = (left, right) {
            Ok(())
        } else {
            Err(RuntimeError {
                token,
                message: "Operands must be numbers.".to_string(),
            })
        }
    }

    fn binary_op<F>(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
        op: F,
    ) -> Result<&'a Value<'a>, RuntimeError>
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
            })
        }
    }

    fn comparison_op<F>(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
        op: F,
    ) -> Result<&'a Value<'a>, RuntimeError>
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
            })
        }
    }

    fn handle_plus(
        &self,
        token: &'a Token<'a>,
        a: &'a Value<'a>,
        b: &'a Value<'a>,
    ) -> Result<&'a Value<'a>, RuntimeError> {
        match (a, b) {
            (Value::Number(n1), Value::Number(n2)) => Ok(self.bump.alloc(Value::Number(n1 + n2))),
            (Value::String(s1), Value::String(s2)) => Ok(self
                .bump
                .alloc(Value::String(self.bump.alloc_str(&format!("{}{}", s1, s2))))),
            _ => Err(RuntimeError {
                token,
                message: "Operands must be two numbers or two strings.".to_string(),
            }),
        }
    }

    fn execute_stmt(&mut self, stmt: &'a Stmt<'a>) -> Result<(), RuntimeError<'a>> {
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
                    None => self.bump.alloc(Value::Nil),
                };
                self.env.borrow_mut().define(&name.lexeme, value);
            }
            Stmt::If(condition, then_stmt, else_stmt) => {
                if self.evaluate_expr(condition)?.is_truthy() {
                    self.execute_stmt(then_stmt)?;
                } else if let Some(else_stmt) = else_stmt {
                    self.execute_stmt(else_stmt)?;
                }
            }
            Stmt::Block(stmts) => {
                let block_env = Rc::new(RefCell::new(Environment::with_enclosing(Rc::clone(
                    &self.env,
                ))));
                self.execute_block(stmts, block_env)?;
            }
        }
        Ok(())
    }

    fn execute_block(
        &mut self,
        stmts: &'a [&Stmt<'a>],
        block_env: Rc<RefCell<Environment<'a>>>,
    ) -> Result<Value<'a>, RuntimeError<'a>> {
        let previous = Rc::clone(&self.env);

        self.env = Rc::clone(&block_env);

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
    fn evaluate_expr(&mut self, expr: &'a Expr<'a>) -> Result<&'a Value<'a>, RuntimeError<'a>> {
        match expr {
            Expr::Assign(name, value) => {
                let val = self.evaluate_expr(value)?;
                self.env.borrow_mut().assign(name, val)?;
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
                    TokenType::BangEqual => Ok(self
                        .bump
                        .alloc(Value::Boolean(!left_val.is_equal(&right_val)))),
                    TokenType::EqualEqual => Ok(self
                        .bump
                        .alloc(Value::Boolean(left_val.is_equal(&right_val)))),
                    _ => Err(RuntimeError {
                        token: operator.clone(),
                        message: "Unsupported operator.".to_string(),
                    }),
                }
            }
            Expr::Call(expr, token, exprs) => todo!(),
            Expr::Get(expr, token) => todo!(),
            Expr::Grouping(expr) => self.evaluate_expr(expr),
            Expr::Literal(literal) => Ok(self.bump.alloc(Value::from(literal))),
            Expr::Logical(expr, token, expr1) => todo!(),
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate_expr(right)?;
                match operator.typ {
                    TokenType::Minus => {
                        self.check_number_operand(operator, &right_val)?;
                        Ok(self.bump.alloc(-right_val))
                    }
                    TokenType::Bang => Ok(self.bump.alloc(Value::Boolean(!right_val.is_truthy()))),
                    _ => Err(RuntimeError {
                        token: operator,
                        message: "Unsupported unary operator.".to_string(),
                    }),
                }
            }
            Expr::Variable(name) => self.env.borrow().get(name),
        }
    }
}

pub fn run_interpreter<'a>(
    source_code: &'a str,
    interpreter: &'a mut Interpreter<'a>,
    bump: &'a Bump,
) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source_code, &bump);

    scanner.scan_tokens();

    for token in &scanner.tokens {
        if token.typ == TokenType::Error {
            println!("[line {}] Error: Unexpected character.", token.line);
            return Err(LoxError::CompileError);
        }
    }

    let parser = Parser::new(&scanner.tokens, &bump);
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
