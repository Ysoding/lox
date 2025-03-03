use std::ops::Neg;

use crate::expr::Expr;
use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

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

    pub fn stringify(&self) -> String {
        match self {
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
        }
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
pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, expr: Expr) -> Result<Value, RuntimeError> {
        let value = self.evaluate(&expr)?;
        Ok(value)
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

    fn evaluate(&self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Assign(token, expr) => todo!(),
            Expr::Binary(left, operator, right) => {
                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;
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
            Expr::Grouping(expr) => self.evaluate(&expr),
            Expr::Literal(literal) => todo!(),
            Expr::Logical(expr, token, expr1) => todo!(),
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate(&right)?;
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
            Expr::Variable(token) => todo!(),
        }
    }
}
