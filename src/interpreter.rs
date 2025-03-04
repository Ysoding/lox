use std::ops::Neg;

use crate::expr::Expr;
use crate::token::TokenType;

pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
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

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Assign(token, expr) => todo!(),
            Expr::Binary(expr, token, expr1) => todo!(),
            Expr::Call(expr, token, exprs) => todo!(),
            Expr::Get(expr, token) => todo!(),
            Expr::Grouping(expr) => self.evalute(&expr),
            Expr::Literal(literal) => todo!(),
            Expr::Logical(expr, token, expr1) => todo!(),
            Expr::Set(expr, token, expr1) => todo!(),
            Expr::Super(token, token1) => todo!(),
            Expr::This(token) => todo!(),
            Expr::Unary(operator, right) => {
                let right = self.evalute(&right);
                match operator.typ {
                    TokenType::Minus => -right,
                    TokenType::Bang => Value::Boolean(!self.is_truth_value(&right)),
                    _ => Value::Nil,
                }
            }
            Expr::Variable(token) => todo!(),
        }
    }

    fn is_truth_value(&self, val: &Value) -> bool {
        match val {
            // Value::Number(v) => *v != 0.0,
            // Value::String(v) => !v.is_empty(),
            Value::Boolean(v) => *v,
            Value::Nil => false,
            _ => true,
        }
    }

    fn evalute(&mut self, expr: &Expr) -> Value {
        todo!()
    }
}
