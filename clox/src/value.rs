use std::fmt::Display;

use anyhow::Result;

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    Nil,
    String(String),
    Bool(bool),
}

impl Value {
    pub fn as_number(self) -> Result<f64, String> {
        match self {
            Value::Number(v) => Ok(v),
            v => Err(format!("cannot convert to Number: {:?}", v)),
        }
    }

    pub fn as_string(self) -> String {
        match self {
            Value::String(s) => s,
            _ => "".to_string(),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            Value::Number(v) => *v != 0.0,
            Value::Bool(v) => *v,
            _ => false,
        }
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_falsy(&self) -> bool {
        self.is_nil() || (self.is_boolean() && !self.as_boolean())
    }

    pub fn equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Bool(v1), Value::Bool(v2)) => *v1 == *v2,
            (Value::Number(v1), Value::Number(v2)) => *v1 == *v2,
            (Value::Nil, Value::Nil) => true,
            (Value::String(v1), Value::String(v2)) => *v1 == *v2,
            _ => false,
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::Number(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
        }
    }
}
