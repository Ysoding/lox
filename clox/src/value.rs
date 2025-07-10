use anyhow::Result;

use crate::{Class, Closure, Function, GcRef, GcTrace, Instance, NativeFunction};

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    NativeFunction(NativeFunction),
    String(GcRef<String>),
    Function(GcRef<Function>),
    Closure(GcRef<Closure>),
    Class(GcRef<Class>),
    Instance(GcRef<Instance>),
}

impl Value {
    pub fn as_number(self) -> Result<f64, String> {
        match self {
            Value::Number(v) => Ok(v),
            v => Err(format!("cannot convert to Number: {:?}", v)),
        }
    }

    pub fn as_string(self) -> Result<GcRef<String>, String> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err("cannot convert to String".into()),
        }
    }

    pub fn as_instance(self) -> Result<GcRef<Instance>, String> {
        match self {
            Value::Instance(v) => Ok(v),
            _ => Err("cannot convert to Instance".into()),
        }
    }

    pub fn as_native_function(self) -> Result<NativeFunction, String> {
        match self {
            Value::NativeFunction(f) => Ok(f),
            _ => Err("cannot convert to NativeFunction".into()),
        }
    }

    pub fn as_closure(self) -> Result<GcRef<Closure>, String> {
        match self {
            Value::Closure(c) => Ok(c),
            _ => Err("cannot convert to Closure".into()),
        }
    }

    pub fn as_function(self) -> Result<GcRef<Function>, String> {
        match self {
            Value::Function(f) => Ok(f),
            _ => Err("cannot convert to Function".into()),
        }
    }

    pub fn as_class(self) -> Result<GcRef<Class>, String> {
        match self {
            Value::Class(c) => Ok(c),
            _ => Err("cannot convert to Class".into()),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            Value::Number(v) => *v != 0.0,
            Value::Bool(v) => *v,
            _ => false,
        }
    }

    pub fn is_instance(&self) -> bool {
        matches!(self, Value::Instance(_))
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, Value::Closure(_))
    }

    pub fn is_boolean(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn is_native_function(&self) -> bool {
        matches!(self, Value::NativeFunction(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    pub fn is_class(&self) -> bool {
        matches!(self, Value::Class(_))
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

impl GcTrace for Value {
    fn format(&self, f: &mut std::fmt::Formatter, gc: &crate::Gc) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Number(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => gc.deref(*v).format(f, gc),
            Value::Function(v) => gc.deref(*v).format(f, gc),
            Value::Closure(v) => gc.deref(*v).format(f, gc),
            Value::Class(v) => gc.deref(*v).format(f, gc),
            Value::Instance(v) => gc.deref(*v).format(f, gc),
        }
    }

    fn size(&self) -> usize {
        0
    }

    fn trace(&self, gc: &mut crate::Gc) {
        match self {
            Value::String(v) => gc.mark_object(*v),
            Value::Function(v) => gc.mark_object(*v),
            Value::Closure(v) => gc.mark_object(*v),
            Value::Class(v) => gc.mark_object(*v),
            _ => {}
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        panic!("Value should not be allocated")
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        panic!("Value should not be allocated")
    }
}
