use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{Chunk, Value};

pub type NativeFunction = fn(Vec<Value>) -> Value;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionType {
    Function,
    Script,
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionUpvalue {
    pub index: u8,
    pub is_local: bool,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
    pub upvalues: Vec<FunctionUpvalue>,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: name.to_string(),
            upvalues: Vec::new(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Upvalue>,
}

impl Closure {
    pub fn new(function: Function) -> Self {
        Self {
            function,
            upvalues: vec![],
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.function)
    }
}

#[derive(Clone, Debug)]
pub struct Upvalue {
    pub location: usize,
    pub closed: Option<Value>,
    pub next: Option<Rc<RefCell<Upvalue>>>,
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Upvalue {
            location,
            closed: None,
            next: None,
        }
    }
}
