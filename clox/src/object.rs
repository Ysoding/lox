use crate::{Chunk, Value};

pub type NativeFunction = fn(Vec<Value>) -> Value;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FunctionType {
    Function,
    Script,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: name.to_string(),
        }
    }
}
