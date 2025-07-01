use std::collections::HashMap;

use anyhow::Result;

use crate::{Chunk, LoxError, OpCode, Value, compile};

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        let a = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        $self.push((a $op b).into());
    }};
}

type Table = HashMap<String, Value>;

#[derive(Default)]
pub struct VirtualMachine {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: Table,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn reset(&mut self) {
        self.ip = 0;
        self.reset_stack();
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), LoxError> {
        let mut chunk = Chunk::new();
        compile(source, &mut chunk)?;
        self.chunk = chunk;
        self.run()
    }

    fn run(&mut self) -> Result<(), LoxError> {
        #[cfg(feature = "debug_trace_execution")]
        let mut dummy_offset = 0;

        loop {
            #[cfg(feature = "debug_trace_execution")]
            {
                print!("          ");
                for ele in &self.stack {
                    print!("[ {} ]", *ele);
                }
                println!();
                dummy_offset = self.chunk.disassemble_instruction(self.ip, dummy_offset);
            }

            match self.read_instruction(self.ip) {
                OpCode::Return => {
                    return Ok(());
                }
                OpCode::Constant(c_idx) => {
                    let constant = self.read_constant(c_idx as usize);
                    self.stack.push(constant.clone());
                }
                OpCode::ConstantLong(c_idx) => {
                    let constant = self.read_constant(c_idx as usize);
                    println!("{}", constant);
                }
                OpCode::Unknown => {
                    panic!("Unknown instruction")
                }
                OpCode::Negate => {
                    let v = self
                        .pop()
                        .as_number()
                        .map_err(|_| self.runtime_error("Operand must be a number."))?;

                    self.push((-v).into());
                }
                OpCode::Add => {
                    let v1 = self.peek(0);
                    let v2 = self.peek(1);
                    if v1.is_string() && v2.is_string() {
                        self.concatenate();
                    } else if v1.is_number() && v2.is_number() {
                        binary_op!(self, +);
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return Err(LoxError::RuntimeError);
                    }
                }
                OpCode::Subtract => {
                    binary_op!(self, -);
                }
                OpCode::Multiply => {
                    binary_op!(self, *);
                }
                OpCode::Divide => {
                    binary_op!(self, /);
                }
                OpCode::Nil => {
                    self.push(Value::Nil);
                }
                OpCode::True => {
                    self.push(Value::Bool(true));
                }
                OpCode::False => {
                    self.push(Value::Bool(false));
                }
                OpCode::Not => {
                    let v = self.pop().is_falsy();
                    self.push(v.into());
                }
                OpCode::Equal => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push((a.equal(&b)).into());
                }
                OpCode::Greater => {
                    binary_op!(self, >);
                }
                OpCode::Less => {
                    binary_op!(self, <);
                }
                OpCode::Print => {
                    println!("{}", self.pop());
                }
                OpCode::Pop => {
                    self.pop();
                }
                OpCode::DefineGlobal(pos) => {
                    let name = self.read_constant(pos as usize).clone().as_string();
                    self.globals.insert(name, self.peek(0).clone());
                    self.pop();
                }
                OpCode::GetGlobal(pos) => {
                    let name = self.read_constant(pos as usize).clone().as_string();
                    if let Some(v) = self.globals.get(&name) {
                        self.push(v.clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'.", name));
                        return Err(LoxError::RuntimeError);
                    }
                }
                OpCode::SetGlobal(pos) => {
                    let name = self.read_constant(pos as usize).clone().as_string();
                    if !self.globals.contains_key(&name) {
                        self.runtime_error(&format!("Undefined variable '{}'.", name));
                        return Err(LoxError::RuntimeError);
                    }

                    self.globals.insert(name, self.peek(0).clone());
                }
                OpCode::GetLocal(slot) => {
                    self.push(self.stack.get(slot as usize).unwrap().clone());
                }
                OpCode::SetLocal(slot) => {
                    self.stack[slot as usize] = self.peek(0).clone();
                }
            }
            self.ip += 1;
        }
    }

    fn read_instruction(&self, instruction: usize) -> OpCode {
        *self.chunk.code.get(instruction).unwrap()
    }

    fn read_constant(&self, constant_idx: usize) -> &Value {
        self.chunk.constants.get(constant_idx).unwrap()
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack.len() - 1 - distance]
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn runtime_error(&mut self, msg: &str) -> LoxError {
        println!("{}", msg);

        let line = self.chunk.get_line(self.ip);
        println!("[line {}] in script", line);
        self.reset_stack();

        LoxError::RuntimeError
    }

    fn concatenate(&mut self) {
        let b = self.pop().as_string();
        let a = self.pop().as_string();

        self.push(Value::String(format!("{}{}", a, b)));
    }
}
