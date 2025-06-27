use anyhow::Result;

use crate::{Chunk, LoxError, OpCode, Value, compile};

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        let a = $self.pop().as_number().map_err(|_| $self.runtime_error("Operands must be numbers."))?;
        $self.push((a $op b).into());
    }};
}

#[derive(Default)]
pub struct VirtualMachine {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
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
                    println!("{}", self.pop());
                    return Ok(());
                }
                OpCode::Constant(c_idx) => {
                    let constant = self.read_constant(*c_idx as usize);
                    self.stack.push(constant.clone());
                }
                OpCode::ConstantLong(c_idx) => {
                    let constant = self.read_constant(*c_idx as usize);
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
                    binary_op!(self, +);
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
            }
            self.ip += 1;
        }
    }

    fn read_instruction(&self, instruction: usize) -> &OpCode {
        self.chunk.code.get(instruction).unwrap()
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
}
