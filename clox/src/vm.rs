use anyhow::Result;

use crate::{Chunk, LoxError, OpCode, Value, compile};

macro_rules! binary_op {
    ($self:ident, $op:tt) => {{
        let b = $self.pop();
        let a = $self.pop();
        $self.push(a $op b);
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

    pub fn clear(&mut self) {
        self.ip = 0;
        self.stack.clear();
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
                    let v = -self.stack.pop().unwrap();
                    self.push(v);
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
}
