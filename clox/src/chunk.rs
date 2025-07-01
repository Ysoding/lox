use crate::Value;

// macro_rules! binary_op {
//     ($self:expr, &op:tt) => {
//         let a = $self.pop_stack
//     };
// }

// The following is just to keep the debug info consistent with the book.
// A more standard approach would be to store the `code` directly as `u8,``
#[derive(Clone, Copy)]
pub enum OpCode {
    Return,            // 1byte
    Constant(u8),      // dummy 2bytes (opcode + u8 operand)  - actually 1byte
    ConstantLong(u32), // dummy 4bytes (opcode + 3-byte operand) - actually 4bytes
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Not,
    Negate,
    Print,
    Pop,
    DefineGlobal(u8),
    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    Add,
    Subtract,
    Multiply,
    Divide,
    Unknown,
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<Value>,
    line_runs: Vec<(u32, u32)>, // (line, count)
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write_chunk(&mut self, op_code: OpCode, line: u32) {
        self.code.push(op_code);
        if let Some((last_line, count)) = self.line_runs.last_mut() {
            if *last_line == line {
                *count += 1;
            } else {
                self.line_runs.push((line, 1));
            }
        } else {
            self.line_runs.push((line, 1));
        }
    }

    pub fn get_line(&self, instruction_index: usize) -> u32 {
        let mut current_index = 0;
        for &(line, count) in &self.line_runs {
            current_index += count as usize;
            if instruction_index < current_index {
                return line;
            }
        }
        panic!("Instruction index {} out of bounds", instruction_index);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}

#[cfg(feature = "debug_trace_execution")]
impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        let mut dummy_offset = 0;
        while offset < self.code.len() {
            dummy_offset = self.disassemble_instruction(offset, dummy_offset);
            offset += 1;
        }
    }
    pub fn disassemble_instruction(&self, offset: usize, dummy_offset: usize) -> usize {
        print!("{:04} ", dummy_offset);

        if offset > 0 && self.get_line(offset) == self.get_line(offset - 1) {
            print!("   | ");
        } else {
            print!("{:04} ", self.get_line(offset))
        }

        let op_code = self.code.get(offset).unwrap();
        match op_code {
            OpCode::Return => {
                self.simple_instruction("OP_RETURN");
                dummy_offset + 1
            }
            OpCode::Negate => {
                self.simple_instruction("OP_NEGATE");
                dummy_offset + 1
            }
            OpCode::Constant(c) => {
                self.constant_instruction("OP_CONSTANT", *c);
                dummy_offset + 2
            }
            OpCode::ConstantLong(c) => {
                self.constant_long_instruction("OP_CONSTANT_LONG", *c);
                dummy_offset + 4
            }
            OpCode::Unknown => dummy_offset + 1,
            OpCode::Add => {
                self.simple_instruction("OP_ADD");
                dummy_offset + 1
            }
            OpCode::Subtract => {
                self.simple_instruction("OP_SUBTRACT");
                dummy_offset + 1
            }
            OpCode::Multiply => {
                self.simple_instruction("OP_MULTIPLY");
                dummy_offset + 1
            }
            OpCode::Divide => {
                self.simple_instruction("OP_DIVIDE");
                dummy_offset + 1
            }
            OpCode::Nil => {
                self.simple_instruction("OP_NIL");
                dummy_offset + 1
            }
            OpCode::True => {
                self.simple_instruction("OP_TRUE");
                dummy_offset + 1
            }
            OpCode::False => {
                self.simple_instruction("OP_FALSE");
                dummy_offset + 1
            }
            OpCode::Not => {
                self.simple_instruction("OP_NOT");
                dummy_offset + 1
            }
            OpCode::Equal => {
                self.simple_instruction("OP_EQUAL");
                dummy_offset + 1
            }
            OpCode::Greater => {
                self.simple_instruction("OP_GREATER");
                dummy_offset + 1
            }
            OpCode::Less => {
                self.simple_instruction("OP_LESS");
                dummy_offset + 1
            }
            OpCode::Print => {
                self.simple_instruction("OP_PRINT");
                dummy_offset + 1
            }
            OpCode::Pop => {
                self.simple_instruction("OP_POP");
                dummy_offset + 1
            }
            OpCode::DefineGlobal(constant) => {
                self.constant_instruction("OP_DEFINE_GLOBAL", *constant);
                dummy_offset + 2
            }
            OpCode::GetGlobal(constant) => {
                self.constant_instruction("OP_GET_GLOBAL", *constant);
                dummy_offset + 2
            }
            OpCode::SetGlobal(constant) => {
                self.constant_instruction("OP_SET_GLOBAL", *constant);
                dummy_offset + 2
            }
            OpCode::GetLocal(slot) => {
                self.byte_instruction("OP_GET_LOCAL", *slot);

                dummy_offset + 2
            }
            OpCode::SetLocal(slot) => {
                self.byte_instruction("OP_SET_LOCAL", *slot);
                dummy_offset + 2
            }
        }
    }
    fn simple_instruction(&self, name: &str) {
        println!("{}", name);
    }

    fn constant_long_instruction(&self, name: &str, constant: u32) {
        println!(
            "{:-16} {:4} '{}'",
            name, constant, self.constants[constant as usize]
        );
    }

    fn constant_instruction(&self, name: &str, constant: u8) {
        println!(
            "{:-16} {:4} '{}'",
            name, constant, self.constants[constant as usize]
        );
    }

    fn byte_instruction(&self, name: &str, slot: u8) {
        println!("{:-16} {:4}", name, slot);
    }
}
