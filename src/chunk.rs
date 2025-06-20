use crate::Value;

// macro_rules! binary_op {
//     ($self:expr, &op:tt) => {
//         let a = $self.pop_stack
//     };
// }

// The following is just to keep the debug info consistent with the book.
// A more standard approach would be to store the `code` directly as `u8,``
pub enum OpCode {
    Return,            // 1byte
    Constant(u8),      // dummy 2bytes (opcode + u8 operand)  - actually 1byte
    ConstantLong(u32), // dummy 4bytes (opcode + 3-byte operand) - actually 4bytes
    Negate,
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

    pub fn write_constant(&mut self, value: Value, line: u32) {
        let constant_index = self.add_constant(value);
        if constant_index < 256 {
            // Use OP_CONSTANT for indices 0-255
            self.write_chunk(OpCode::Constant(constant_index as u8), line);
        } else if constant_index < (1 << 24) {
            // Use OP_CONSTANT_LONG for indices 256-16,777,215
            self.write_chunk(OpCode::ConstantLong(constant_index as u32), line);
        } else {
            panic!("Too many constants in one chunk");
        }
    }

    fn get_line(&self, instruction_index: usize) -> u32 {
        let mut current_index = 0;
        for &(line, count) in &self.line_runs {
            current_index += count as usize;
            if instruction_index < current_index {
                return line;
            }
        }
        panic!("Instruction index {} out of bounds", instruction_index);
    }

    fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    #[cfg(feature = "debug_trace_execution")]
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        let mut dummy_offset = 0;
        while offset < self.code.len() {
            dummy_offset = self.disassemble_instruction(offset, dummy_offset);
            offset += 1;
        }
    }

    #[cfg(feature = "debug_trace_execution")]
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
        }
    }

    #[cfg(feature = "debug_trace_execution")]
    fn simple_instruction(&self, name: &str) {
        println!("{}", name);
    }

    #[cfg(feature = "debug_trace_execution")]
    fn constant_long_instruction(&self, name: &str, constant: u32) {
        println!(
            "{:-16} {:4} '{}'",
            name, constant, self.constants[constant as usize]
        );
    }

    #[cfg(feature = "debug_trace_execution")]
    fn constant_instruction(&self, name: &str, constant: u8) {
        println!(
            "{:-16} {:4} '{}'",
            name, constant, self.constants[constant as usize]
        );
    }
}
