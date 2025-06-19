use crate::Value;

pub enum OpCode {
    Return,
    Constant(u8),
    Unknown,
}

#[derive(Default)]
pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write_chunk(&mut self, op_code: OpCode) {
        self.code.push(op_code);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instruction(offset);
        }
    }

    fn disassemble_instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        let op_code = self.code.get(offset).unwrap();
        match op_code {
            OpCode::Return => self.simple_instruction("OP_RETURN"),
            OpCode::Constant(c) => self.constant_instruction("OP_CONSTANT", *c),
            OpCode::Unknown => {}
        }
        offset + 1
    }

    fn simple_instruction(&self, name: &str) {
        println!("{}", name);
    }

    fn constant_instruction(&self, name: &str, constant: u8) {
        println!(
            "{:-16} {:4} '{}'",
            name, constant, self.constants[constant as usize]
        );
    }
}
