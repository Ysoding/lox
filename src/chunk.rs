pub enum OpCode {
    Return,
    Unknown,
}

#[derive(Default)]
pub struct Chunk {
    code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn write_opcode(&mut self, op_code: OpCode) {
        self.code.push(op_code);
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
            OpCode::Return => self.simple_instruction("OP_RETURN", offset),
            OpCode::Unknown => offset + 1,
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        return offset + 1;
    }
}
