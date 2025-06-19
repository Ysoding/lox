use std::env;

use lox::{Chunk, OpCode};

fn main() {
    let _args: Vec<String> = env::args().collect();
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(lox::Value::Number(1.2));
    chunk.write_chunk(OpCode::Constant(constant as u8));

    chunk.write_chunk(OpCode::Return);
    chunk.disassemble("test chunk");
}
