use std::env;

use lox::{Chunk, OpCode};

fn main() {
    let _args: Vec<String> = env::args().collect();
    let mut chunk = Chunk::new();

    chunk.write_opcode(OpCode::Return);

    chunk.disassemble("test chunk");
}
