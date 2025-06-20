use std::{env, process};

use lox::{Chunk, LoxError, OpCode, Value, VirtualMachine};

fn main() {
    let _args: Vec<String> = env::args().collect();
    let mut vm = VirtualMachine::new();

    let mut chunk = Chunk::new();

    chunk.write_constant(Value::Number(1.2), 123);
    chunk.write_constant(Value::Number(3.4), 123);
    chunk.write_chunk(OpCode::Add, 123);

    chunk.write_constant(Value::Number(5.6), 123);
    chunk.write_chunk(OpCode::Divide, 123);
    chunk.write_chunk(OpCode::Negate, 123);

    chunk.write_chunk(OpCode::Return, 123);

    match vm.interpret(chunk) {
        Ok(_) => {}
        Err(err) => match err {
            LoxError::CompileError => process::exit(65),
            LoxError::RuntimeError => process::exit(70),
        },
    }
}
