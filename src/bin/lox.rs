use std::{
    fs,
    io::{self, BufRead, Write},
    path::PathBuf,
    process,
};

use clap::Parser;
use lox::{run_interpreter, Cli, Interpreter};

fn main() {
    let cli = Cli::parse();
    if let Some(file) = cli.file {
        run_file(file);
    } else {
        run_prompt();
    }
}

fn run_file(file_path: PathBuf) {
    let source = fs::read_to_string(file_path).expect("Failed to read source file");
    let mut interp = Interpreter::default();
    if let Err(error) = run_interpreter(&source, &mut interp) {
        match error {
            lox::LoxError::CompileError => process::exit(65),
            lox::LoxError::RuntimeError => process::exit(70),
        }
    }
}

fn run_prompt() {
    let mut input = io::stdin().lock();
    let mut output = io::stdout();
    let mut interpreter = Interpreter::default();

    loop {
        output.write_all(b"> ").unwrap();
        output.flush().unwrap();

        let mut buffer = String::new();
        input
            .read_line(&mut buffer)
            .expect("Read line from stdio failed");

        if buffer.is_empty() {
            continue;
        }
        run_interpreter(&buffer, &mut interpreter).ok();
    }
}
