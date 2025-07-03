use std::{
    env, fs,
    io::{self, BufRead, Write},
    path::PathBuf,
    process,
};

use clox::{LoxError, VirtualMachine};

fn main() {
    let args: Vec<String> = env::args().collect();

    // run_file("./test.lox".into());

    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(args.get(1).unwrap().into());
    } else {
        eprintln!("Usage: clox [path]");
        process::exit(64);
    }
}

fn run_file(path: PathBuf) {
    let source = fs::read_to_string(path).expect("Failed to read source file");
    // let source: &'static str = Box::leak(source.into_boxed_str());
    let mut vm = VirtualMachine::new();

    match vm.interpret(&source) {
        Ok(_) => {}
        Err(err) => match err {
            LoxError::CompileError => process::exit(65),
            LoxError::RuntimeError => process::exit(70),
        },
    }
}

fn repl() {
    let mut vm = VirtualMachine::new();
    let mut input = io::stdin().lock();
    let mut output = io::stdout();

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
        vm.interpret(&buffer).ok();
        vm.reset();
    }
}
