use std::{
    fs,
    io::{self, BufRead, Write},
    path::PathBuf,
};

use clap::Parser;

mod cli;
use cli::*;
mod scanner;
use scanner::*;
mod token;
use token::*;
mod parser;
use parser::*;
mod expr;
use expr::*;

fn main() {
    let cli = Cli::parse();
    if let Some(file) = cli.file {
        run_file(file);
    } else {
        run_prompt();
    }
}

fn run_file(file_path: PathBuf) {
    let source = fs::read_to_string(file_path).expect("failed to read the file");
    run(source);
}

fn run_prompt() {
    let username = users::get_current_username().expect("Failed to get current username");

    println!(
        "Hello {}! This is the Lox programming language!",
        username.to_string_lossy()
    );
    println!("Feel free to type in commands");

    let mut input = io::stdin().lock();
    let mut output = io::stdout();

    let mut buffer = String::new();
    loop {
        output.write_all(b">> ").unwrap();
        output.flush().unwrap();

        input.read_line(&mut buffer).unwrap();
        run(buffer.clone());

        buffer.clear();
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(&source);
    scanner.scan_tokens();

    for token in scanner.tokens {
        if token.typ == TokenType::Error {
            error(token.line, token.lexeme);
            return;
        }
        println!("{}", token);
    }
}

fn error(line: usize, msg: &str) {
    report(line, "", msg);
}

fn report(line: usize, wh: &str, msg: &str) {
    eprintln!("[line {} ] Error{}: {}", line, wh, msg);
}
