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
mod expr;
mod parser;
use expr::*;

type RunnerResult = Result<String, String>;

fn main() {
    let cli = Cli::parse();
    if let Some(file) = cli.file {
        if let Err(e) = run_file(file) {
            println!("{}", e);
        }
    } else {
        if let Err(e) = run_prompt() {
            println!("{}", e);
        }
    }
}

fn run_file(file_path: PathBuf) -> RunnerResult {
    let source =
        fs::read_to_string(file_path).map_err(|e| format!("Failed to read file: {}", e))?;
    run(source)
}

fn run_prompt() -> RunnerResult {
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
        if let Err(e) = run(buffer.clone()) {
            println!("{}", e);
        }

        buffer.clear();
    }
}

fn run(source: String) -> RunnerResult {
    let mut scanner = Scanner::new(&source);
    scanner.scan_tokens();

    for token in &scanner.tokens {
        if token.typ == TokenType::Error {
            return Err(format!(
                "Scanning error at line {}: {}",
                token.line, token.lexeme
            ));
        }
    }
    let mut parser = parser::Parser::new(scanner.tokens);
    match parser.parse() {
        Ok(expr) => {
            println!("{}", AstPrinter::default().print(&expr));

            Ok("Success".into())
        }
        Err(e) => Err(e.to_string()),
    }
}
