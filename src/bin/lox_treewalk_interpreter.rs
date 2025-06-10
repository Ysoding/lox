use std::{env, fs, path::PathBuf, process};

use bumpalo::Bump;
use lox::{LoxError, Parser, Resolver, Scanner, TokenType, TreewalkInterpreter};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        run_file(args.get(1).unwrap().into());
    } else {
        panic!("No Prompt")
    }
}

fn run_file(file_path: PathBuf) {
    let source = fs::read_to_string(file_path).expect("Failed to read source file");
    let bump = Bump::new();
    let mut interp = TreewalkInterpreter::new(&bump);

    let mut resolver = Resolver::new(&mut interp);
    if let Err(error) = run(&mut resolver, &source) {
        match error {
            lox::LoxError::CompileError => process::exit(65),
            lox::LoxError::RuntimeError => process::exit(70),
        }
    }
}

// fn run_prompt() {
//     let mut input = io::stdin().lock();
//     let mut output = io::stdout();
//     // FIXME: Memory keeps growing indefinitely; needs a reset, or separate the AST part from the Value part.
//     let global_bump = Bump::new();
//     let mut interp = TreewalkInterpreter::new(&global_bump);
//     let mut resolver = Resolver::new(&mut interp);

//     loop {
//         output.write_all(b"> ").unwrap();
//         output.flush().unwrap();

//         let mut buffer = String::new();
//         input
//             .read_line(&mut buffer)
//             .expect("Read line from stdio failed");

//         if buffer.is_empty() {
//             continue;
//         }

//         run(&mut resolver, global_bump.alloc_str(&buffer)).ok();
//     }
// }

fn run<'a>(resolver: &'a mut Resolver<'a>, source_code: &'a str) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source_code, resolver.interpreter.bump);

    scanner.scan_tokens();

    for token in &scanner.tokens {
        if token.typ == TokenType::Error {
            eprintln!("[line {}] Error: Unexpected character.", token.line);
            return Err(LoxError::CompileError);
        }
    }

    let parser = Parser::new(&scanner.tokens, resolver.interpreter.bump);
    match parser.parse() {
        Ok(stmts) => {
            match resolver.resolve(&stmts) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("{}", err);
                    return Err(LoxError::CompileError);
                }
            };
            let res = resolver.interpreter.interpret(&stmts);
            match res {
                Ok(v) => Ok(v),
                Err(e) => {
                    eprintln!("{}\n[line {}]", e.message, e.token.line);
                    Err(LoxError::RuntimeError)
                }
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            Err(LoxError::CompileError)
        }
    }
}
