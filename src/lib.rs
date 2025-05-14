use anyhow::Result;

mod expr;
use expr::*;

mod error;
pub use error::*;
mod scanner;
pub use scanner::*;
mod token;
pub use token::*;
mod treewalk_interpreter;
pub use treewalk_interpreter::*;
mod parser;
pub use parser::*;

pub trait Interpreter<'a> {
    fn run(&mut self, source_code: &'a str) -> Result<(), LoxError>;
}
