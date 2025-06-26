use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::ops::Add;

use crate::{Chunk, LoxError, OpCode, Scanner, Token, TokenType, Value};

pub fn compile(source_code: &str, chunk: &mut Chunk) -> Result<(), LoxError> {
    let parser = Parser::new(source_code);
    parser.compile(chunk)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Add<u8> for Precedence {
    type Output = Self;

    fn add(self, rhs: u8) -> Self::Output {
        Self::try_from(self as u8 + rhs).unwrap()
    }
}

struct Compiler {}

impl Compiler {
    fn new() -> Self {
        Self {}
    }
}
struct Parser<'a> {
    previous: Token<'a>,
    current: Token<'a>,
    scanner: Scanner<'a>,
    had_error: bool,
    panic_mode: bool,
    current_chunk: Option<&'a mut Chunk>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            scanner: Scanner::new(source),
            previous: Token::default(),
            current: Token::default(),
            had_error: false,
            panic_mode: false,
            current_chunk: None,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current;

        while let Some(token) = self.scanner.next() {
            self.current = token;

            if self.current.typ == TokenType::Error {
                self.error_at_current(self.current.lexeme);
            } else {
                break;
            }
        }
    }

    fn compile(mut self, chunk: &'a mut Chunk) -> Result<(), LoxError> {
        self.current_chunk = Some(chunk);
        self.had_error = false;
        self.panic_mode = false;

        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");

        self.end_compiler();

        if self.had_error {
            return Err(LoxError::CompileError);
        }

        Ok(())
    }

    fn consume(&mut self, typ: TokenType, msg: &str) {
        if self.current.typ == typ {
            self.advance();
        } else {
            self.error_at_current(msg);
        }
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current, msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg);
    }

    fn error_at(&mut self, token: Token<'a>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);
        if token.typ == TokenType::Eof {
            eprint!(" at end");
        } else if token.typ == TokenType::Error {
            // do nothing
        } else {
            eprint!(" at '{}'", token.lexeme);
        }
        eprintln!(": {msg}");
        self.had_error = true;
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.current_chunk
            .as_mut()
            .unwrap()
            .write_chunk(byte, self.previous.line);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
    }

    fn emit_return(&mut self) {
        #[cfg(feature = "debug_trace_execution")]
        {
            if !self.had_error {
                self.current_chunk.as_mut().unwrap().disassemble("code");
            }
        }

        self.emit_byte(OpCode::Return);
    }

    fn emit_two_bytes(&mut self, byte1: OpCode, byte2: OpCode) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn number(&mut self) {
        self.emit_constant(self.previous.lexeme.parse::<f64>().unwrap().into());
    }

    fn emit_constant(&mut self, val: Value) {
        let constant = self.make_constant(val);
        if constant < u8::MAX as usize {
            // Use OP_CONSTANT for indices 0-255
            self.emit_byte(OpCode::Constant(constant as u8));
        } else if constant < (1 << 24) {
            // Use OP_CONSTANT_LONG for indices 256-16,777,215
            self.emit_byte(OpCode::ConstantLong(constant as u32));
        } else {
            self.error("Too many constants in one chunk.");
        }
    }

    fn make_constant(&mut self, val: Value) -> usize {
        let constant_index = self.current_chunk.as_mut().unwrap().add_constant(val);
        constant_index
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.previous.typ;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => {
                self.emit_byte(OpCode::Negate);
            }
            _ => return,
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous.typ;
        let rule = ParseRule::get_rule(operator_type);
        self.parse_precedence(rule.precedence + 1);

        match operator_type {
            TokenType::Plus => {
                self.emit_byte(OpCode::Add);
            }
            TokenType::Minus => {
                self.emit_byte(OpCode::Subtract);
            }
            TokenType::Star => {
                self.emit_byte(OpCode::Multiply);
            }
            TokenType::Slash => {
                self.emit_byte(OpCode::Divide);
            }
            _ => return,
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let prefix_rule = match ParseRule::get_rule(self.previous.typ).prefix {
            Some(prefix_rule) => prefix_rule,
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        prefix_rule(self);

        while precedence <= ParseRule::get_rule(self.current.typ).precedence {
            self.advance();

            let infix_rule = ParseRule::get_rule(self.previous.typ).infix.unwrap();

            infix_rule(self);
        }
    }
}

type ParseFn<'a> = fn(&mut Parser<'a>);

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'a> ParseRule<'a> {
    fn new(
        prefix: Option<ParseFn<'a>>,
        infix: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    fn get_rule(typ: TokenType) -> Self {
        match typ {
            TokenType::LeftParen => Self::new(Some(Parser::grouping), None, Precedence::None),
            TokenType::RightParen => Self::new(None, None, Precedence::None),
            TokenType::LeftBrace => Self::new(None, None, Precedence::None),
            TokenType::RightBrace => Self::new(None, None, Precedence::None),
            TokenType::Comma => Self::new(None, None, Precedence::None),
            TokenType::Dot => Self::new(None, None, Precedence::None),
            TokenType::Minus => {
                Self::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term)
            }
            TokenType::Plus => Self::new(None, Some(Parser::binary), Precedence::Term),
            TokenType::Semicolon => Self::new(None, None, Precedence::None),
            TokenType::Slash => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Star => Self::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Bang => Self::new(None, None, Precedence::None),
            TokenType::BangEqual => Self::new(None, None, Precedence::None),
            TokenType::Equal => Self::new(None, None, Precedence::None),
            TokenType::EqualEqual => Self::new(None, None, Precedence::None),
            TokenType::Greater => Self::new(None, None, Precedence::None),
            TokenType::GreaterEqual => Self::new(None, None, Precedence::None),
            TokenType::Less => Self::new(None, None, Precedence::None),
            TokenType::LessEqual => Self::new(None, None, Precedence::None),
            TokenType::Identifier => Self::new(None, None, Precedence::None),
            TokenType::String => Self::new(None, None, Precedence::None),
            TokenType::Number => Self::new(Some(Parser::number), None, Precedence::None),
            TokenType::And => Self::new(None, None, Precedence::None),
            TokenType::Class => Self::new(None, None, Precedence::None),
            TokenType::Else => Self::new(None, None, Precedence::None),
            TokenType::False => Self::new(None, None, Precedence::None),
            TokenType::Fun => Self::new(None, None, Precedence::None),
            TokenType::For => Self::new(None, None, Precedence::None),
            TokenType::If => Self::new(None, None, Precedence::None),
            TokenType::Nil => Self::new(None, None, Precedence::None),
            TokenType::Or => Self::new(None, None, Precedence::None),
            TokenType::Print => Self::new(None, None, Precedence::None),
            TokenType::Return => Self::new(None, None, Precedence::None),
            TokenType::Super => Self::new(None, None, Precedence::None),
            TokenType::This => Self::new(None, None, Precedence::None),
            TokenType::True => Self::new(None, None, Precedence::None),
            TokenType::Var => Self::new(None, None, Precedence::None),
            TokenType::While => Self::new(None, None, Precedence::None),
            TokenType::Break => Self::new(None, None, Precedence::None),
            TokenType::Eof => Self::new(None, None, Precedence::None),
            TokenType::Error => Self::new(None, None, Precedence::None),
        }
    }
}
