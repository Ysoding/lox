use core::fmt;

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Number(f64),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub literal: Option<Literal>,
}

impl Token {
    pub fn new(typ: TokenType, lexeme: &str, line: usize, literal: Option<Literal>) -> Self {
        Self {
            typ,
            lexeme: lexeme.to_string(),
            line,
            literal,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token: {:?} {} {:?}",
            self.typ, self.lexeme, self.literal
        )
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, PartialOrd, Ord, Eq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
    Error,
}
