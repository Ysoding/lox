use core::fmt;

use bumpalo::Bump;

#[derive(Debug)]
pub struct Token<'a> {
    pub typ: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
    pub literal: Option<Literal<'a>>,
}

#[derive(Debug)]
pub enum Literal<'a> {
    Number(f64),
    String(&'a str),
    True,
    False,
    Nil,
}

impl<'a> Token<'a> {
    pub fn new(typ: TokenType, lexeme: &'a str, line: usize, literal: Option<Literal<'a>>) -> Self {
        Self {
            typ,
            lexeme,
            line,
            literal,
        }
    }
}

impl<'a> Literal<'a> {
    pub fn clone_to<'b>(&self, bump: &'b Bump) -> Literal<'b> {
        match self {
            Literal::Number(n) => Literal::Number(*n),
            Literal::String(s) => Literal::String(bump.alloc_str(s)),
            Literal::True => Literal::True,
            Literal::False => Literal::False,
            Literal::Nil => Literal::Nil,
        }
    }
}

impl<'a> Token<'a> {
    pub fn clone_to<'b>(&self, bump: &'b Bump) -> &'b Token<'b> {
        bump.alloc(Token {
            typ: self.typ,
            lexeme: bump.alloc_str(self.lexeme),
            line: self.line,
            literal: self.literal.as_ref().map(|lit| lit.clone_to(bump)),
        })
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token: typ: {:?}, lexeme: {}, literal: {:?}",
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
    Break,

    Eof,
    Error,
}
