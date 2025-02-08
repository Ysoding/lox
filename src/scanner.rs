use std::{iter::Peekable, str::Chars};

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    iter: Peekable<Chars<'a>>,
    start: usize,
    current: usize,
    line: usize,

    pub tokens: Vec<Token<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token {
            typ: TokenType::Eof,
            lexeme: "",
            literal: None,
            line: self.line,
        });
    }

    fn scan_token(&mut self) {
        if let Some(ch) = self.advance() {
            match ch {
                '(' => self.add_token(TokenType::LeftParen),
                ')' => self.add_token(TokenType::RightParen),
                '{' => self.add_token(TokenType::LeftBrace),
                '}' => self.add_token(TokenType::RightBrace),
                ',' => self.add_token(TokenType::Comma),
                '.' => self.add_token(TokenType::Dot),
                '-' => self.add_token(TokenType::Minus),
                '+' => self.add_token(TokenType::Plus),
                ';' => self.add_token(TokenType::Semicolon),
                '*' => self.add_token(TokenType::Star),
                '!' => {
                    let t = if self._match('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    self.add_token(t);
                }
                '=' => {
                    let t = if self._match('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    self.add_token(t);
                }
                '<' => {
                    let t = if self._match('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    self.add_token(t);
                }
                '>' => {
                    let t = if self._match('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    };
                    self.add_token(t);
                }
                _ => {
                    self.tokens.push(Token {
                        typ: TokenType::Error,
                        lexeme: "Unexpected character.",
                        literal: None,
                        line: self.line,
                    });
                }
            }
        }
    }

    fn _match(&mut self, ch: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if *self.peek().unwrap() != ch {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn add_token(&mut self, typ: TokenType) {
        self.tokens.push(Token {
            typ,
            lexeme: &self.source[self.start..self.current],
            literal: None,
            line: self.line,
        });
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.iter.next()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}
