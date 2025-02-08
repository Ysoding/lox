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
                '/' => {
                    if self._match('/') {
                        while let Some(ch) = self.peek() {
                            if *ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else {
                        self.add_token(TokenType::Slash);
                    }
                }
                ' ' | '\r' | '\t' => {}
                '\n' => {
                    self.line += 1;
                }
                '"' => {
                    self.scan_string();
                }
                _ => {
                    if ch.is_ascii_digit() {
                        self.scan_number();
                    } else if ch.is_alphabetic() {
                        self.scan_identifier();
                    } else {
                        self.tokens.push(Token {
                            typ: TokenType::Error,
                            lexeme: "Unexpected character.",
                            line: self.line,
                        });
                    }
                }
            }
        }
    }

    fn advance_digit(&mut self) {
        while let Some(ch) = self.peek() {
            if !ch.is_ascii_digit() {
                break;
            }
            self.advance();
        }
    }

    fn scan_number(&mut self) {
        self.advance_digit();

        // Look for a fractional part.
        if self.source[self.current..].len() > 2 {
            let mut iter = self.source[self.current..self.current + 2].chars();
            if let Some('.') = iter.next() {
                if let Some(c2) = iter.next() {
                    // consume the .
                    if c2.is_ascii_digit() {
                        self.advance();
                    }
                    self.advance_digit();
                }
            }
        }

        self.add_token(TokenType::Number);
    }

    fn scan_identifier(&mut self) {
        while let Some(ch) = self.peek() {
            if !ch.is_alphabetic() {
                break;
            }
            self.advance();
        }

        let t = match &self.source[self.start..self.current] {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };
        self.add_token(t);
    }

    fn scan_string(&mut self) {
        while let Some(ch) = self.peek() {
            if *ch == '"' {
                break;
            }
            if *ch == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.tokens.push(Token {
                typ: TokenType::Error,
                lexeme: "Unterminated string.",
                line: self.line,
            });
            return;
        }

        // "
        self.advance();
        // "v"
        self.add_token(TokenType::String);
    }

    fn _match(&mut self, ch: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if *self.peek().unwrap() != ch {
            return false;
        }
        self.current += 1;
        true
    }

    fn add_token(&mut self, typ: TokenType) {
        self.tokens.push(Token {
            typ,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        });
    }

    fn peek(&mut self) -> Option<&char> {
        if self.is_at_end() {
            None
        } else {
            self.iter.peek()
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.iter.next()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}
