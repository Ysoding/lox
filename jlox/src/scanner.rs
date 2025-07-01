use std::{iter::Peekable, str::Chars};

use crate::token::{Literal, Token, TokenType};

#[derive(Debug)]
pub struct Scanner<'a> {
    bump: &'a bumpalo::Bump,
    source: &'a str,
    iter: Peekable<Chars<'a>>,
    start: usize,
    current: usize,
    line: usize,

    pub tokens: bumpalo::collections::Vec<'a, &'a Token<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, bump: &'a bumpalo::Bump) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            tokens: bumpalo::collections::Vec::new_in(bump),
            start: 0,
            current: 0,
            line: 1,
            bump,
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        let token = self.bump.alloc(Token::new(
            TokenType::Eof,
            self.bump.alloc_str(""),
            self.line,
            None,
        ));
        self.tokens.push(token);
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
                        // //
                        while let Some(ch) = self.peek() {
                            if *ch == '\n' {
                                break;
                            }
                            self.advance();
                        }
                    } else if self._match('*') {
                        // /* */
                        let mut nesting_level = 1;
                        while nesting_level > 0 {
                            if self.is_at_end() {
                                let token = self.bump.alloc(Token::new(
                                    TokenType::Error,
                                    self.bump.alloc_str("Unterminated block comment."),
                                    self.line,
                                    None,
                                ));
                                self.tokens.push(token);
                                return;
                            }

                            match self.advance() {
                                Some('/') => {
                                    if self._match('*') {
                                        nesting_level += 1;
                                    }
                                }
                                Some('*') => {
                                    if self._match('/') {
                                        nesting_level -= 1;
                                    }
                                }
                                Some('\n') => {
                                    self.line += 1;
                                }
                                _ => {}
                            }
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
                        let token = self.bump.alloc(Token::new(
                            TokenType::Error,
                            self.bump.alloc_str("Unexpected character."),
                            self.line,
                            None,
                        ));
                        self.tokens.push(token);
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
        if self.source[self.current..].len() >= 2 {
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

        let val: f64 = self.source[self.start..self.current].parse().unwrap();

        self.add_token_literal(TokenType::Number, Some(Literal::Number(val)));
    }

    fn scan_identifier(&mut self) {
        while let Some(ch) = self.peek() {
            if !ch.is_alphanumeric() {
                break;
            }
            self.advance();
        }

        let t = match &self.source[self.start..self.current] {
            "break" => TokenType::Break,
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
            self.tokens.push(self.bump.alloc(Token::new(
                TokenType::Error,
                self.bump.alloc_str("Unterminated string."),
                self.line,
                None,
            )));
            return;
        }

        // "
        self.advance();
        // "v"
        self.add_token_literal(
            TokenType::String,
            Some(Literal::String(
                self.bump
                    .alloc_str(&self.source[self.start + 1..self.current - 1]),
            )),
        );
    }

    fn _match(&mut self, ch: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if *self.peek().unwrap() != ch {
            return false;
        }
        self.advance();
        true
    }

    fn add_token(&mut self, typ: TokenType) {
        self.add_token_literal(typ, None);
    }

    fn add_token_literal(&mut self, typ: TokenType, literal: Option<Literal<'a>>) {
        self.tokens.push(self.bump.alloc(Token::new(
            typ,
            self.bump.alloc(&self.source[self.start..self.current]),
            self.line,
            literal,
        )));
    }

    fn peek(&mut self) -> Option<&char> {
        if self.is_at_end() {
            None
        } else {
            self.iter.peek()
        }
    }

    fn advance(&mut self) -> Option<char> {
        if let Some(ch) = self.iter.next() {
            self.current += ch.len_utf8();
            Some(ch)
        } else {
            None
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.source.len()
    }
}
