use crate::{Token, TokenType};

#[derive(Debug, Default)]
pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: usize,

    pub tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(input: String) -> Self {
        Self {
            source: input,
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
            typ: TokenType::EOF,
            lexeme: "".to_string(),
            literal: None,
            line: self.line,
        });
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self) {}
}
