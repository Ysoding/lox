use std::{iter::Peekable, str::Chars};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub line: u32,
    pub typ: TokenType,
}

impl<'a> Token<'a> {
    pub fn new(typ: TokenType, lexeme: &'a str, line: u32) -> Self {
        Self { lexeme, line, typ }
    }

    pub fn synthetic(lexeme: &'a str) -> Self {
        Self {
            lexeme,
            line: 0,
            typ: TokenType::Error,
        }
    }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            lexeme: "",
            line: 1,
            typ: TokenType::Eof,
        }
    }
}

pub struct Scanner<'a> {
    pub source: &'a str,
    iter: Peekable<Chars<'a>>,
    pub line: u32,
    pub start: usize,
    pub current: usize,
    is_eof: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            iter: source.chars().peekable(),
            line: 1,
            current: 0,
            start: 0,
            is_eof: false,
        }
    }

    fn scan_token(&mut self) -> Token<'a> {
        self.skip_white_spaces();

        self.start = self.current;

        if let Some(ch) = self.advance() {
            match ch {
                ch if ch.is_ascii_digit() => return self.scan_number(),
                ch if is_alpha(ch) => return self.scan_identifier(),
                '(' => return self.make_token(TokenType::LeftParen),
                ')' => return self.make_token(TokenType::RightParen),
                '{' => return self.make_token(TokenType::LeftBrace),
                '}' => return self.make_token(TokenType::RightBrace),
                ';' => return self.make_token(TokenType::Semicolon),
                ',' => return self.make_token(TokenType::Comma),
                '.' => return self.make_token(TokenType::Dot),
                '-' => return self.make_token(TokenType::Minus),
                '+' => return self.make_token(TokenType::Plus),
                '/' => return self.make_token(TokenType::Slash),
                '*' => return self.make_token(TokenType::Star),
                '!' => {
                    let t = if self._match('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    };
                    return self.make_token(t);
                }
                '=' => {
                    let t = if self._match('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    };
                    return self.make_token(t);
                }
                '<' => {
                    let t = if self._match('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    };
                    return self.make_token(t);
                }
                '>' => {
                    let t = if self._match('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    };
                    return self.make_token(t);
                }
                '"' => {
                    return self.scan_string();
                }
                _ => return Token::new(TokenType::Error, "Unexpected character.", self.line),
            }
        }

        Token::new(TokenType::Eof, "", self.line)
    }

    fn advance_digit(&mut self) {
        while matches!(self.peek(), Some(ch) if ch.is_ascii_digit()) {
            self.advance();
        }
    }

    fn scan_number(&mut self) -> Token<'a> {
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

        self.make_token(TokenType::Number)
    }

    fn scan_string(&mut self) -> Token<'a> {
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
            return Token::new(TokenType::Error, "Unterminated string.", self.line);
        }

        // "
        self.advance();
        // "v"
        self.make_token(TokenType::String)
    }

    fn advance(&mut self) -> Option<char> {
        match self.iter.next() {
            Some(ch) => {
                self.current += ch.len_utf8();
                Some(ch)
            }
            None => None,
        }
    }

    fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
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

    fn make_token(&self, typ: TokenType) -> Token<'a> {
        Token {
            lexeme: &self.source[self.start..self.current],
            line: self.line,
            typ,
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    fn skip_white_spaces(&mut self) {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if &self.source[self.current..=self.current + 1] == "//" {
                        while matches!(self.peek(), Some(c) if *c != '\n') {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn scan_identifier(&mut self) -> Token<'a> {
        while matches!(self.peek(), Some(&ch) if is_alpha(ch) || ch.is_ascii_digit()) {
            self.advance();
        }

        let typ = match &self.source[self.start..self.current] {
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

        self.make_token(typ)
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_eof {
            None
        } else {
            let token = self.scan_token();
            self.is_eof = token.typ == TokenType::Eof;
            Some(token)
        }
    }
}

fn is_alpha(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}
