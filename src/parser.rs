use anyhow::Result;
use thiserror::Error;

use crate::{
    expr::Literal,
    token::{self, TokenType},
    Expr, Token,
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Parser error at line {line}: {message}")]
    Internal { line: usize, message: String },
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> ParseResult<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.equality()
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> ParseResult<Expr> {
        let mut expr = self.comparison()?.clone();
        let typs = vec![TokenType::BangEqual, TokenType::EqualEqual];
        while self.match_any(&typs) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> ParseResult<Expr> {
        let mut expr = self.term()?;
        let typs = vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ];
        while self.match_any(&typs) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.factor()?;
        let typs = vec![TokenType::Minus, TokenType::Plus];
        while self.match_any(&typs) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.unary()?;
        let typs = vec![TokenType::Slash, TokenType::Star];
        while self.match_any(&typs) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }
        Ok(expr)
    }

    //    unary          → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> ParseResult<Expr> {
        let typs = vec![TokenType::Bang, TokenType::Minus];
        if self.match_any(&typs) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        return self.primary();
    }

    // primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> ParseResult<Expr> {
        if self.match_one(&TokenType::False) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.match_one(&TokenType::True) {
            return Ok(Expr::Literal(Literal::True));
        }
        if self.match_one(&TokenType::Nil) {
            return Ok(Expr::Literal(Literal::Nil));
        }

        if self.match_one(&TokenType::Number) {
            match &self.previous().literal {
                Some(token::Literal::Number(v)) => return Ok(Expr::Literal(Literal::Number(*v))),
                Some(l) => {
                    return Err(ParseError::Internal {
                        line: self.previous().line,
                        message: format!("Expected a number literal, found: {:?}", l),
                    }
                    .into())
                }
                None => {
                    return Err(ParseError::Internal {
                        line: self.previous().line,
                        message: "Expected a number literal, but found None".into(),
                    }
                    .into())
                }
            }
        }
        if self.match_one(&TokenType::String) {
            match &self.previous().literal {
                Some(token::Literal::String(v)) => {
                    return Ok(Expr::Literal(Literal::String(v.clone())))
                }
                Some(l) => {
                    return Err(ParseError::Internal {
                        line: self.previous().line,
                        message: format!("Expected a string literal, found: {:?}", l),
                    }
                    .into())
                }
                None => {
                    return Err(ParseError::Internal {
                        line: self.previous().line,
                        message: "Expected a string literal, but found None".into(),
                    }
                    .into())
                }
            }
        }

        if self.match_one(&TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        Err(ParseError::Internal {
            line: self.previous().line,
            message: "Unexpected token in expression".into(),
        }
        .into())
    }

    fn consume(&mut self, typ: &TokenType, msg: &str) -> ParseResult<&Token> {
        if self.check(typ) {
            return Ok(self.advance());
        }

        Err(ParseError::Internal {
            line: self.previous().line,
            message: msg.into(),
        }
        .into())
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        return self.previous();
    }

    fn match_one(&mut self, typ: &TokenType) -> bool {
        if self.check(typ) {
            self.advance();
            return true;
        }
        return false;
    }

    fn check(&self, typ: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return self.peek().typ == *typ;
    }

    fn match_any(&mut self, typs: &[TokenType]) -> bool {
        typs.iter().any(|typ| self.match_one(typ))
    }

    fn is_at_end(&self) -> bool {
        self.peek().typ == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        // assume current is valid
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &Token {
        if self.current == 0 {
            self.tokens.get(0).unwrap()
        } else {
            self.tokens.get(self.current - 1).unwrap()
        }
    }
}
