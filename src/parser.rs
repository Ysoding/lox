use std::{error::Error, fmt};

use anyhow::Result;

use crate::{
    expr::{Literal, Stmt},
    token::{self, TokenType},
    Expr, Token,
};

#[derive(Debug)]
pub struct ParseError {
    message: String,
    token: Token,
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error", self.token.line)?;
        match self.token.typ {
            TokenType::Eof => write!(f, " at end"),
            TokenType::Error => Ok(()),
            _ => write!(f, " at '{}'", self.token.lexeme),
        }?;
        write!(f, ": {}", self.message)
    }
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

    pub fn parse(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut res = vec![];

        while !self.is_at_end() {
            res.push(self.declaration()?);
        }

        Ok(res)
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        if self.match_one(&TokenType::Var) {
            return self.var_declaration();
        }

        return self.statement();
    }

    fn var_declaration(&mut self) -> ParseResult<Stmt> {
        let name = self
            .consume(&TokenType::Identifier, "Expect variable name.")?
            .clone();

        let initializer = if self.match_one(&TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(Stmt::Var(name, initializer))
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        if self.match_one(&TokenType::If) {
            return self.if_statement();
        }

        if self.match_one(&TokenType::Print) {
            return self.print_statement();
        }

        if self.match_one(&TokenType::LeftBrace) {
            return self.block_statement();
        }

        return self.expression_statement();
    }

    fn block_statement(&mut self) -> ParseResult<Stmt> {
        let mut statments = vec![];

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statments.push(Box::new(self.declaration()?));
        }

        self.consume(&TokenType::RightBrace, "Expect ')' after block.")?;
        Ok(Stmt::Block(statments))
    }

    fn expression_statement(&mut self) -> ParseResult<Stmt> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Expression(expr))
    }

    fn print_statement(&mut self) -> ParseResult<Stmt> {
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(value))
    }

    fn if_statement(&mut self) -> ParseResult<Stmt> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_one(&TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If(condition, Box::new(then_branch), else_branch))
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.equality()?;

        if self.match_one(&TokenType::Equal) {
            // let equals = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(name) = expr {
                return Ok(Expr::Assign(name, Box::new(value)));
            }

            return Err(self.error("Invalid assignment target."));
        }

        Ok(expr)
    }

    fn error(&mut self, msg: &str) -> ParseError {
        return ParseError {
            token: self.previous().clone(),
            message: msg.to_string(),
        };
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

    // primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
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
                    return Err(self.error(&format!("Expected a number literal, found: {:?}", l)))
                }
                None => {
                    return Err(self.error("Expected a number literal, but found None"));
                }
            }
        }
        if self.match_one(&TokenType::String) {
            match &self.previous().literal {
                Some(token::Literal::String(v)) => {
                    return Ok(Expr::Literal(Literal::String(v.clone())))
                }
                Some(l) => {
                    return Err(self.error(&format!("Expected a string literal, found: {:?}", l)));
                }
                None => {
                    return Err(self.error("Expected a string literal, but found None"));
                }
            }
        }

        if self.match_one(&TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.match_one(&TokenType::Identifier) {
            return Ok(Expr::Variable(self.previous().clone()));
        }

        return Err(self.error("Unexpected token in expression"));
    }

    fn consume(&mut self, typ: &TokenType, msg: &str) -> ParseResult<&Token> {
        if self.check(typ) {
            return Ok(self.advance());
        }

        return Err(self.error(msg));
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
