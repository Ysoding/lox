use std::{cell::Cell, error::Error, fmt};

use anyhow::Result;

use crate::{
    expr::Stmt,
    token::{Literal, TokenType},
    Expr, Token,
};

use bumpalo::{collections::Vec as BVec, Bump};

#[derive(Debug)]
pub struct ParseError<'a> {
    message: String,
    token: &'a Token<'a>,
}

impl Error for ParseError<'_> {}

impl fmt::Display for ParseError<'_> {
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

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

pub struct Parser<'a> {
    tokens: BVec<'a, &'a Token<'a>>,
    bump: &'a Bump,
    current: Cell<usize>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &BVec<'a, &'a Token<'a>>, bump: &'a bumpalo::Bump) -> Self {
        let mut v = BVec::new_in(bump);
        v.extend(tokens);

        Self {
            current: Cell::new(0),
            bump,
            tokens: v,
        }
    }

    pub fn parse(&self) -> ParseResult<BVec<'a, &'a Stmt<'a>>> {
        let mut res = BVec::new_in(self.bump);

        while !self.is_at_end() {
            res.push(self.declaration()?);
        }

        Ok(res)
    }

    fn declaration(&self) -> ParseResult<&'a Stmt<'a>> {
        if self.match_one(&TokenType::Fun) {
            return self.function("function");
        }
        if self.match_one(&TokenType::Var) {
            return self.var_declaration();
        }

        self.statement()
    }

    fn function(&self, kind: &str) -> ParseResult<&'a Stmt<'a>> {
        let name = self.consume(&TokenType::Identifier, &format!("Expect {} name.", kind))?;
        self.consume(
            &TokenType::LeftParen,
            &format!("Expect '(' after {} name.", kind),
        )?;
        let mut parameters = BVec::new_in(&self.bump);
        if !self.check(&TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 parameters.");
                }
                parameters.push(self.consume(&TokenType::Identifier, "Expect parameter name.")?);

                if !self.match_one(&TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(&TokenType::RightParen, "Expect ')' after parameters.")?;

        self.consume(
            &TokenType::LeftBrace,
            &format!("Expect '{{' before {} body.", kind),
        )?;
        let body = self.block_statement()?;
        Ok(self.bump.alloc(Stmt::Function(name, parameters, body)))
    }

    fn var_declaration(&self) -> ParseResult<&'a Stmt<'a>> {
        let name = self.consume(&TokenType::Identifier, "Expect variable name.")?;

        let initializer = if self.match_one(&TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        Ok(self.bump.alloc(Stmt::Var(name, initializer)))
    }

    fn statement(&self) -> ParseResult<&'a Stmt<'a>> {
        if self.match_one(&TokenType::Break) {
            return self.break_statement();
        }

        if self.match_one(&TokenType::For) {
            return self.for_statement();
        }

        if self.match_one(&TokenType::If) {
            return self.if_statement();
        }

        if self.match_one(&TokenType::Print) {
            return self.print_statement();
        }

        if self.match_one(&TokenType::Return) {
            return self.return_statement();
        }

        if self.match_one(&TokenType::While) {
            return self.while_statement();
        }

        if self.match_one(&TokenType::LeftBrace) {
            return self.block_statement();
        }

        self.expression_statement()
    }

    fn return_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        let keyword = self.previous();

        let value = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(self.bump.alloc(Stmt::Return(keyword, value)))
    }

    fn break_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        let break_token = self.previous();
        self.consume(&TokenType::Semicolon, "Expect ';' after 'break'.")?;
        Ok(self.bump.alloc(Stmt::Break(break_token)))
    }

    fn for_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_one(&TokenType::Semicolon) {
            None
        } else if self.match_one(&TokenType::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let mut condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(&TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(incr) = increment {
            let mut v = BVec::new_in(self.bump);
            v.push(body);
            v.push(self.bump.alloc(Stmt::Expression(incr)));

            body = self.bump.alloc(Stmt::Block(v))
        }

        if condition.is_none() {
            condition = Some(self.bump.alloc(Expr::Literal(Literal::True)));
        }

        body = self.bump.alloc(Stmt::While(condition.unwrap(), body));

        if let Some(init) = initializer {
            let mut v = BVec::new_in(self.bump);
            v.push(init);
            v.push(body);

            body = self.bump.alloc(Stmt::Block(v))
        }

        Ok(body)
    }

    fn while_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;

        self.consume(&TokenType::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(self.bump.alloc(Stmt::While(condition, body)))
    }

    fn block_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        let mut statments = BVec::new_in(self.bump);

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statments.push(self.declaration()?);
        }

        self.consume(&TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(self.bump.alloc(Stmt::Block(statments)))
    }

    fn expression_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(self.bump.alloc(Stmt::Expression(expr)))
    }

    fn print_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(self.bump.alloc(Stmt::Print(value)))
    }

    fn if_statement(&self) -> ParseResult<&'a Stmt<'a>> {
        self.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = if self.match_one(&TokenType::Else) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(self
            .bump
            .alloc(Stmt::If(condition, then_branch, else_branch)))
    }

    fn expression(&self) -> ParseResult<&'a Expr<'a>> {
        self.assignment()
    }

    fn assignment(&self) -> ParseResult<&'a Expr<'a>> {
        let expr = self.or()?;

        if self.match_one(&TokenType::Equal) {
            let equals = self.previous();
            let value = self.assignment()?;

            if let Expr::Variable(name) = expr {
                return Ok(self.bump.alloc(Expr::Assign(name, value)));
            }

            return Err(self.error(equals, "Invalid assignment target."));
        }

        Ok(expr)
    }

    fn or(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.and()?;

        while self.match_one(&TokenType::Or) {
            let operator = self.previous();
            let right = self.and()?;
            expr = self.bump.alloc(Expr::Logical(expr, operator, right));
        }

        Ok(expr)
    }

    fn and(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.equality()?;

        while self.match_one(&TokenType::And) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = self.bump.alloc(Expr::Logical(expr, operator, right));
        }

        Ok(expr)
    }

    fn error(&self, token: &'a Token<'a>, msg: &str) -> ParseError<'a> {
        ParseError {
            token,
            message: msg.to_string(),
        }
    }

    fn equality(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.comparison()?;
        let typs = vec![TokenType::BangEqual, TokenType::EqualEqual];
        while self.match_any(&typs) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = self.bump.alloc(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn comparison(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.term()?;
        let typs = vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ];
        while self.match_any(&typs) {
            let operator = self.previous();
            let right = self.term()?;
            expr = self.bump.alloc(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn term(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.factor()?;
        let typs = vec![TokenType::Minus, TokenType::Plus];
        while self.match_any(&typs) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = self.bump.alloc(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn factor(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.unary()?;
        let typs = vec![TokenType::Slash, TokenType::Star];
        while self.match_any(&typs) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = self.bump.alloc(Expr::Binary(expr, operator, right));
        }
        Ok(expr)
    }

    fn unary(&self) -> ParseResult<&'a Expr<'a>> {
        let typs = vec![TokenType::Bang, TokenType::Minus];
        if self.match_any(&typs) {
            let operator = self.previous();
            let right = self.unary()?;
            return Ok(self.bump.alloc(Expr::Unary(operator, right)));
        }
        self.call()
    }

    fn call(&self) -> ParseResult<&'a Expr<'a>> {
        let mut expr = self.primary()?;
        loop {
            if self.match_one(&TokenType::LeftParen) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&self, callee: &'a Expr<'a>) -> ParseResult<&'a Expr<'a>> {
        let mut args = BVec::new_in(&self.bump);
        if !self.check(&TokenType::RightParen) {
            loop {
                if args.len() >= 255 {
                    self.error(self.peek(), "Can't have more than 255 arguments.");
                }
                args.push(self.expression()?);
                if !self.match_one(&TokenType::Comma) {
                    break;
                }
            }
        }
        let paren = self.consume(&TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(self.bump.alloc(Expr::Call(callee, paren, args)))
    }

    fn primary(&self) -> ParseResult<&'a Expr<'a>> {
        if self.match_one(&TokenType::False) {
            return Ok(self.bump.alloc(Expr::Literal(Literal::False)));
        }
        if self.match_one(&TokenType::True) {
            return Ok(self.bump.alloc(Expr::Literal(Literal::True)));
        }
        if self.match_one(&TokenType::Nil) {
            return Ok(self.bump.alloc(Expr::Literal(Literal::Nil)));
        }

        if self.match_one(&TokenType::Number) {
            match &self.previous().literal {
                Some(Literal::Number(v)) => {
                    return Ok(self.bump.alloc(Expr::Literal(Literal::Number(*v))))
                }
                _ => panic!("Expected Number Literal."),
            }
        }
        if self.match_one(&TokenType::String) {
            match &self.previous().literal {
                Some(Literal::String(v)) => {
                    return Ok(self.bump.alloc(Expr::Literal(Literal::String(v))))
                }
                _ => panic!("Expected String Literal."),
            }
        }

        if self.match_one(&TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(&TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(self.bump.alloc(Expr::Grouping(expr)));
        }

        if self.match_one(&TokenType::Identifier) {
            return Ok(self.bump.alloc(Expr::Variable(self.previous())));
        }

        Err(self.error(self.peek(), "Expect expression."))
    }

    fn consume(&self, typ: &TokenType, msg: &str) -> ParseResult<&'a Token<'a>> {
        if self.check(typ) {
            return Ok(self.advance());
        }

        Err(self.error(self.peek(), msg))
    }

    fn advance(&self) -> &'a Token<'a> {
        if !self.is_at_end() {
            self.current.set(self.current.get() + 1);
        }
        self.previous()
    }

    fn match_one(&self, typ: &TokenType) -> bool {
        if self.check(typ) {
            self.advance();
            return true;
        }
        false
    }

    fn check(&self, typ: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().typ == *typ
    }

    fn match_any(&self, typs: &[TokenType]) -> bool {
        typs.iter().any(|typ| self.match_one(typ))
    }

    fn is_at_end(&self) -> bool {
        self.peek().typ == TokenType::Eof
    }

    fn peek(&self) -> &'a Token<'a> {
        // assume current is valid
        self.tokens.get(self.current.get()).unwrap()
    }

    fn previous(&self) -> &'a Token<'a> {
        if self.current.get() == 0 {
            self.tokens.first().unwrap()
        } else {
            self.tokens.get(self.current.get() - 1).unwrap()
        }
    }
}
