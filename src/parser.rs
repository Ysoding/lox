use crate::{Expr, Token};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: i32,
}

impl<'a> Parser<'a> {
    fn expression(&mut self) -> Expr {
        self.equality()
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Expr {}
}
