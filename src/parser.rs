use crate::Token;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: i32,
}
