/*
program        → statement* EOF ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
*/
use crate::expr::Expr;

#[derive(Debug, Clone)]
pub enum Stmt {
    Block,
    Class,
    Expression(Expr),
    Function,
    If,
    Print(Expr),
    Return,
    Var,
    While,
}
