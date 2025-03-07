/*
program        → statement* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;

expression     → assignment ;
assignment     → IDENTIFIER "=" assignment | equality ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
*/

use crate::token::*;

#[derive(Debug, Clone)]
pub enum Stmt {
    Block,
    Class,
    Expression(Expr),
    Function,
    If,
    Print(Expr),
    Return,
    Var(Token, Option<Expr>),
    While,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Get(Box<Expr>, Token),
    Grouping(Box<Expr>),
    Literal(Literal),
    Logical(Box<Expr>, Token, Box<Expr>),
    Set(Box<Expr>, Token, Box<Expr>),
    Super(Token, Token),
    This(Token),
    Unary(Token, Box<Expr>),
    Variable(Token),
}

#[derive(Debug, Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Default)]
pub struct AstPrinter;

impl AstPrinter {
    /// Prints an expression by recursively traversing the AST.
    pub fn print(&self, expr: &Expr) -> String {
        match expr {
            Expr::Assign(name, value) => {
                format!("(assign {} {})", name.lexeme, self.print(value))
            }
            Expr::Binary(left, op, right) => {
                format!("({} {} {})", op.lexeme, self.print(left), self.print(right))
            }
            Expr::Call(callee, paren, arguments) => {
                let args = arguments
                    .iter()
                    .map(|arg| self.print(arg))
                    .collect::<Vec<_>>()
                    .join(" ");
                format!("(call {} {} {})", self.print(callee), paren.lexeme, args)
            }
            Expr::Get(object, name) => format!("(. {} {})", self.print(object), name.lexeme),
            Expr::Grouping(expression) => format!("(group {})", self.print(expression)),
            Expr::Literal(literal) => match literal {
                Literal::Number(n) => n.to_string(),
                Literal::String(s) => format!("\"{}\"", s),
                Literal::True => "true".to_string(),
                Literal::False => "false".to_string(),
                Literal::Nil => "nil".to_string(),
            },
            Expr::Logical(left, op, right) => {
                format!("({} {} {})", op.lexeme, self.print(left), self.print(right))
            }
            Expr::Set(object, name, value) => {
                format!(
                    "(= {} {} {})",
                    self.print(object),
                    name.lexeme,
                    self.print(value)
                )
            }
            Expr::Super(_keyword, method) => {
                format!("(super {})", method.lexeme)
            }
            Expr::This(_keyword) => "this".to_string(),
            Expr::Unary(op, right) => format!("({} {})", op.lexeme, self.print(right)),
            Expr::Variable(name) => name.lexeme.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::Expr;

    #[test]
    fn expr() {
        // (* (- 123) (group 45.67))
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                Token::new(TokenType::Minus, "-", 1, None),
                Box::new(Expr::Literal(Literal::Number(123.0))),
            )),
            Token::new(TokenType::Star, "*", 1, None),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        );
        let printer = AstPrinter;
        let output = printer.print(&expr);
        assert_eq!(output, "(* (- 123) (group 45.67))");
    }
}
