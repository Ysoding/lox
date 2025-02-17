/*
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
*/

use crate::token::*;

#[derive(Debug)]
pub enum Expr<'a> {
    Assign(Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Call(Box<Expr<'a>>, Token<'a>, Vec<Expr<'a>>),
    Get(Box<Expr<'a>>, Token<'a>),
    Grouping(Box<Expr<'a>>),
    Literal(Literal),
    Logical(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Set(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Super(Token<'a>, Token<'a>),
    This(Token<'a>),
    Unary(Token<'a>, Box<Expr<'a>>),
    Variable(Token<'a>),
}

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

pub struct AstPrinter;

impl<'a> AstPrinter {
    /// Prints an expression by recursively traversing the AST.
    pub fn print(&self, expr: &Expr<'a>) -> String {
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
                Token {
                    typ: TokenType::Minus,
                    lexeme: "-",
                    line: 1,
                },
                Box::new(Expr::Literal(Literal::Number(123.0))),
            )),
            Token {
                typ: TokenType::Star,
                lexeme: "*",
                line: 1,
            },
            Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        );
        let printer = AstPrinter;
        let output = printer.print(&expr);
        assert_eq!(output, "(* (- 123) (group 45.67))");
    }
}
