// https://craftinginterpreters.com/appendix-i.html
use crate::token::*;

#[derive(Debug)]
pub enum Stmt<'a> {
    Block(bumpalo::collections::Vec<'a, &'a Stmt<'a>>),
    Class,
    Expression(&'a Expr<'a>),
    Function,
    If(&'a Expr<'a>, &'a Stmt<'a>, Option<&'a Stmt<'a>>),
    Print(&'a Expr<'a>),
    Return,
    Var(&'a Token<'a>, Option<&'a Expr<'a>>),
    While(&'a Expr<'a>, &'a Stmt<'a>),
    Break(&'a Token<'a>),
}

#[derive(Debug)]
pub enum Expr<'a> {
    Assign(&'a Token<'a>, &'a Expr<'a>),
    Binary(&'a Expr<'a>, &'a Token<'a>, &'a Expr<'a>),
    Call(&'a Expr<'a>, &'a Token<'a>, Vec<&'a Expr<'a>>),
    Get(&'a Expr<'a>, &'a Token<'a>),
    Grouping(&'a Expr<'a>),
    Literal(Literal<'a>),
    Logical(&'a Expr<'a>, &'a Token<'a>, &'a Expr<'a>),
    Set(&'a Expr<'a>, &'a Token<'a>, &'a Expr<'a>),
    Super(&'a Token<'a>, &'a Token<'a>),
    This(&'a Token<'a>),
    Unary(&'a Token<'a>, &'a Expr<'a>),
    Variable(&'a Token<'a>),
}

#[derive(Default)]
pub struct AstPrinter;

impl AstPrinter {
    /// Prints an expression by recursively traversing the AST.
    #[allow(clippy::only_used_in_recursion, unused)]
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
        let t = Token::new(TokenType::Minus, "-", 1, None);
        let left = Expr::Unary(&t, &Expr::Literal(Literal::Number(123.0)));

        let t = Token::new(TokenType::Star, "*", 1, None);
        let expr = Expr::Binary(
            &left,
            &t,
            &Expr::Grouping(&Expr::Literal(Literal::Number(45.67))),
        );
        let printer = AstPrinter;
        let output = printer.print(&expr);
        assert_eq!(output, "(* (- 123) (group 45.67))");
    }
}
