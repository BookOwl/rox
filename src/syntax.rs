use super::scanner;
#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: scanner::Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Literal(scanner::Literal),
    Unary {
        operator: scanner::Token,
        right: Box<Expr>,
    }
}

impl Expr {
    pub fn pretty_print(&self) -> String {
        match *self {
            Expr::Binary { ref left, ref operator, ref right } => format!("({} {} {})", 
                                                                left.pretty_print(), 
                                                                operator.lexeme, 
                                                                right.pretty_print()),
            Expr::Grouping(ref e) => format!("( {} )", e.pretty_print()),
            Expr::Literal(ref l) => match *l {
                scanner::Literal::Identifier(ref x) => format!("{}", x),
                scanner::Literal::Number(ref x) => format!("{}", x),
                scanner::Literal::Str(ref x) => format!("\"{}\"", x),
            }
            Expr::Unary { ref operator, ref right } => format!("({}{})",  
                                                                operator.lexeme, 
                                                                right.pretty_print()),
        }
    }
}