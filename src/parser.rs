use scanner::{Literal, Token, TokenType};
use scanner::TokenType::*;
use syntax::{Expr, Statement};
use super::error;
use failure::Error;

pub type ParseResult = Result<Vec<Box<Statement>>, Error>;
pub type StmtParseResult = Result<Box<Statement>, Error>;
pub type ExprParseResult = Result<Box<Expr>, Error>;

macro_rules! binary_rule {
    ($name:ident => $( $t:path ),+ => $next:ident) => {
        fn $name(&mut self) -> ExprParseResult {
            let mut expr: Box<Expr> = self.$next()?;
            while self.matches(&[$( $t , )*]) {
                let op = self.previous().clone();
                let right: Box<Expr> = self.$next()?;
                expr = Box::new(Expr::Binary {
                    left: expr,
                    operator: op.clone(),
                    right,
                });
            }
            Ok(expr)
    }};
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            current: 0,
        }
    }
    pub fn parse(&mut self) -> ParseResult {
        let mut statements = vec![];
        while !self.is_at_end() {
            statements.push(self.statement()?);
        }
        Ok(statements)
    }
    fn statement(&mut self) -> StmtParseResult {
        if self.matches(&[Print]) {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }
    fn print_statement(&mut self) -> StmtParseResult {
        let val = self.expression()?;
        self.consume(Semicolon, "Expect ';' after value.")?;
        Ok(Box::new(Statement::Print(val)))
    }
    fn expression_statement(&mut self) -> StmtParseResult {
        let val = self.expression()?;
        self.consume(Semicolon, "Expect ';' after value.")?;
        Ok(Box::new(Statement::Expr(val)))
    }
    pub fn expression(&mut self) -> ExprParseResult {
       self.equality()
    }
    binary_rule!(equality => BangEqual, EqualEqual => comparision);
    binary_rule!(comparision => Greater, GreaterEqual, Less, LessEqual => addition);
    binary_rule!(addition => Minus, Plus => multiplication);
    binary_rule!(multiplication => Slash, Star => unary);
    fn unary(&mut self) -> ExprParseResult {
        if self.matches(&[Bang, Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            Ok(Box::new(Expr::Unary { operator, right }))
        } else {
            self.primary()
        }
    }
    fn primary(&mut self) -> ExprParseResult {
        if self.matches(&[False]) { Ok(Box::new(Expr::Literal(Literal::Bool(false)))) }
        else if self.matches(&[True]) { Ok(Box::new(Expr::Literal(Literal::Bool(true)))) }
        else if self.matches(&[Nil]) { Ok(Box::new(Expr::Literal(Literal::Nil))) }
        else if self.matches(&[Number, Str]) { Ok(Box::new(Expr::Literal(self.previous().literal.clone().unwrap()))) }
        else if self.matches(&[LeftParen]) {
           let expr = self.expression()?;
           self.consume(RightParen, "Expect ')' after expression")?;
           Ok(Box::new(Expr::Grouping(expr)))
        } else {
           Err(self.error("Expected expression."))
        }
    }
    fn matches(&mut self, types: &[TokenType]) -> bool {
        for t in types {
           if self.check(*t) {
               self.advance();
               return true;
           }
        }
        false
    }
    fn check(&self, t: TokenType) -> bool {
       if self.is_at_end() {
           false
       } else {
           self.peek().token_type == t
       }
    }
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn consume(&mut self, t: TokenType, msg: &str) -> Result<&Token, Error> {
        if self.check(t) {
            Ok(self.advance())
        } else {
            Err(self.error(msg))
        }
    }
    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    fn previous(&self) -> &Token {
        &self.tokens[self.current-1]
    }
    fn error(&self, msg: &str) -> Error {
        let tok = self.peek();
        error(tok.line, msg)
    }
}