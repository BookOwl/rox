use std::cmp::PartialEq;
use std::ops::{Deref, DerefMut};
use std::fmt;
use super::syntax::{Expr, Literal, Token, TokenType};
use super::error;
use failure::Error;

macro_rules! assert_nonnil {
    ($e:expr) => {{
        if $e.type_of() == LoxType::Nil {
            return Err(format_err!("nil error"))
        }
    }};
}

pub type IResult = Result<LoxValue, Error>;

#[derive(Debug, Clone)]
pub enum NonNilLoxValue {
    Number(f64),
    Str(String),
    Bool(bool), 
}

#[derive(Debug, Clone)]
pub struct LoxValue(Option<NonNilLoxValue>);

impl LoxValue {
    pub fn type_of(&self) -> LoxType {
        match **self {
            Some(ref v) => match *v {
                NonNilLoxValue::Bool(_) => LoxType::Bool,
                NonNilLoxValue::Number(_) => LoxType::Number,
                NonNilLoxValue::Str(_) => LoxType::Str,
            }
            None => LoxType::Nil,
        }
    }
    pub fn is_truthy(&self) -> bool {
        if self.type_of() == LoxType::Nil {
            false
        } else {
            match *self.as_ref().unwrap() {
                NonNilLoxValue::Bool(b) => b,
                _ => true
            }
        }
    }
    pub fn nil() -> Self {
        LoxValue(None)
    }
    pub fn number(n: f64) -> Self {
        LoxValue(Some(NonNilLoxValue::Number(n)))
    }
    pub fn bool(b: bool) -> Self {
        LoxValue(Some(NonNilLoxValue::Bool(b)))
    }
    pub fn string(s: String) -> Self {
        LoxValue(Some(NonNilLoxValue::Str(s)))
    }
    pub fn as_number(&self) -> Result<f64, Error> {
        let t = self.type_of();
        match self.0 {
            Some(ref v) => match *v {
                NonNilLoxValue::Number(x) => Ok(x),
                _ => Err(format_err!("Expected number, found {:?}", t))
            }
            None => Err(format_err!("Expected number, found {:?}", t))
        }
    }
    pub fn as_bool(&self) -> Result<bool, Error> {
        let t = self.type_of();
        match self.0 {
            Some(ref v) => match *v {
                NonNilLoxValue::Bool(x) => Ok(x),
                _ => Err(format_err!("Expected bool, found {:?}", t))
            }
            None => Err(format_err!("Expected bool, found {:?}", t))
        }
    }
    pub fn as_string(&self) -> Result<&str, Error> {
        let t = self.type_of();
        match self.0 {
            Some(ref v) => match *v {
                NonNilLoxValue::Str(ref x) => Ok(x),
                _ => Err(format_err!("Expected string, found {:?}", t))
            }
            None => Err(format_err!("Expected string, found {:?}", t))
        }
    }
}
impl PartialEq for LoxValue {
    fn eq(&self, other: &Self) -> bool {
       if self.type_of() == LoxType::Nil || other.type_of() == LoxType::Nil {
           false
       } else {
           unimplemented!()
       }
    }
}
impl Deref for LoxValue {
    type Target = Option<NonNilLoxValue>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for LoxValue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
impl fmt::Display for LoxValue {
   fn fmt(&self, f: &mut fmt::Formatter ) -> fmt::Result {
       match **self {
           Some(ref v) => match *v {
               NonNilLoxValue::Bool(ref x) => write!(f, "{}", x),
               NonNilLoxValue::Number(ref x) => write!(f, "{}", x),
               NonNilLoxValue::Str(ref x) => write!(f, "\"{}\"", x),
           },
           None => write!(f, "nil"),
       }
   }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LoxType {
    Number,
    Str,
    Bool,
    Nil,
}

#[derive(Debug)]
pub struct Interpeter {

}
impl Interpeter {
    pub fn new() -> Self {
        Interpeter {

        }
    }
    pub fn evaulate(&mut self, expr: &Box<Expr>) -> IResult {
        match **expr {
            Expr::Literal(ref lit) => self.evaulate_literal(&lit),
            Expr::Grouping(ref expr) => self.evaulate_grouping(&expr),
            Expr::Unary{ref operator, ref right} => self.evaulate_unary(&operator, &right),
            Expr::Binary{ref left, ref operator, ref right} => self.evaulate_binary(&left, &operator, &right)            
        }
    }
    fn evaulate_literal(&mut self, lit: &Literal) -> IResult {
        match *lit {
            Literal::Nil => Ok(LoxValue::nil()),
            Literal::Bool(b) => Ok(LoxValue::bool(b)),
            Literal::Number(n) => Ok(LoxValue::number(n)),
            Literal::Str(ref s) => Ok(LoxValue::string(s.clone())),
            Literal::Identifier(ref name) => Err(format_err!("rox can't handle variables yet")),
        }
    }
    fn evaulate_grouping(&mut self, expr: &Box<Expr>) -> IResult {
        self.evaulate(expr)
    }
    fn evaulate_unary(&mut self, op: &Token, right: &Box<Expr>) -> IResult {
        let right = self.evaulate(right)?;
        assert_nonnil!(right);
        let t = right.type_of();
        match op.token_type {
            TokenType::Minus => {
                match *right.as_ref().unwrap() {
                    NonNilLoxValue::Number(n) => Ok(LoxValue::number(-n)),
                    _ => Err(error(op.line, &format!("Type error: {:?} doesn't work with unary -", t))),
                }
            },
            TokenType::Bang => {
                Ok(LoxValue::bool(!right.is_truthy()))
            }
            o => Err(error(op.line, &format!("Invalid unary op {:?}!", o))),
        }
    }
    fn evaulate_binary(&mut self, left: &Box<Expr>, op: &Token, right: &Box<Expr>) -> IResult {
        let left = self.evaulate(left)?;
        let right = self.evaulate(right)?;
        match op.token_type {
            TokenType::Minus => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::number(x - y))
            },
            TokenType::Star => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::number(x * y))
            },
            TokenType::Slash => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                if y == 0. {
                    Err(format_err!("Division by 0 error!"))
                } else {
                    Ok(LoxValue::number(x / y))
                }
            },
            TokenType::Greater => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::bool(x > y))
            },
            TokenType::GreaterEqual => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::bool(x >= y))
            },
            TokenType::Less => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::bool(x < y))
            },
            TokenType::LessEqual => {
                let (x, y) = (left.as_number()?, right.as_number()?);
                Ok(LoxValue::bool(x <= y))
            },
            TokenType::Plus => {
                if left.type_of() == LoxType::Number {
                    let (x, y) = (left.as_number()?, right.as_number()?);
                    Ok(LoxValue::number(x + y))
                } else if left.type_of() == LoxType::Str {
                    let (x, y) = (left.as_string()?, right.as_string()?);
                    Ok(LoxValue::string(format!("{}{}", x, y)))
                } else {
                    Err(format_err!("Invalid types for '+': {:?} and {:?}", left.type_of(), right.type_of()))
                }
            },
            _ => Err(format_err!("Unknown binary op {:?}", op.token_type))
        }
    }
}