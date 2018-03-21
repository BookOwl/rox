use std::iter::{Iterator, Peekable};
use std::collections::HashMap;
use std::fmt;
use std::str::Chars;
use failure;
use super::error;

macro_rules! keywords {
    ($($x:ident),*) => (
        {
            use self::TokenType::*;
            let mut map = HashMap::new();
            $(
                map.insert(stringify!($x).to_lowercase(), $x);
            )*
            map
        }
    )
}

lazy_static! {
    static ref KEYWORDS: HashMap<String, TokenType> = keywords!(
        And,
        Class,
        Else,
        False,
        Fun,
        For,
        If,
        Nil,
        Or,
        Print,
        Return,
        Super,
        This,
        True,
        Var,
        While);
}

pub struct Scanner<'a> {
    text: &'a str,
    chars: Peekable<Chars<'a>>,
    start: usize,
    current: usize,
    line: usize,
    eof: bool
}
impl<'a> Scanner<'a> {
    pub fn new(text: &'a str) -> Scanner<'a> {
        Scanner {
            text,
            chars: text.chars().peekable(),
            start: 0,
            current: 0,
            line: 1,
            eof: false,
        }
    }
}
impl<'a> Iterator for Scanner<'a> {
    type Item = Result<Token, failure::Error>;
    fn next(&mut self) -> Option<Result<Token, failure::Error>> {
        if self.eof { 
            None
        } else {
            let token = self.scan_token();
            if let Ok(ref t) = token {
                if let TokenType::EOF = t.token_type {
                    self.eof = true;
                };
            };
            Some(token)
        }
    }
}
impl<'a> Scanner<'a> {
    fn is_at_end(&self) -> bool {
        self.current >= self.text.len()
    }
    fn scan_token(&mut self) -> Result<Token, failure::Error> {
        use self::TokenType::*;
        self.start = self.current;
        let c = self.advance();
        let token_type = match c {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ',' => Comma,
            '.' => Dot,
            '-' => Minus,
            '+' => Plus,
            ';' => Semicolon,
            '*' => Star,
            '!' => if self.match_char('=') { BangEqual } else { Bang },
            '=' => if self.match_char('=') { EqualEqual } else { Equal },
            '<' => if self.match_char('=') { LessEqual } else { Less },
            '>' => if self.match_char('=') { GreaterEqual } else { Greater },
            '/' => {
               if self.match_char('/') {
                  while self.peek() != '\n' && !self.is_at_end() {
                     self.advance();
                  }
                  return self.scan_token()
               } else {
                   Slash
               }
            }
            // Skip whitespace
            // TODO: Get rid of recursion.
            ' ' | '\t' | '\r' => return self.scan_token(),
            '\n' => {
                self.line += 1;
                return self.scan_token();
            }
            '"' => return self.string(),
            '0'...'9' => return self.number(),
            c if is_alpha(c) => return self.identifier(),
            // Handle EOF
            '\0' => return Ok(Token::new(EOF, self.line, "", None)),
            _ => return Err(error(self.line, &format!("Unexpected character {:?}", c)))
        };
        let lexeme = &self.text[self.start..self.current];
        Ok(Token::new(token_type, self.line, lexeme, None))
    }
    fn advance(&mut self) -> char {
        self.current += 1;
        match self.chars.next() {
            Some(c) => c,
            None => '\0'
        }
    }
    fn match_char(&mut self, c: char) -> bool {
       if self.is_at_end() || *self.chars.peek().expect("unexpected EOF") != c {
           false
       } else {
           self.advance();
           true
       }
    }
    fn peek(&mut self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            *self.chars.peek().expect("unexpected EOF")
        }
    }
    fn string(&mut self) -> Result<Token, failure::Error> {
       while self.peek() != '"' && !self.is_at_end() {
           if self.peek() == '\n' { self.line += 1 };
           self.advance();
       };
       if self.is_at_end() {
           Err(error(self.line, "Unterminated string."))
       } else {
           self.advance();
           let literal = Literal::Str(self.text[self.start+1..self.current-1].to_owned());
           let lexeme = &self.text[self.start..self.current];
           Ok(Token::new(TokenType::Str, self.line, lexeme, Some(literal)))
       }
    }
    fn number(&mut self) -> Result<Token, failure::Error> {
        while is_digit(self.peek()) {
           self.advance();
        }
        if self.peek() == '.' && is_digit(self.peek2()) {
            self.advance();
            while is_digit(self.peek()) {
                self.advance();
            }
        }
        let lexeme = &self.text[self.start..self.current];
        let literal: f64 = lexeme.parse().expect("invalid number literal!");
        Ok(Token::new(TokenType::Number, self.line, lexeme, Some(Literal::Number(literal))))
    }
    fn identifier(&mut self) -> Result<Token, failure::Error> {
        while is_alpha_numeric(self.peek()) {
            self.advance();
        }
        let text = self.text[self.start..self.current].to_owned();
        Ok(match KEYWORDS.get(&text) {
            Some(k) => Token::new(*k, self.line, &text, None),
            None => Token::new(TokenType::Identifier, self.line, &text, Some(Literal::Identifier(text.clone())))
        })
    }
    fn peek2(&mut self) -> char {
        if self.current + 1 >= self.text.len() {
            '\0'
        } else {
            let mut chars = self.chars.clone();
            let _ = chars.next().expect("unexpected eof");
            let snd = chars.next().expect("unexpected eof");
            snd
        }
    }
}

fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}

fn is_alpha(c: char) -> bool {
   (c >= 'a' && c <= 'z') ||
   (c >= 'A' && c <= 'Z') ||
   c == '_' 
}

fn is_alpha_numeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub lexeme: String,
    pub literal: Option<Literal>,
}
impl Token {
    pub fn new(token_type: TokenType, line: usize, lexeme: &str, literal: Option<Literal>) -> Token {
        Token {
            token_type,
            line,
            lexeme: lexeme.to_owned(),
            literal,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?}", self.token_type, self.lexeme)?;
        match self.literal {
            Some(ref l) => match *l {
                Literal::Number(ref x) => write!(f, " {}", x),
                Literal::Str(ref x) | Literal::Identifier(ref x) => write!(f, " {}", x),

            },
            None => Ok(())
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    // Single chaarcter tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // Literals
    Identifier,
    Str,
    Number,
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // End of file
    EOF
}

#[derive(Debug, Clone)]
pub enum Literal {
    Str(String),
    Identifier(String),
    Number(f64),
}