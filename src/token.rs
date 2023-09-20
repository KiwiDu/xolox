use std::fmt;

pub trait Stack {
    type Item;
    fn peek(&self) -> Self::Item;
    fn next(&mut self) -> Self::Item;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens.
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

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenType::*;
        write!(
            f,
            "{}",
            match self {
                LeftParen => "(",
                RightParen => ")",
                LeftBrace => "{",
                RightBrace => "}",
                Comma => ",",
                Dot => ".",
                Minus => "-",
                Plus => "+",
                Semicolon => ";",
                Slash => "/",
                Star => "*",
                Bang => "!",
                BangEqual => "!=",
                Equal => "=",
                EqualEqual => "==",
                Greater => ">",
                GreaterEqual => ">=",
                Less => "<",
                LessEqual => "<=",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keywords {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Op(TokenType),
    Idt(String),
    Str(String),
    Kwd(Keywords),
    Num(f64),
    NoOp,
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Token::Op(t) => write!(f, "{}", t),
            Token::Idt(name) => write!(f, "{}", name),
            Token::Str(content) => write!(f, "\"{}\"", content),
            Token::Kwd(kwd) => write!(f, "{:#?}", kwd),
            Token::Num(num) => write!(f, "{}", num),
            Token::NoOp => write!(f, ""),
            Token::EOF => write!(f, ""),
        }
    }
}
