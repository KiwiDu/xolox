use core::fmt;

pub trait Stack {
    type Item;
    fn peek(&self) -> Self::Item;
    fn next(&mut self) -> Self::Item;
}

macro_rules! map_enum {
    ($name:ident<$vtype:ty> $($k:ident : $v:expr),* $(,)?) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        pub enum $name{
            $($k,)*
        }
        impl $name{
            pub fn get(n : $vtype) -> Option<Self>{
                Some(match n{
                    $($v => $name::$k,)*
                    _ => return None,
                })
            }
        }
        impl core::fmt::Display for $name{
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                write!(f, "{}", match self{
                    $($name::$k => $v,)*
                })
            }
        }
        impl core::convert::From<$vtype> for $name{
            fn from(n : $vtype) -> Self{
                Self::get(n).expect("Conversion failed!")
            }
        }
    };
}

map_enum! {
    TokenType<&str>
    // Single-character tokens.
    LeftParen : "(",
    RightParen : ")",
    LeftBrace : "{",
    RightBrace : "}",
    Comma : ",",
    Dot : ".",
    Minus : "-",
    Plus : "+",
    Semicolon : ";",
    Slash : "/",
    Star : "*",
    Bang : "!",
    // One or two character tokens.
    BangEqual : "!=",
    Equal : "=",
    EqualEqual : "==",
    Greater : ">",
    GreaterEqual : ">=",
    Less : "<",
    LessEqual : "<=",
}

map_enum! {
    Keywords<&str>
    And : "and",
    Class : "class",
    Else : "else",
    False : "false",
    Fun : "fun",
    For : "for",
    If : "if",
    Nil : "nil",
    Or : "or",
    Print : "print",
    Return : "return",
    Super : "super",
    This : "this",
    True : "true",
    Var : "var",
    While : "while",
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
            Token::Kwd(kwd) => write!(f, "{}", kwd),
            Token::Num(num) => write!(f, "{}", num),
            Token::NoOp => write!(f, ""),
            Token::EOF => write!(f, ""),
        }
    }
}

impl Token {
    pub fn from(k: &str) -> Token {
        if let Some(x) = TokenType::get(k) {
            return Token::Op(x);
        }
        if let Some(x) = Keywords::get(k) {
            return Token::Kwd(x);
        }
        Token::NoOp
    }
}
