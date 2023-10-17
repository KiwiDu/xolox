use core::fmt;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum S<T> {
    Atom(T),
    Unary(T, Box<Self>),
    Bin(T, Box<Self>, Box<Self>),
    Cons(T, Vec<Self>),
}

impl fmt::Display for S<Token> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
            S::Unary(head, operand) => write!(f, "({} {})", head, operand),
            S::Bin(head, left, right) => write!(f, "({} {} {})", head, left, right),
        }
    }
}
