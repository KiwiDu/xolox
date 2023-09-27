use std::fmt;

use crate::token::Token;
pub enum S {
    Atom(Token),
    Unary(Token, Box<S>),
    Bin(Token, Box<S>, Box<S>),
    Cons(Token, Vec<S>),
}

impl fmt::Display for S {
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
