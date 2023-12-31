use std::{
    collections::HashMap,
    fmt,
    ops::{Add, Div, Mul, Not, Sub},
};

use crate::{sexpr::S, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    Str(String),
    Num(f64),
    Bool(bool),
    Obj(HashMap<String, Val>),
    Var(String),
    Fun(String, Vec<String>, S<Token>),
    FFun(String, Vec<String>),
    Nil,
}
impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Var(n) => write!(f, "{}", n),

            Val::Fun(n, args, _) => write!(f, "{}({})", n, args.join(", ")),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Num(n) => write!(f, "{}", n),
            Val::Nil => write!(f, "nil"),

            Val::Obj(fields) => {
                writeln!(f, "{{")?;
                for (name, val) in fields {
                    writeln!(f, "    {} : {}", name, val)?
                }
                write!(f, "}}")
            }
            Val::FFun(n, args) => write!(f, "<ffi: {}({})>", n, args.join(", ")),
        }
    }
}
impl From<bool> for Val {
    fn from(value: bool) -> Self {
        Val::Bool(value)
    }
}
impl From<&Val> for bool {
    fn from(value: &Val) -> Self {
        use Val::*;
        match value {
            Str(s) => !s.is_empty(),
            Num(n) => n.is_normal(),
            Bool(b) => *b,
            _ => false,
        }
    }
}
impl Not for &Val {
    type Output = bool;

    fn not(self) -> Self::Output {
        let b: bool = self.into();
        !b
    }
}
impl Add for &Val {
    type Output = Val;

    fn add(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Str(a), Str(b)) => Str(format!("{}{}", a, b)),
            (Num(a), Num(b)) => Num(a + b),
            (a, b) => panic!("Invalid operation add for '{}' and '{}'!", a, b),
        }
    }
}

impl Sub for &Val {
    type Output = Val;

    fn sub(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a - b),
            (a, b) => panic!("Invalid operation sub for '{}' and '{}'!", a, b),
        }
    }
}

impl Mul for &Val {
    type Output = Val;

    fn mul(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a * b),
            (Str(a), Num(b)) => Str(a.repeat(*b as usize)),
            (a, b) => panic!("Invalid operation mul for '{}' and '{}'!", a, b),
        }
    }
}

impl Div for &Val {
    type Output = Val;

    fn div(self, rhs: Self) -> Self::Output {
        use Val::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a / b),
            (a, b) => panic!("Invalid operation div for '{}' and '{}'!", a, b),
        }
    }
}

impl PartialOrd for &Val {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use Val::*;
        match (self, rhs) {
            (Num(a), Num(b)) => a.partial_cmp(b),
            (a, b) => panic!("Invalid comparison for '{}' and '{}'!", a, b),
        }
    }
}

impl Eq for Val {}
