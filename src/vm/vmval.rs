use std::{
    borrow::Borrow,
    fmt,
    ops::{Add, Div, Mul, Not, Sub},
};

#[derive(Clone)]
pub enum VmVal<Str: Borrow<str>> {
    Str(Str),
    Num(f64),
    Bool(bool),
    Var(Str),
    Fun(Str, u8, Vec<u8>),
    Nil,
}
impl<T: Borrow<str> + Clone> VmVal<T> {
    pub fn extract_str(&self) -> Option<T> {
        match self {
            VmVal::Str(s) | VmVal::Var(s) | VmVal::Fun(s, _, _) => Some(s.clone()),
            _ => None,
        }
    }
}
impl<T: Borrow<str>> fmt::Debug for VmVal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(arg0) => f.debug_tuple("Str").field(&arg0.borrow()).finish(),
            Self::Num(arg0) => f.debug_tuple("Num").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Var(arg0) => f.debug_tuple("Var").field(&arg0.borrow()).finish(),
            Self::Fun(arg0, arg1, _arg2) => f
                .debug_tuple("Fun")
                .field(&arg0.borrow())
                .field(arg1)
                //.field(arg2)
                .finish(),
            Self::Nil => write!(f, "Nil"),
        }
    }
}
impl<T: Borrow<str>> fmt::Display for VmVal<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            VmVal::Str(s) => write!(f, "\"{}\"", s.borrow()),
            VmVal::Var(n) => write!(f, "<var {}>", n.borrow()),
            VmVal::Fun(n, _, _) => write!(f, "<fun {}>", n.borrow()),
            VmVal::Bool(b) => write!(f, "{}", b),
            VmVal::Num(n) => write!(f, "{}", n),
            VmVal::Nil => write!(f, "nil"),
        }
    }
}
impl<T: Borrow<str>> From<bool> for VmVal<T> {
    fn from(value: bool) -> Self {
        VmVal::Bool(value)
    }
}
impl<T: Borrow<str>> From<&VmVal<T>> for bool {
    fn from(value: &VmVal<T>) -> Self {
        use VmVal::*;
        match value {
            Str(s) => !s.borrow().is_empty(),
            Num(n) => n.is_normal(),
            Bool(b) => *b,
            _ => false,
        }
    }
}
impl<T: Borrow<str>> Not for &VmVal<T> {
    type Output = bool;

    fn not(self) -> Self::Output {
        let b: bool = self.into();
        !b
    }
}
impl<T: Borrow<str> + From<String>> Add for &VmVal<T> {
    type Output = VmVal<T>;

    fn add(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Str(a), Str(b)) => Str(format!("{}{}", a.borrow(), b.borrow()).into()),
            (Num(a), Num(b)) => Num(a + b),
            (a, b) => panic!("Invalid operation add for '{}' and '{}'!", a, b),
        }
    }
}

impl<T: Borrow<str>> Sub for &VmVal<T> {
    type Output = VmVal<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a - b),
            (a, b) => panic!("Invalid operation sub for '{}' and '{}'!", a, b),
        }
    }
}

impl<T: Borrow<str> + From<String>> Mul for &VmVal<T> {
    type Output = VmVal<T>;

    fn mul(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a * b),
            (Str(a), Num(b)) => Str(a.borrow().repeat(*b as usize).into()),
            (a, b) => panic!("Invalid operation mul for '{}' and '{}'!", a, b),
        }
    }
}

impl<T: Borrow<str>> Div for &VmVal<T> {
    type Output = VmVal<T>;

    fn div(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a / b),
            (a, b) => panic!("Invalid operation div for '{}' and '{}'!", a, b),
        }
    }
}

impl<T: Borrow<str> + PartialEq> PartialEq for &VmVal<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (VmVal::Str(l0), VmVal::Str(r0)) => l0 == r0,
            (VmVal::Num(l0), VmVal::Num(r0)) => l0 == r0,
            (VmVal::Bool(l0), VmVal::Bool(r0)) => l0 == r0,
            (VmVal::Var(l0), VmVal::Var(r0)) => l0 == r0,
            (VmVal::Fun(l0, _, _), VmVal::Fun(r0, _, _)) => l0 == r0,
            _ => core::mem::discriminant(*self) == core::mem::discriminant(*other),
        }
    }
}

impl<T: Borrow<str> + PartialEq> PartialOrd for &VmVal<T> {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => a.partial_cmp(b),
            (a, b) => panic!("Invalid comparison for '{}' and '{}'!", a, b),
        }
    }
}

impl<T: Borrow<str> + Eq> Eq for &VmVal<T> {}
