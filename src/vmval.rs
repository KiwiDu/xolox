use std::{
    //cell::OnceCell,
    fmt,
    ops::{Add, Div, Mul, Not, Sub},
    rc::Rc,
};

#[derive(Clone)]
pub enum VmVal {
    Str(Rc<str>),
    Num(f64),
    Bool(bool),
    Var(Rc<str>),
    Fun(Rc<str>, u8, Vec<u8>),
    Nil,
}
impl VmVal {
    pub fn extract_str(&self) -> Option<Rc<str>> {
        match self {
            VmVal::Str(s) | VmVal::Var(s) | VmVal::Fun(s, _, _) => Some(Rc::clone(s)),
            _ => None,
        }
    }
}
impl fmt::Debug for VmVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(arg0) => f.debug_tuple("Str").field(arg0).finish(),
            Self::Num(arg0) => f.debug_tuple("Num").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Var(arg0) => f.debug_tuple("Var").field(arg0).finish(),
            Self::Fun(arg0, arg1, _arg2) => f
                .debug_tuple("Fun")
                .field(arg0)
                .field(arg1)
                //.field(arg2)
                .finish(),
            Self::Nil => write!(f, "Nil"),
        }
    }
}
impl fmt::Display for VmVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            VmVal::Str(s) => write!(f, "\"{}\"", s),
            VmVal::Var(n) => write!(f, "<var {}>", n),
            VmVal::Fun(n, _, _) => write!(f, "<fun {}>", n),
            VmVal::Bool(b) => write!(f, "{}", b),
            VmVal::Num(n) => write!(f, "{}", n),
            VmVal::Nil => write!(f, "nil"),
        }
    }
}
impl From<bool> for VmVal {
    fn from(value: bool) -> Self {
        VmVal::Bool(value)
    }
}
impl From<&VmVal> for bool {
    fn from(value: &VmVal) -> Self {
        use VmVal::*;
        match value {
            Str(s) => !s.is_empty(),
            Num(n) => n.is_normal(),
            Bool(b) => *b,
            _ => false,
        }
    }
}
impl Not for &VmVal {
    type Output = bool;

    fn not(self) -> Self::Output {
        let b: bool = self.into();
        !b
    }
}
impl Add for &VmVal {
    type Output = VmVal;

    fn add(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Str(a), Str(b)) => Str(format!("{}{}", a, b).into()),
            (Num(a), Num(b)) => Num(a + b),
            (a, b) => panic!("Invalid operation add for '{}' and '{}'!", a, b),
        }
    }
}

impl Sub for &VmVal {
    type Output = VmVal;

    fn sub(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a - b),
            (a, b) => panic!("Invalid operation sub for '{}' and '{}'!", a, b),
        }
    }
}

impl Mul for &VmVal {
    type Output = VmVal;

    fn mul(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a * b),
            (Str(a), Num(b)) => Str(a.repeat(*b as usize).into()),
            (a, b) => panic!("Invalid operation mul for '{}' and '{}'!", a, b),
        }
    }
}

impl Div for &VmVal {
    type Output = VmVal;

    fn div(self, rhs: Self) -> Self::Output {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => Num(a / b),
            (a, b) => panic!("Invalid operation div for '{}' and '{}'!", a, b),
        }
    }
}

impl PartialOrd for &VmVal {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        use VmVal::*;
        match (self, rhs) {
            (Num(a), Num(b)) => a.partial_cmp(b),
            (a, b) => panic!("Invalid comparison for '{}' and '{}'!", a, b),
        }
    }
}

impl PartialEq for &VmVal {
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

impl Eq for &VmVal {}
