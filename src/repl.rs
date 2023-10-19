use std::collections::HashMap;

use crate::{
    error::Error,
    sexpr::S,
    token::{Keywords, Token, TokenType},
    value::Val,
};

type Result = core::result::Result<Val, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum V {
    L,
    R,
}

fn rt_err(s: &str) -> Result {
    Err(Error::RuntimeError(s.to_owned()))
}

pub struct Repl {
    pub env: Vec<HashMap<String, Val>>,
}

impl Repl {
    fn assign(&mut self, name: String, v: Val, init: bool) -> Option<Val> {
        let frame = self.env.last_mut().unwrap();
        if init || frame.contains_key(&name) {
            frame.insert(name, v)
        } else {
            None
        }
    }
    fn get(&mut self, name: String) -> Option<Val> {
        for frame in self.env.iter_mut().rev() {
            if frame.contains_key(&name) {
                return frame.get(&name).cloned();
            }
        }
        None
    }
}

impl Repl {
    pub fn from() -> Self {
        Self {
            env: vec![HashMap::new()],
        }
    }

    fn atom(&mut self, t: Token, v: V) -> Result {
        match (v, t) {
            (V::L, Token::Idt(id)) => Ok(Val::Left(id)),
            (V::L, _) => rt_err("Expected a left value to assign to."),
            (V::R, Token::Idt(id)) => self
                .get(id)
                .ok_or_else(|| Error::RuntimeError(String::from("Variable not found."))), // Retrieve from the state
            (V::R, Token::Kwd(Keywords::True)) => Ok(Val::Bool(true)),
            (V::R, Token::Kwd(Keywords::False)) => Ok(Val::Bool(false)),
            (V::R, Token::Kwd(Keywords::Nil)) => Ok(Val::Nil),
            (V::R, Token::NoOp) => Ok(Val::Nil), // Kind of make sense...
            (V::R, Token::Num(f)) => Ok(Val::Num(f)),
            (V::R, Token::Str(s)) => Ok(Val::Str(s)),
            _ => rt_err("Invalid atom."),
        }
    }

    fn unary(&mut self, tt: &TokenType, oprand: Val, v: V) -> Result {
        use TokenType::*;
        if v == V::L {
            return rt_err("Cannot be used as a left value.");
        }
        Ok(match tt {
            Plus => oprand,
            Minus => Val::Num(0.0) - oprand,
            Bang => Val::Bool(!oprand),
            _ => return rt_err(""),
        })
    }

    fn binary(&mut self, tt: &TokenType, left: Val, right: Val, v: V) -> Result {
        use TokenType::*;
        if v == V::L {
            return rt_err("Cannot be used as a left value.");
        }
        Ok(match tt {
            Plus => left + right,
            Minus => left - right,
            Star => left * right,
            Slash => left / right,
            EqualEqual => Val::Bool(left == right),
            BangEqual => Val::Bool(left != right),
            Less => Val::Bool(left < right),
            LessEqual => Val::Bool(left <= right),
            Greater => Val::Bool(left > right),
            GreaterEqual => Val::Bool(left >= right),
            _ => return rt_err(""),
        })
    }

    pub fn exec(&mut self, s: &S) -> Result {
        self.eval(s, V::R)
    }

    fn eval(&mut self, s: &S, v: V) -> Result {
        use Token::*;
        //println!("{}", sexpr);
        match s {
            S::Atom(t) => self.atom(t.clone(), v),
            S::Unary(Op(tt), s) => {
                let oprand = self.eval(s, v)?;
                self.unary(tt, oprand, v)
            }
            S::Unary(_, _) => rt_err("Invalid Syntax!"),
            S::Bin(Op(TokenType::Equal), left, right) => {
                let l = self.eval(&left, V::L)?;

                if let Val::Left(name) = l {
                    let r = self.eval(&right, V::R)?;
                    self.assign(name, r.clone(), false).ok_or_else(|| {
                        Error::RuntimeError(String::from(
                            "Variable not found. Declare it first before assignment.",
                        ))
                    })
                } else {
                    rt_err("Expected a left value to assign to.")
                }
            }
            S::Bin(Kwd(Keywords::Var), left, right) => {
                let l = self.eval(&left, V::L)?;

                if let Val::Left(name) = l {
                    let r = self.eval(&right, V::R)?;
                    self.assign(name, r.clone(), true);
                    Ok(r)
                } else {
                    rt_err("Expected a left value to assign to.")
                }
            }
            S::Bin(Op(tt), left, right) => {
                let l = self.eval(left, v)?; // Left side first
                let r = self.eval(right, v)?;
                self.binary(tt, l, r, v)
            }
            S::Cons(Op(TokenType::LeftBrace), stmts) => {
                stmts.iter().map(|s| self.exec(s)).last().unwrap()
            }
            S::Cons(Kwd(Keywords::If), args) => match &args[..] {
                [cond, thendo, elsedo] => {
                    if self.eval(cond, V::R)?.into() {
                        self.exec(thendo)
                    } else {
                        self.exec(elsedo)
                    }
                }
                _ => rt_err("Invalid If clause!"),
            },
            S::Cons(Kwd(Keywords::While), args) => match &args[..] {
                [cond, loopdo] => {
                    let mut ret = Val::Nil;
                    while self.eval(cond, V::R)?.into() {
                        ret = self.exec(loopdo)?
                    }
                    Ok(ret)
                }
                _ => rt_err("Invalid If clause!"),
            },
            _ => todo!(),
        }
    }
}
