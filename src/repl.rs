use std::collections::HashMap;

use crate::{
    sexpr::S,
    token::{Keywords, Token, TokenType},
    value::Val,
};
pub struct Repl {
    pub state: HashMap<String, Val>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum V {
    L,
    R,
}

#[derive(Debug, Clone)]
pub struct RuntimeError(pub String);

fn err(s: &str) -> Result<Val, RuntimeError> {
    Err(RuntimeError(s.to_owned()))
}

impl Repl {
    pub fn from() -> Self {
        Self {
            state: HashMap::new(),
        }
    }

    fn atom(&mut self, t: Token, v: V) -> Result<Val, RuntimeError> {
        match (v, t) {
            (V::L, Token::Idt(id)) => Ok(Val::Left(id)),
            (V::L, _) => err("Expected a left value to assign to."),
            (V::R, Token::Idt(id)) => self
                .state
                .get(&id)
                .ok_or(RuntimeError(String::from("")))
                .cloned(), // Retrieve from the state
            (V::R, Token::Kwd(Keywords::True)) => Ok(Val::Bool(true)),
            (V::R, Token::Kwd(Keywords::False)) => Ok(Val::Bool(false)),
            (V::R, Token::Kwd(Keywords::Nil)) => Ok(Val::Nil),
            (V::R, Token::Num(f)) => Ok(Val::Num(f)),
            (V::R, Token::Str(s)) => Ok(Val::Str(s)),
            _ => err("Invalid atom."),
        }
    }

    fn unary(&mut self, tt: &TokenType, oprand: Val, v: V) -> Result<Val, RuntimeError> {
        use TokenType::*;
        if v == V::L {
            return err("Cannot be used as a left value.");
        }
        Ok(match tt {
            Plus => oprand,
            Minus => Val::Num(0.0) - oprand,
            Bang => Val::Bool(!oprand),
            _ => return err(""),
        })
    }

    fn binary(&mut self, tt: &TokenType, left: Val, right: Val, v: V) -> Result<Val, RuntimeError> {
        use TokenType::*;
        if v == V::L {
            return err("Cannot be used as a left value.");
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
            _ => return err(""),
        })
    }
    pub fn exec(&mut self, s: &S) -> Result<Val, RuntimeError> {
        self.eval(s, V::R)
    }

    fn eval(&mut self, s: &S, v: V) -> Result<Val, RuntimeError> {
        use Token::*;
        //println!("{}", sexpr);
        match s {
            S::Atom(t) => self.atom(t.clone(), v),
            S::Unary(Op(tt), s) => {
                let oprand = self.eval(s, v)?;
                self.unary(tt, oprand, v)
            }
            S::Unary(_, _) => err("Invalid Syntax!"),
            S::Bin(Op(TokenType::Equal), left, right) => {
                let l = self.eval(&left, V::L)?;
                let r = self.eval(&right, V::R)?;
                if let Val::Left(name) = l {
                    if self.state.contains_key(&name) {
                        self.state.insert(name, r.clone());
                    } else {
                        return err("Variable does not exist.");
                    }
                } else {
                    return err("Expected a left value to assign to.");
                }

                Ok(r)
            }
            S::Bin(Kwd(Keywords::Var), left, right) => {
                let l = self.eval(&left, V::L)?;
                let r = self.eval(&right, V::R)?;
                if let Val::Left(name) = l {
                    self.state.insert(name, r.clone());
                } else {
                    return err("Expected a left value to assign to.");
                }

                Ok(r)
            }
            S::Bin(Op(tt), left, right) => {
                let l = self.eval(left, v)?; // Left side first
                let r = self.eval(right, v)?;
                self.binary(tt, l, r, v)
            }
            _ => todo!(),
        }
    }
}
