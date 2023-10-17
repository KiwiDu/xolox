use std::{collections::HashMap, iter::zip};

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
    pub out: Vec<String>,
}

impl Repl {
    fn assign(&mut self, name: &str, v: Val, init: bool) -> Option<Val> {
        let frame = self.env.last_mut().unwrap();
        if init || frame.contains_key(name) {
            frame.insert(name.to_owned(), v)
        } else {
            None
        }
    }
    fn get(&mut self, name: &str) -> Option<Val> {
        for frame in self.env.iter_mut().rev() {
            if frame.contains_key(name) {
                return frame.get(name).cloned();
            }
        }
        None
    }
    fn fork(&mut self) {
        self.env.push(HashMap::new());
    }
    fn unfork(&mut self) {
        if self.env.len() >= 1 {
            self.env.pop();
        }
    }
}

impl Repl {
    pub fn new() -> Self {
        Self {
            env: vec![HashMap::new()],
            out: vec![],
        }
    }

    fn atom(&mut self, t: Token, v: V) -> Result {
        match (v, t) {
            (V::L, Token::Idt(id)) => Ok(Val::Var(id)),
            (V::L, _) => rt_err("Expected a left value to assign to."),
            (V::R, Token::Idt(id)) => self
                .get(&id)
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
            Semicolon => Val::Nil,
            _ => return rt_err(format!("Unexpected unary operator: {}!", tt).as_str()),
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

    pub fn exec(&mut self, s: &S<Token>) -> Result {
        let result = self.eval(s, V::R);
        for s in std::mem::replace(&mut self.out, Vec::new()) {
            println!("{}", s);
        }
        result
    }

    fn eval(&mut self, s: &S<Token>, v: V) -> Result {
        use Token::*;
        //println!("{}", sexpr);
        match s {
            S::Atom(t) => self.atom(t.clone(), v),
            S::Unary(Op(tt), s) => {
                let oprand = self.eval(s, v)?;
                self.unary(tt, oprand, v)
            }
            S::Unary(Token::Kwd(Keywords::Print), e) => {
                let s = format!("{}", self.eval(e, V::R)?);
                self.out.push(s);
                Ok(Val::Nil)
            }
            S::Bin(Op(TokenType::Equal), left, right) => {
                let l = self.eval(&left, V::L)?;

                if let Val::Var(name) = l {
                    let r = self.eval(&right, V::R)?;
                    self.assign(&name, r.clone(), false).ok_or_else(|| {
                        Error::RuntimeError(String::from(
                            "Variable not found. Declare it first before assignment.",
                        ))
                    })
                } else {
                    rt_err("Expected a left value to assign to.")
                }
            }
            S::Cons(Kwd(Keywords::Fun), tail) => match &tail[..] {
                [S::Atom(Idt(name)), ref args @ .., body] => {
                    let args: Vec<_> = args
                        .iter()
                        .map(|x| match x {
                            S::Atom(Idt(name)) => name,
                            _ => panic!("Invalid function declaration!"), //TODO: How to get rid of it?
                        })
                        .cloned()
                        .collect();
                    let func = Val::Fun(name.to_string(), args, body.clone());
                    self.assign(name, func.clone(), true);
                    Ok(func)
                }
                _ => rt_err("Expected a valid function declaration."),
            },
            S::Cons(Idt(name), tail) => match self.get(name) {
                Some(Val::Fun(_, args, body)) => {
                    if args.len() != tail.len() {
                        return rt_err("Arity does not match!");
                    }
                    self.fork();
                    for (arg, para) in zip(args, tail) {
                        let v = self.eval(para, V::R)?;
                        self.assign(&arg, v, true);
                    }
                    let e = self.exec(&body);
                    self.unfork();
                    e
                }
                Some(_) => rt_err("Object not callable!"),
                _ => rt_err("Function does not exist!"),
            },
            S::Bin(Kwd(Keywords::Var), left, right) => {
                let l = self.eval(&left, V::L)?;

                if let Val::Var(name) = l {
                    let r = self.eval(&right, V::R)?;
                    self.assign(&name, r.clone(), true);
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
                    while self.eval(cond, V::R)?.into() {
                        self.exec(loopdo)?;
                    }
                    Ok(Val::Nil)
                }
                _ => rt_err("Invalid If clause!"),
            },
            S::Cons(Kwd(Keywords::For), args) => match &args[..] {
                [init, cond, end, loopdo] => {
                    self.exec(init)?;
                    while self.eval(cond, V::R)?.into() {
                        self.exec(loopdo)?;
                        self.exec(end)?;
                    }
                    Ok(Val::Nil)
                }
                _ => rt_err("Invalid If clause!"),
            },
            _ => todo!(),
        }
    }
}
