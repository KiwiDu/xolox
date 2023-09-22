use std::{collections::HashMap, ops};

use crate::{
    sexpr::S,
    token::{Keywords, Token, TokenType},
};
pub struct Repl {
    pub state: HashMap<String, Token>,
}
impl Repl {
    pub fn from() -> Self {
        Self {
            state: HashMap::new(),
        }
    }

    pub fn exec(&mut self, s: &S, as_left: bool) -> Token {
        use Keywords::*;
        use Token::*;
        use TokenType::*;
        //println!("{}", sexpr);
        match s {
            S::Atom(t) => {
                if as_left {
                    t.clone()
                } else {
                    match t {
                        Idt(id) => self.state.get(id).unwrap().clone(),
                        _ => t.clone(),
                    }
                }
            }
            S::Cons(Op(t), tail) => match t {
                Plus => tail
                    .iter()
                    .map(|x| self.exec(x, false))
                    .reduce(|a, b| a + b)
                    .expect("Invalid add expression with 0 argument!"),
                Minus => Some(Num(0.0))
                    .filter(|_| tail.len() == 1)
                    .into_iter()
                    .chain(tail.into_iter().map(|x| self.exec(x, false)))
                    .reduce(ops::Sub::sub)
                    .unwrap(),
                LeftParen | RightParen | LeftBrace | RightBrace => {
                    panic!("Not supposed to be in a expr.")
                }
                Comma => todo!(),
                Dot => todo!(),
                Semicolon => todo!(),
                Slash => {
                    assert_eq!(tail.len(), 2);
                    match (self.exec(&tail[0], false), self.exec(&tail[1], false)) {
                        (a, b) => a / b,
                    }
                }
                Star => tail
                    .iter()
                    .map(|x| self.exec(x, false))
                    .reduce(|a, b| a * b)
                    .unwrap(),
                Bang => todo!(),
                BangEqual => self.cmp(tail, &|a, b| a != b),
                Equal => {
                    assert_eq!(tail.len(), 2);
                    if let Idt(lhs) = self.exec(&tail[0], true) {
                        let rhs = self.exec(&tail[1], false);
                        self.state.insert(lhs, rhs.clone());
                        rhs
                    } else {
                        panic!("Expected left value!");
                    }
                }
                EqualEqual => self.cmp(tail, &|a, b| a == b),
                /* Greater => self.cmp(tail, &|a, b| a > b),
                GreaterEqual => self.cmp(tail, &|a, b| a >= b),
                Less => self.cmp(tail, &|a, b| a < b),
                LessEqual => self.cmp(tail, &|a, b| a <= b), */
                _ => todo!(),
            },
            S::Cons(_, _) => todo!(),
        }
    }
    fn cmp(&mut self, tail: &Vec<S>, pred: &dyn Fn(Token, Token) -> bool) -> Token {
        use Keywords::*;
        use Token::*;
        assert_eq!(tail.len(), 2);
        let lhs = self.exec(&tail[0], false);
        let rhs = self.exec(&tail[1], false);
        if pred(lhs, rhs) {
            Kwd(True)
        } else {
            Kwd(False)
        }
    }
}
