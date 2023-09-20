use std::fmt;

use crate::token::{Stack, Token, TokenType};
pub enum S {
    Atom(Token),
    Cons(Token, Vec<S>),
}
/* pub enum Op {
    UnaryPlus,
    UnaryMinus,
    Plus,
    Minus,
    Mul,
    Div,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::UnaryMinus | Op::Minus => write!(f, "-"),
            Op::UnaryPlus | Op::Plus => write!(f, "+"),
            Op::Div => write!(f, "/"),
            Op::Mul => write!(f, "*"),
        }
    }
} */

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
        }
    }
}

pub struct Parser {
    stack: Vec<Token>,
}
impl Stack for Parser {
    type Item = Token;

    fn peek(&self) -> Self::Item {
        self.stack.last().cloned().unwrap_or(Token::EOF)
    }

    fn next(&mut self) -> Self::Item {
        self.stack.pop().unwrap_or(Token::EOF)
    }
}
impl Parser {
    pub fn from(tokens: &Vec<Token>) -> Self {
        let mut stack: Vec<Token> = tokens.iter().cloned().collect();
        stack.reverse();
        Parser { stack }
    }
    fn expect_after<T>(
        &mut self,
        f: &mut dyn FnMut(&mut Self) -> T,
        expected: TokenType,
        msg: &str,
    ) -> T {
        let r = f(self);
        match self.next() {
            Token::Op(t) if t == expected => (),
            _ => panic!("{}", msg),
        }
        r
    }
    pub fn parse_stmt(&mut self) -> S {
        use Token::*;
        let token = self.peek();
        match token {
            Op(_) | Idt(_) | Str(_) | Num(_) => self.expect_after(
                &mut Self::parse_expr,
                TokenType::Semicolon,
                "Unfinished stmt!",
            ),
            Kwd(_) => todo!(),

            NoOp => panic!("Unexpected NoOp!"),
            EOF => panic!("Unexpected EOF!"),
        }
    }

    pub fn parse_expr(&mut self) -> S {
        self.expr(0, 0)
    }

    fn expr(&mut self, power: u8, level: usize) -> S {
        //let indent = "    ".repeat(level);
        //println!("{}Stack: {:?}", indent, self.stack);
        use Token::*;
        let token = self.next();
        let mut left = match token {
            Op(TokenType::LeftParen) => {
                let e = self.expr(0, level + 1);
                if Op(TokenType::RightParen) != self.next() {
                    panic!("Expected a matching right parenthesis!")
                }; //Must be right parenthesis.
                e
            }
            Op(t) => {
                let (_, r) = prefix_power(t);
                let right = self.expr(r, level + 1);
                S::Cons(token, vec![right])
            }
            Num(_) | Idt(_) | Str(_) => S::Atom(token),
            EOF => panic!("Unexpected End of File!"),
            _ => panic!(
                "Bad token! Got '{:#?}', expected a prefix or an expr!",
                token
            ),
        };
        //println!("{}Lhs:\t{}", indent, left);
        while let Op(t) = self.peek() {
            //println!("{}Op:\t{}", indent, t);
            if let Some((l, _)) = postfix_power(t) {
                if l < power {
                    //println!("{}Break!", indent); //The postfix is not strong enough
                    break;
                }
                self.stack.pop();
                left = S::Cons(Op(t), vec![left]);
                //println!("{}cons:\t{}", indent, left);
            } else if let Some((l, r)) = infix_power(t) {
                if l < power {
                    //println!("{}Break!", indent); //The infix is not strong enough
                    break;
                }
                self.stack.pop(); // if the op is strong enough, pop
                let right = self.expr(r, level + 1);
                left = S::Cons(Op(t), vec![left, right]);
                //println!("{}cons:\t{}", indent, left);
            } else {
                //println!("{}Break from expr!", indent); //No suitable operator found
                break;
            }
        }
        //println!("{}Returns: {}", indent, left);
        left
    }
}

fn prefix_power(t: TokenType) -> ((), u8) {
    use TokenType::*;
    match t {
        Minus | Plus => ((), 255),
        _ => panic!("Bad token! Got '{:#?}', expected a prefix!", t),
    }
}

fn infix_power(t: TokenType) -> Option<(u8, u8)> {
    use TokenType::*;
    Some(match t {
        Equal => (1, 2),
        Greater | GreaterEqual | Less | LessEqual => (3, 4),
        BangEqual | EqualEqual => (5, 6),
        Minus | Plus => (7, 8),
        Slash | Star => (9, 10),
        _ => return None,
    })
}

fn postfix_power(t: TokenType) -> Option<(u8, ())> {
    use TokenType::*;
    Some(match t {
        Bang => (255, ()),
        _ => return None,
    })
}
