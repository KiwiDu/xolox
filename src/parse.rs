use std::collections::VecDeque;

use crate::{
    sexpr::S,
    token::{Keywords, Token, TokenType},
};

pub struct Parser {
    stack: VecDeque<Token>,
}
impl Parser {
    pub fn from(tokens: VecDeque<Token>) -> Parser {
        Parser { stack: tokens }
    }

    fn peek(&mut self) -> Option<Token> {
        self.stack.front().cloned()
    }

    fn next(&mut self) -> Option<Token> {
        self.stack.pop_front()
    }

    fn next_if(&mut self, expected: Token) -> Option<Token> {
        self.peek().filter(|c| *c == expected)?; //Early returns None if the desired char is not there
        self.next()
    }

    fn expect_after<T>(
        &mut self,
        f: &mut dyn FnMut(&mut Self) -> T,
        expected: TokenType,
        msg: &str,
    ) -> T {
        let r = f(self);
        match self.next() {
            Some(Token::Op(t)) if t == expected => (),
            _ => panic!("{}", msg),
        }
        r
    }
    pub fn parse_stmt(&mut self) -> S {
        use Token::*;
        let token = self.peek().clone().unwrap_or(EOF);
        match token {
            Op(TokenType::LeftBrace) => {
                self.next();
                let mut stmts = Vec::new();
                while self.peek() != Some(Op(TokenType::RightBrace)) {
                    stmts.push(self.parse_stmt());
                }
                self.next();
                S::Cons(token, stmts)
            }
            Op(_) | Idt(_) | Str(_) | Num(_) => self.expect_after(
                &mut Self::parse_expr,
                TokenType::Semicolon,
                "Unfinished stmt!",
            ),
            Kwd(Keywords::Var) => {
                self.next();
                if let S::Bin(Op(TokenType::Equal), l, r) = self.expect_after(
                    &mut Self::parse_expr,
                    TokenType::Semicolon,
                    "Unfinished stmt!",
                ) {
                    S::Bin(token, l, r)
                } else {
                    panic!("Expected an assignment statment!")
                }
            }
            Kwd(Keywords::If) => {
                self.next();
                let cond = self.parse_expr();
                let thendo = self.parse_stmt();
                let elsedo = match self.next_if(Kwd(Keywords::Else)) {
                    Some(_) => self.parse_stmt(),
                    None => S::Atom(Token::NoOp),
                };

                S::Cons(token, vec![cond, thendo, elsedo])
            }
            Kwd(Keywords::While) => {
                self.next();
                let cond = self.parse_expr();
                let loopdo = self.parse_stmt();

                S::Cons(token, vec![cond, loopdo])
            }
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
        use Keywords::*;
        use Token::*;
        let token = self.next().unwrap_or(EOF);
        let mut left = match token {
            Op(TokenType::LeftParen) => {
                let e = self.expr(0, level + 1);
                if Some(Op(TokenType::RightParen)) != self.next() {
                    panic!("Expected a matching right parenthesis!")
                }; //Must be right parenthesis.
                e
            }
            Op(t) => {
                let (_, r) = prefix_power(t);
                let right = self.expr(r, level + 1);
                S::Unary(token.clone(), Box::new(right))
            }
            Num(_) | Idt(_) | Str(_) | Kwd(True) | Kwd(False) => S::Atom(token.clone()),
            EOF => panic!("Unexpected End of File!"),
            _ => panic!(
                "Bad token! Got '{:#?}', expected a prefix or an expr!",
                token
            ),
        };
        //println!("{}Lhs:\t{}", indent, left);
        while let Some(Op(t)) = self.peek() {
            //println!("{}Op:\t{}", indent, t);
            if let Some((l, _)) = postfix_power(t) {
                if l < power {
                    //println!("{}Break!", indent); //The postfix is not strong enough
                    break;
                }
                self.next();
                left = S::Unary(Op(t), Box::new(left));
                //println!("{}cons:\t{}", indent, left);
            } else if let Some((l, r)) = infix_power(t) {
                if l < power {
                    //println!("{}Break!", indent); //The infix is not strong enough
                    break;
                }
                self.next(); // if the op is strong enough, pop
                let right = self.expr(r, level + 1);
                left = S::Bin(Op(t), Box::new(left), Box::new(right));
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
        Equal => (2, 1),
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
