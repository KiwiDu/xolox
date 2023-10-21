use std::collections::VecDeque;

use crate::{
    error::Error,
    sexpr::S,
    token::{Keywords, Token, TokenType},
};

pub struct Parser {
    stack: VecDeque<Token>,
}

type Result = core::result::Result<S<Token>, Error>;

fn stx_err(s: &str) -> Result {
    Err(Error::SyntaxError(s.to_owned()))
}
/*
macro_rules! expect_to_match {
    ($p:pat) => {
        if matches!(self.peek()?, p) {
            self.next()
        } else {
            None
        }
    };
}*/

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

    fn expect_after(
        &mut self,
        f: &mut dyn FnMut(&mut Self) -> S<Token>,
        expected: TokenType,
        msg: &str,
    ) -> Result {
        let r = f(self);
        match self.next() {
            Some(Token::Op(t)) if t == expected => (),
            _ => return stx_err(msg),
        }
        Ok(r)
    }

    fn parse_paren_inner(&mut self) -> Vec<S<Token>> {
        let mut args = Vec::new();
        if self.peek() != Some(Token::from(")")) {
            loop {
                args.push(self.parse_expr());
                if self.peek() != Some(Token::from(",")) {
                    break;
                }
                self.next();
            }
        }
        args
    }

    pub fn eof(&self) -> bool {
        return self.stack.is_empty();
    }

    pub fn parse_stmt(&mut self) -> Result {
        use Token::*;
        let token = self.peek().clone().unwrap_or(EOF);
        Ok(match token {
            //Block stmt
            Op(TokenType::LeftBrace) => {
                self.next();
                let mut stmts = Vec::new();
                while self.peek() != Some(Op(TokenType::RightBrace)) {
                    stmts.push(self.parse_stmt()?);
                }
                self.next();
                S::Cons(token, stmts)
            }
            //Expr or expr stmt (Excluding Identifiers)
            Op(_) | Idt(_) | Str(_) | Num(_) => {
                let e = self.parse_expr();
                if matches!(self.peek(), Some(Op(TokenType::Semicolon))) {
                    self.next();
                    S::Unary(Op(TokenType::Semicolon), Box::new(e))
                } else {
                    e
                }
            }
            //Identifiers could be a variable, a function or smothing else
            /* ident @ Idt(_) => {
                let e = self.parse_expr();
                match self.peek() {
                    // Expr stmt
                    Some(Op(TokenType::Semicolon)) => {
                        self.next();
                        S::Unary(Op(TokenType::Semicolon), Box::new(e))
                    }
                    //Function call
                    Some(Op(TokenType::LeftParen)) => {
                        self.next();
                        let r = S::Cons(ident, self.parse_paren_inner());
                        if self.next() != Some(Token::from(")")) {
                            return stx_err("Unfinished function call!");
                        }
                        if self.peek() == Some(Token::from(";")) {
                            self.next();
                            return Ok(S::Unary(Op(TokenType::Semicolon), Box::new(r)));
                        }
                        r
                    }
                    //Expr
                    None => e,
                    _ => e,
                }
            } */
            //Print stmt
            Kwd(Keywords::Print) => {
                self.next();
                S::Unary(
                    token,
                    Box::new(self.expect_after(
                        &mut Self::parse_expr,
                        TokenType::Semicolon,
                        "Unfinished stmt!",
                    )?),
                )
            }
            //Declaration stmts w. optional assignment
            Kwd(Keywords::Var) => {
                self.next();
                match self.expect_after(
                    &mut Self::parse_expr,
                    TokenType::Semicolon,
                    "Unfinished stmt!",
                )? {
                    S::Bin(Op(TokenType::Equal), l, r) => S::Bin(token, l, r),
                    name @ S::Atom(Idt(_)) => {
                        S::Bin(token, Box::new(name), Box::new(S::Atom(Token::NoOp)))
                    }
                    _ => return stx_err("Expected an assignment statment!"),
                }
            }
            Kwd(Keywords::Fun) => {
                self.next();
                match self.parse_expr() {
                    S::Cons(idt @ Idt(_), mut args) => {
                        args.reserve(2);
                        args.insert(0, S::Atom(idt));
                        args.push(self.parse_stmt()?);
                        S::Cons(token, args)
                    }
                    _ => return stx_err("Invalid function declaration!"),
                }
            }
            //Return Stmt
            Kwd(Keywords::Return) => {
                self.next();
                let retval = self.parse_expr();
                if self.next() != Some(Token::from(";")) {
                    return stx_err("Missing semicolon after a return statment!");
                }
                S::Unary(token, Box::new(retval))
            }
            //If stmt and else caluse
            Kwd(Keywords::If) => {
                self.next();
                let cond = self.parse_expr();
                let thendo = self.parse_stmt()?;
                let elsedo = match self.next_if(Kwd(Keywords::Else)) {
                    Some(_) => self.parse_stmt()?,
                    None => S::Atom(Token::NoOp),
                };

                S::Cons(token, vec![cond, thendo, elsedo])
            }
            //Loop stmts, namely c-styled for and while
            Kwd(Keywords::While) => {
                self.next();
                let cond = self.parse_expr();
                let loopdo = self.parse_stmt()?;

                S::Cons(token, vec![cond, loopdo])
            }
            Kwd(Keywords::For) => {
                self.next();
                let init = self.parse_stmt()?;
                let cond = self.expect_after(
                    &mut Self::parse_expr,
                    TokenType::Semicolon,
                    "Expected semicolon as delimiter!",
                )?;
                let end = self.parse_expr();
                let loopdo = self.parse_stmt()?;

                S::Cons(token, vec![init, cond, end, loopdo])
            }

            Kwd(_) => todo!(),

            NoOp => return stx_err("Unexpected NoOp!"),
            EOF => return stx_err("Unexpected EOF!"),
        })
    }

    pub fn parse_expr(&mut self) -> S<Token> {
        self.expr(0, 0)
    }

    fn expr(&mut self, power: u8, level: usize) -> S<Token> {
        //let indent = "    ".repeat(level);
        //println!("{}Stack: {:?}", indent, self.stack);
        use Keywords::*;
        use Token::*;
        let token = self.next().unwrap_or(EOF);
        let mut left = match token {
            Op(TokenType::LeftParen) => {
                let e = self.expr(0, level + 1);
                if Some(Token::from(")")) != self.next() {
                    panic!("Expected a matching right parenthesis!")
                }; //Must be right parenthesis.
                e
            }
            Op(t) => {
                let (_, r) = prefix_power(t);
                let right = self.expr(r, level + 1);
                S::Unary(token.clone(), Box::new(right))
            }
            Num(_) | Str(_) | Kwd(True) | Kwd(False) => S::Atom(token.clone()),
            Idt(_) => {
                if self.peek() == Some(Token::from("(")) {
                    self.next();
                    let r = S::Cons(token, self.parse_paren_inner());
                    if self.next() != Some(Token::from(")")) {
                        panic!("Unfinished function call!");
                    }
                    r
                } else {
                    S::Atom(token)
                }
            }
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
        Minus | Plus | Bang => ((), 255),
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
    match t {
        Bang => return None, //(255, ()),
        _ => return None,
    }
}
