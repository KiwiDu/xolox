use crate::token::{Keywords, Token, TokenType};
pub struct Lexer {
    stack: Vec<char>,
}

impl Lexer {
    pub fn from(ln: &str) -> Self {
        let mut stack: Vec<char> = ln.chars().collect();
        stack.reverse();
        Lexer { stack }
    }

    fn expect_or<T>(&mut self, expected: char, yes: T, no: T) -> T {
        match self.stack.last() {
            Some(c) if c == &expected => {
                self.stack.pop(); //Got what we expected, consume it.
                yes
            }
            _ => no, //Save the char for next iteration.
        }
    }

    fn string(&mut self) -> Token {
        let mut s = String::new();
        let mut escaped = false;

        while let Some(c) = self.stack.pop() {
            match c {
                '\\' => escaped = true,
                '"' if !escaped => break,
                _ => (),
            }
            s.push(c)
        }
        Token::Str(s)
    }

    fn number(&mut self, mut s: String) -> Token {
        let mut no_dot = true;
        loop {
            if let Some(&c) = self.stack.last() {
                match c {
                    '0'..='9' => s.push(c),
                    '.' if no_dot => {
                        no_dot = false;
                        s.push(c);
                    }
                    _ => break,
                }
            } else {
                break;
            }
            self.stack.pop();
        }
        Token::Num(s.parse().unwrap())
    }

    fn keyword(s: &str) -> Option<Keywords> {
        use Keywords::*;
        Some(match s {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "fun" => Fun,
            "for" => For,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => return None,
        })
    }

    fn name(&mut self, mut s: String) -> Token {
        use Token::*;
        loop {
            if let Some(c) = self.stack.last() {
                match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                        s.push(*c);
                    }
                    _ => break,
                }
            } else {
                break;
            }
            self.stack.pop();
        }
        if let Some(k) = Self::keyword(&s) {
            Kwd(k)
        } else {
            Idt(s)
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        use Token::*;
        use TokenType::*;
        let mut v = vec![];

        while let Some(c) = self.stack.pop() {
            let token = match c {
                '!' => Op(self.expect_or('=', BangEqual, Bang)),
                '=' => Op(self.expect_or('=', EqualEqual, Equal)),
                '>' => Op(self.expect_or('=', GreaterEqual, Greater)),
                '<' => Op(self.expect_or('=', LessEqual, Less)),
                '(' => Op(LeftParen),
                ')' => Op(RightParen),
                '{' => Op(LeftBrace),
                '}' => Op(RightBrace),
                '+' => Op(Plus),
                '-' => Op(Minus),
                '*' => Op(Star),
                ',' => Op(Comma),
                '.' => Op(Dot),
                ';' => Op(Semicolon),
                '/' => {
                    if let Some('/') = self.stack.last() {
                        break; // Cannot acomplish this via the `expect_or`
                    } else {
                        Op(Slash)
                    }
                }
                ' ' | '\t' | '\n' | '\r' => NoOp,
                '0'..='9' => self.number(c.to_string()),
                'a'..='z' | 'A'..='Z' | '_' => self.name(c.to_string()),
                '"' => self.string(),
                _ => panic!("Invalid char '{}'", c),
            };
            if token != NoOp {
                v.push(token);
            }
        }
        v
    }
}
