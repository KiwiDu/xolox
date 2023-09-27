use std::{collections::VecDeque, iter::Peekable, str::Chars};

use crate::token::{Keywords, Token, TokenType};
pub struct Lexer<'a> {
    stack: Peekable<Chars<'a>>,
}

impl Lexer<'_> {
    pub fn from<'a>(ln: &'a str) -> Lexer<'a> {
        let stack = ln.chars().peekable();
        Lexer { stack }
    }

    fn peek(&mut self) -> Option<&char> {
        self.stack.peek()
    }

    fn next(&mut self) -> Option<char> {
        self.stack.next()
    }

    fn next_if(&mut self, expected: char) -> Option<char> {
        self.peek().filter(|&&c| c == expected)?; //Early returns None if the desired char is not there
        self.next()
    }

    fn match_or<T>(&mut self, expected: char, yes: T, no: T) -> T {
        match self.next_if(expected) {
            Some(_) => yes,
            None => no,
        }
    }

    fn string(&mut self) -> Token {
        let mut s = String::new();
        let mut escaped = false;
        while let Some(c) = self.next() {
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
            if let Some(&c) = self.peek() {
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
            self.next();
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
            if let Some(&c) = self.stack.peek() {
                match c {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                        s.push(c);
                    }
                    _ => break,
                }
            } else {
                break;
            }
            self.stack.next();
        }
        match Self::keyword(&s) {
            Some(k) => Kwd(k),
            None => Idt(s),
        }
    }

    pub fn scan_tokens(&mut self) -> VecDeque<Token> {
        use Token::*;
        use TokenType::*;
        let mut v = VecDeque::with_capacity(0);

        while let Some(c) = self.stack.next() {
            let token = match c {
                '!' => Op(self.match_or('=', BangEqual, Bang)),
                '=' => Op(self.match_or('=', EqualEqual, Equal)),
                '>' => Op(self.match_or('=', GreaterEqual, Greater)),
                '<' => Op(self.match_or('=', LessEqual, Less)),
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
                    if let Some('/') = self.stack.peek() {
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
                v.push_back(token);
            }
        }
        v
    }
}
