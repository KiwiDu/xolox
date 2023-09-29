pub mod error;
pub mod lexer;
pub mod parse;
pub mod repl;
pub mod sexpr;
pub mod token;
pub mod value;

use std::{
    collections::VecDeque,
    fs,
    io::{self, Write},
};

use lexer::Lexer;
use token::Token;

type TokenStack = VecDeque<Token>;

fn print(s: &str) {
    print!("{}", s);
    io::stdout().flush().unwrap();
}

pub fn scan_ln() -> Option<TokenStack> {
    let mut ln = String::new();
    io::stdin().read_line(&mut ln).unwrap();
    if ln.trim() == "quit" {
        return None;
    }
    Some(Lexer::from(&ln).scan())
}

pub fn from_stdin() -> Option<TokenStack> {
    print(">> ");
    let mut stack = Vec::new();
    let mut tokens = scan_ln()?;
    loop {
        match tokens.back()?.to_string().as_str() {
            "{" => stack.push('{'),
            "}" => match stack.last() {
                Some('{') => {
                    stack.pop();
                }
                _ => panic!("Unmatched brackets!"),
            },
            "(" => stack.push('('),
            ")" => match stack.last() {
                Some('(') => {
                    stack.pop();
                }
                _ => panic!("Unmatched parentheses!"),
            },
            _ => (),
        }
        if stack.is_empty() {
            break;
        }
        print(".. ");
        let mut cont = scan_ln()?;
        tokens.append(&mut cont);
    }
    Some(tokens)
}

pub fn from_file(path: &str) -> Option<TokenStack> {
    let f = fs::read_to_string(path).ok()?;
    Some(Lexer::from(&f).scan())
}
