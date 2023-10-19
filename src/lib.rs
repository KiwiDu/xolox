use std::{
    collections::VecDeque,
    fs,
    io::{self, Write},
};

use lexer::Lexer;
use token::Token;

type TokenStack = VecDeque<Token>;

pub fn scan_ln(promt: &'static str, stack: &mut Vec<u8>) -> Option<TokenStack> {
    print!("{}", promt);
    io::stdout().flush().unwrap();
    let mut ln = String::new();
    io::stdin().read_line(&mut ln).unwrap();
    if ln.trim() == "quit" {
        return None;
    }
    let tokens = Lexer::from(&ln).scan();
    stack_check(&tokens, stack);
    Some(tokens)
}

fn stack_check(ln: &TokenStack, stack: &mut Vec<u8>) {
    ln.iter().for_each(|t| match t.to_string().as_str() {
        "{" => stack.push(b'{'),
        "}" => match stack.last() {
            Some(b'{') => {
                stack.pop();
            }
            _ => panic!("Unmatched brackets!"),
        },
        "(" => stack.push(b'('),
        ")" => match stack.last() {
            Some(b'(') => {
                stack.pop();
            }
            _ => panic!("Unmatched parentheses!"),
        },
        _ => (),
    })
}

pub fn from_stdin() -> Option<TokenStack> {
    let mut stack: Vec<u8> = Vec::new();
    let mut tokens = scan_ln(">> ", &mut stack)?;
    while !stack.is_empty() {
        let mut cont = scan_ln(".. ", &mut stack)?;
        tokens.append(&mut cont);
    }
    Some(tokens)
}

pub fn from_file(path: &str) -> Option<TokenStack> {
    let f = fs::read_to_string(path).ok()?;
    Some(Lexer::from(&f).scan())
}

pub mod error;
pub mod lexer;
pub mod parse;
pub mod repl;
pub mod sexpr;
pub mod token;
pub mod value;
