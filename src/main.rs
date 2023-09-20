use std::io::{self, Write};

use xolox::{lexer::Lexer, sexpr::Parser};

fn main() {
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let mut ln = String::new();
        io::stdin().read_line(&mut ln).unwrap();
        if ln.trim() == "quit" {
            return;
        }
        let mut lexer = Lexer::from(&ln);
        print!("Tokens: ");
        let tokens = lexer.scan_tokens();
        for token in &tokens {
            print!("{:?} ", token);
        }
        println!("");
        let mut parser = Parser::from(&tokens);
        println!("Expr: \n\t{}", parser.parse_stmt());
    }
}
