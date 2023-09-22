use std::io::{self, Write};

use xolox::{
    lexer::Lexer,
    repl::{Repl, RuntimeError},
    sexpr::Parser,
};

fn main() {
    let mut repl = Repl::from();
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();
        let mut ln = String::new();
        io::stdin().read_line(&mut ln).unwrap();
        if ln.trim() == "quit" {
            return;
        }
        let mut lexer = Lexer::from(&ln);
        //print!("Tokens: ");
        let tokens = lexer.scan_tokens();
        /* for token in &tokens {
            print!("{:?} ", token);
        } */
        //println!("");
        let mut parser = Parser::from(&tokens);
        let s = parser.parse_stmt();
        println!("Expr: \n    {}", s);

        print!("    => ");
        match repl.exec(&s) {
            Ok(v) => println!("{}", v),
            Err(RuntimeError(msg)) => println!("Runtime Error: {}", msg),
        }

        //println!("State: {:?}", repl.state);
    }
}
