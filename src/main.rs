use xolox::{error::Error, from_stdin, parse::Parser, repl::Repl};

fn main() {
    let mut repl = Repl::new();
    loop {
        let tokens = from_stdin().unwrap();
        let mut parser = Parser::from(tokens);
        let s = parser.parse_stmt();
        println!("Expr: {}", s);

        let result = repl.exec(&s);
        print!("   => ");
        match result {
            Ok(v) => println!("{}", v),
            Err(Error::RuntimeError(msg)) => println!("Runtime Error: {}", msg),
            Err(Error::SyntaxError(msg)) => println!("Syntax Error: {}", msg),
        }

        //println!("State: {:?}", repl.env);
    }
}
