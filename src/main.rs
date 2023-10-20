use std::env;

use xolox::{error::Error, from_file, from_stdin, parse::Parser, repl::Repl, value::Val};

fn run(repl: &mut Repl, parser: &mut Parser) -> Result<Val, Error> {
    let s = parser.parse_stmt()?;

    println!("Expr: {}", s);

    repl.exec(&s)
}

fn main() {
    let mut repl = Repl::new();
    //let (option, files) = env::args().skip(1).partition(|a| a.starts_with("-"));
    for arg in env::args().skip(1) {
        let file = from_file(&arg).unwrap();
        let mut parser = Parser::from(file);
        while !parser.eof() {
            let result = run(&mut repl, &mut parser);
            print!("   => ");
            match result {
                Ok(v) => println!("{}", v),
                Err(Error::RuntimeError(msg)) => {
                    println!("Runtime Error: {}", msg);
                    break;
                }
                Err(Error::SyntaxError(msg)) => {
                    println!("Syntax Error: {}", msg);
                    break;
                }
                Err(Error::Return(v)) => {
                    println!("Returns(Shouldn't have appeared): {}", v);
                }
            }
        }
    }

    while let Some(tokens) = from_stdin() {
        let mut parser = Parser::from(tokens);
        let result = run(&mut repl, &mut parser);
        print!("   => ");
        match result {
            Ok(v) => println!("{}", v),
            Err(Error::RuntimeError(msg)) => println!("Runtime Error: {}", msg),
            Err(Error::SyntaxError(msg)) => println!("Syntax Error: {}", msg),
            Err(Error::Return(v)) => {
                println!("Returns(Shouldn't have appeared): {}", v);
            }
        }

        //println!("State: {:?}", repl.env);
    }
}
