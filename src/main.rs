use std::env;
use std::thread;
use xolox::{error::Error, from_file, from_stdin, parse::Parser, repl::Repl, value::Val};

fn run(repl: &mut Repl, parser: &mut Parser, verbose: bool) -> Result<Val, Error> {
    let s = parser.parse_stmt()?;
    if verbose {
        println!("Expr: {}", s);
    }

    repl.exec(&s)
}

fn file_loop(repl: &mut Repl, opt: &Options, path: &str) {
    let file = from_file(path).unwrap();
    let mut parser = Parser::from(file);
    while !parser.eof() {
        let result = run(repl, &mut parser, opt.verbose);
        if opt.verbose {
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
}

fn stdin_loop(repl: &mut Repl, opt: &Options) {
    if !opt.repl {
        return;
    }
    while let Some(tokens) = from_stdin() {
        let mut parser = Parser::from(tokens);
        let result = run(repl, &mut parser, opt.verbose);

        print!("   => ");
        match result {
            Ok(v) => println!("{}", v),
            Err(Error::RuntimeError(msg)) => println!("Runtime Error: {}", msg),
            Err(Error::SyntaxError(msg)) => println!("Syntax Error: {}", msg),
            Err(Error::Return(v)) => {
                println!("Returns(Shouldn't have appeared): {}", v);
            }
        }
        if opt.state {
            println!("State: {:?}", repl.env);
        }
    }
}

struct Options {
    verbose: bool,
    repl: bool,
    state: bool,
}

impl Options {
    fn from(flags: &Vec<String>, files: &Vec<String>) -> Self {
        let mut options = Options {
            verbose: false,
            repl: files.is_empty(), // repl turned on by default if no file is supplied.
            state: false,
        };
        for f in flags.iter().flat_map(|f| f.chars().next()) {
            match f {
                'v' => options.verbose = true,
                'r' => options.repl = true,
                's' => options.state = true,
                _ => eprintln!("Unexpected option '-{}' ignored.", f),
            }
        }
        options
    }
}

fn main() {
    let mut repl = Repl::new();
    let (flags, files) = env::args().skip(1).partition(|a| a.starts_with("-"));
    let options = Options::from(&flags, &files);
    let handler = thread::Builder::new()
        .stack_size(200 * 1024 * 1024)
        .spawn(move || {
            for path in files.iter() {
                file_loop(&mut repl, &options, path);
            }
            stdin_loop(&mut repl, &options);
        })
        .expect("can't spawn thread");
    handler.join().expect("can't spawn thread");
}
