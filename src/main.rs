use std::env;
use std::fmt::Display;
use std::mem::swap;
use std::thread;
use xolox::compile::Compiler;
use xolox::vm::VM;
use xolox::vmval::VmVal as Val;
use xolox::{
    error::Error,
    from_file,
    from_stdin,
    parse::Parser,
    repl::Repl,
    //value::Val
};
/*
fn run(repl: &mut Repl, parser: &mut Parser, verbose: bool) -> Result<Val, Error> {
    let s = parser.parse_stmt()?;
    if verbose {
        println!("Expr: {}", s);
    }

    repl.exec(&s)
}
*/
fn file_loop<T>(
    opt: &Options,
    path: &str,
    mut run: impl FnMut(&mut Parser, &Options) -> Result<T, Error>,
) where
    T: Display,
{
    let file = from_file(path).unwrap();
    let mut parser = Parser::from(file);
    while !parser.eof() {
        let result = run(&mut parser, opt);
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

fn stdin_loop<T>(opt: &Options, mut run: impl FnMut(&mut Parser, &Options) -> Result<T, Error>)
where
    T: Display,
{
    if !opt.repl {
        return;
    }
    while let Some(tokens) = from_stdin() {
        let mut parser = Parser::from(tokens);
        let result = run(&mut parser, opt);

        if opt.verbose {
            print!("   => ");
            match result {
                Ok(v) => println!("{}", v),
                Err(Error::RuntimeError(msg)) => println!("Runtime Error: {}", msg),
                Err(Error::SyntaxError(msg)) => println!("Syntax Error: {}", msg),
                Err(Error::Return(v)) => {
                    println!("Returns(Shouldn't have appeared): {}", v);
                }
            }
        }
        /* if opt.state {
            println!("State: {:?}", repl.env);
        } */
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

fn s(vm: &mut VM, c: &mut Compiler) {
    swap(&mut c.heap, &mut vm.heap);
    //swap(&mut c.chunk, &mut vm.chunk);
}
fn main() {
    let mut compiler = Compiler::new();
    let mut vm = VM::new();
    vm.reg(&compiler);
    /* let mut repl = Repl::new();*/
    let (flags, files) = env::args().skip(1).partition(|a| a.starts_with("-"));
    let options = Options::from(&flags, &files);
    for path in files.iter() {
        file_loop(&options, path, |parser: &mut Parser, opt: &Options| {
            while let Ok(a) = &parser.parse_stmt() {
                println!("{}", a);
                compiler.compile(a, false);
            }
            println!();
            compiler.dasm();
            println!();
            s(&mut vm, &mut compiler);
            let output = vm.run().unwrap_or(&Val::Nil);
            let output = format!("{}", output);
            println!(
                "{:?}\n\
                \tHeap  = {:?}\n\
                \tStack = {:?}\n\
                \tGlobal= {:?}",
                output, vm.heap, vm.stack, vm.globals
            );

            s(&mut vm, &mut compiler);
            Ok(output)
        });
    }

    stdin_loop(&options, |parser: &mut Parser, opt: &Options| {
        compiler.compile(&parser.parse_stmt().ok().unwrap(), false);
        s(&mut vm, &mut compiler);
        let output = vm.run().unwrap_or(&Val::Nil);
        let output = format!("{}", output);
        println!(
            "{:?}\n\
            \tHeap  = {:?}\n\
            \tStack = {:?}\n\
            \tGlobal= {:?}",
            output, vm.heap, vm.stack, vm.globals
        );

        s(&mut vm, &mut compiler);
        Ok(output)
    });
}
