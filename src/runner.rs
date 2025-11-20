use crate::interpreter::{self, Interpreter};
use crate::recursive_descent_parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;
use crate::{ast::AstPrinter, recursive_descent_parser::RecursiveDescentParser};

use std::io::{self, Read, Write};

pub fn run_file(file_name: &str) -> Result<(), RunnerError> {
    let mut file = std::fs::File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let mut scanner = Scanner::new();
    let mut interpreter = Interpreter::new();
    let mut resolver = Resolver::new(&mut interpreter);
    let mut parser = RecursiveDescentParser::new();

    let tokens = scanner.scan_tokens(contents.as_bytes());
    let statements = parser.parse(&tokens)?;

    let mut ast_printer = AstPrinter::new();
    ast_printer.print(&statements);

    resolver.resolve_statements(&statements);

    interpreter.interpret(&statements)?;

    Ok(())
}

pub fn run_repl() -> Result<(), RunnerError> {
    let mut buffer = String::with_capacity(1024);

    let stdin = io::stdin();
    let mut scanner = Scanner::new();

    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        io::stdout().flush()?;
        stdin.read_line(&mut buffer)?;

        if buffer.trim_end() == "exit" || buffer.trim_end() == "quit" {
            println!("good bye!");
            return Ok(());
        }

        let tokens = scanner.scan_tokens(buffer.as_bytes());

        let mut parser = RecursiveDescentParser::new();

        let statements = parser.parse(&tokens)?;

        let mut ast_printer = AstPrinter::new();
        ast_printer.print(&statements);

        interpreter.interpret(&statements)?;

        buffer.clear();
        scanner.clear();
    }
}

#[derive(Debug)]
pub enum RunnerError {
    ParseError(recursive_descent_parser::ParseError),
    RuntimeError(interpreter::RuntimeError),
    IO(io::Error),
}

impl std::fmt::Display for RunnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RunnerError::*;
        match self {
            ParseError(e) => write!(f, "{}", e),
            RuntimeError(e) => write!(f, "{}", e),
            IO(e) => write!(f, "{}", e),
        }
    }
}

impl From<recursive_descent_parser::ParseError> for RunnerError {
    fn from(value: recursive_descent_parser::ParseError) -> Self {
        RunnerError::ParseError(value)
    }
}

impl From<io::Error> for RunnerError {
    fn from(value: io::Error) -> Self {
        RunnerError::IO(value)
    }
}

impl From<interpreter::RuntimeError> for RunnerError {
    fn from(value: interpreter::RuntimeError) -> Self {
        RunnerError::RuntimeError(value)
    }
}
