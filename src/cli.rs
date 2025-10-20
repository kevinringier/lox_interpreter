use crate::interpreter::{self, Interpreter};
use crate::recursive_descent_parser;
use crate::scanner::Scanner;
use crate::{ast::AstPrinter, recursive_descent_parser::RecursiveDescentParser};

use std::io::{self, Write};

// TODO: implement error reporting
pub fn run_prompt() -> Result<(), CLIError> {
    let mut buffer = String::with_capacity(1024);
    let stdin = io::stdin();
    let mut scanner = Scanner::new();

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

        let stmts = parser.parse(&tokens)?;

        let ast_printer = AstPrinter::new();
        ast_printer.print(&stmts);

        let interpreter = Interpreter::new();
        interpreter.interpret(&stmts)?;

        buffer.clear();
        scanner.clear();
    }
}

#[derive(Debug)]
pub enum CLIError {
    ParseError(recursive_descent_parser::ParseError),
    RuntimeError(interpreter::RuntimeError),
    IO(io::Error),
}

impl From<recursive_descent_parser::ParseError> for CLIError {
    fn from(value: recursive_descent_parser::ParseError) -> Self {
        CLIError::ParseError(value)
    }
}

impl From<io::Error> for CLIError {
    fn from(value: io::Error) -> Self {
        CLIError::IO(value)
    }
}

impl From<interpreter::RuntimeError> for CLIError {
    fn from(value: interpreter::RuntimeError) -> Self {
        CLIError::RuntimeError(value)
    }
}
