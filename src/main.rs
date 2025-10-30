mod ast;
mod environment;
mod interpreter;
mod recursive_descent_parser;
mod runner;
mod scanner;
mod span;

use std::process;

fn main() -> process::ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        todo!("Usage: lox [script]");
    } else if args.len() == 2 {
        match runner::run_file(&args[1]) {
            Ok(_) => process::ExitCode::SUCCESS,
            Err(_) => process::ExitCode::from(64),
        }
    } else {
        match runner::run_repl() {
            Ok(_) => process::ExitCode::SUCCESS,
            // TODO: match different errors
            // and report to user what went wrong
            // with approriate exit codes.
            Err(e) => {
                println!("{}", e);
                process::ExitCode::from(64)
            }
        }
    }
}
