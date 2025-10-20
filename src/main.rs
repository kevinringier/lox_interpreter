use std::process;

mod ast;
mod cli;
mod interpreter;
mod recursive_descent_parser;
mod scanner;
mod span;

fn main() -> process::ExitCode {
    match cli::run_prompt() {
        Ok(_) => process::ExitCode::SUCCESS,
        // TODO: match different errors
        // and report to user what went wrong
        Err(e) => {
            // TODO: better format error printing and write to std error
            dbg!(e);
            process::ExitCode::from(64)
        }
    }
}
