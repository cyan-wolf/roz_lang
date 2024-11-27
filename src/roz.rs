mod token;
mod scanner;
mod error;
mod expr;
mod parser;
mod interpreter;

use std::io::{self, Write};
use error::RozError;
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;

// Runs a source file.
pub fn run_file(file_name: String) -> Result<(), io::Error> {
    let content: Vec<_> = std::fs::read_to_string(file_name)?
        .chars()
        .collect();

    match run(content) {
        Err(err) => {
            eprintln!("{err}");

            match err {
                RozError::Syntax(..) => {
                    std::process::exit(65);
                },
                RozError::Runtime(..) => {
                    std::process::exit(70);
                },
            }
        },
        _ => {}
    }

    Ok(())
}

// Runs an interactive prompt.
pub fn run_prompt() -> Result<(), io::Error> {
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut buf = String::new();
        io::stdin().read_line(&mut buf)?;

        let content: Vec<_> = buf.trim()
            .chars()
            .collect();

        match run(content) {
            Err(err) => {
                eprintln!("{err}");
            },
            _ => {},
        }
    }
}

fn run(content: Vec<char>) -> Result<(), RozError> {
    let scanner = Scanner::new(content);
    let tokens = scanner.scan_tokens()?;

    let ast = Parser::new(tokens).parse()?;

    let mut interpreter = Interpreter::new();
    interpreter.interpret(ast)?;

    Ok(())
}
