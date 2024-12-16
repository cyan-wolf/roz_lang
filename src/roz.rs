mod token;
mod scanner;
mod error;
mod expr;
mod stmt;
mod parser;
mod resolver;
mod interpreter;

use std::io::{self, Write};
use error::RozError;
use resolver::Resolver;
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;

// Runs a source file.
pub fn run_file(file_name: String) -> Result<(), io::Error> {
    let content: Vec<_> = std::fs::read_to_string(file_name)?
        .chars()
        .collect();

    match run(&mut Interpreter::new(), content) {
        Err(err) => {
            eprintln!("{err}");

            match err {
                RozError::Syntax(..) => {
                    std::process::exit(65);
                },
                RozError::Resolution(..) => {
                    std::process::exit(67);
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
    let mut interpreter = Interpreter::new();

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut buf = String::new();
        io::stdin().read_line(&mut buf)?;

        let content: Vec<_> = buf.trim()
            .chars()
            .collect();

        match run(&mut interpreter, content) {
            Err(err) => {
                eprintln!("{err}");
            },
            _ => {},
        }
    }
}

fn run(interpreter: &mut Interpreter, content: Vec<char>) -> Result<(), RozError> {
    let scanner = Scanner::new(content);
    let tokens = scanner.scan_tokens()?;

    let mut statements = Parser::new(tokens).parse()?;

    Resolver::new().resolve_program(&mut statements)?;

    interpreter.interpret(statements)?;

    Ok(())
}
