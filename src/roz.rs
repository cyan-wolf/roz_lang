mod token;
mod scanner;
mod error;
mod expr;
mod parser;
mod interpreter;

use std::io::{self, Write};
use error::SyntaxError;
use scanner::Scanner;
use parser::Parser;
use interpreter::Interpreter;

// Runs a source file.
pub fn run_file(file_name: String) -> Result<(), io::Error> {
    let content: Vec<_> = std::fs::read_to_string(file_name)?
        .chars()
        .collect();

    match run(content) {
        Err(errors) => {
            for err in errors {
                eprintln!("{err}");
            }
            std::process::exit(65);
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
            Err(errors) => {
                for err in errors {
                    eprintln!("{err}");
                }
            },
            _ => {},
        }
    }
}

fn run(content: Vec<char>) -> Result<(), Vec<SyntaxError>> {
    let scanner = Scanner::new(content);
    let tokens = scanner.scan_tokens()?;

    let expr = Parser::new(tokens).parse();

    let mut interpreter = Interpreter::new();

    if let Some(expr) = expr {
        if let Err(err) = interpreter.interpret(expr) {
            eprintln!("{err}");
        }
    }

    Ok(())
}
