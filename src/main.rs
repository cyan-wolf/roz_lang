mod token;
mod scanner;
mod error;
mod expr;
mod parser;

use std::io::{self, Write};
use parser::Parser;
use scanner::Scanner;
use error::Error;

// Runs a source file.
fn run_file(file_name: String) -> Result<(), io::Error> {
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
fn run_prompt() -> Result<(), io::Error> {
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

fn run(content: Vec<char>) -> Result<(), Vec<Error>> {
    let scanner = Scanner::new(content);
    let tokens = scanner.scan_tokens()?;

    let expr = Parser::new(tokens).parse();

    if let Some(expr) = expr {
        println!("{expr}");
    }

    Ok(())
}

fn main() -> Result<(), io::Error> {
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.len() > 1 {
        eprintln!("Usage: roz [script]");
        std::process::exit(64);
    } else if args.len() == 1 {
        run_file(args.into_iter().next().unwrap())?;
    } else {
        run_prompt()?;
    }

    Ok(())
}
