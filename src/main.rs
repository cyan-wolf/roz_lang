use std::io::{self, Write};

use scanner::Scanner;

mod token;
mod scanner;

fn run_file(file_name: String) -> Result<(), io::Error> {
    let content: Vec<_> = std::fs::read_to_string(file_name)?
        .chars()
        .collect();

    run(content);

    Ok(())
}

fn run_prompt() -> Result<(), io::Error> {
    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut buf = String::new();
        io::stdin().read_line(&mut buf)?;

        let content: Vec<_> = buf.trim()
            .chars()
            .collect();

        run(content);
    }
}

fn run(content: Vec<char>) {
    let mut scanner = Scanner::new(content);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{token:?}");
    }
}

fn main() -> Result<(), io::Error> {
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.len() > 1 {
        eprintln!("Usage: roz [script]");
        std::process::exit(1);
    } else if args.len() == 1 {
        run_file(args.into_iter().next().unwrap())?;
    } else {
        run_prompt()?;
    }

    Ok(())
}
