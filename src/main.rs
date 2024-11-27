mod roz;

use std::io;

fn main() -> Result<(), io::Error> {
    let args: Vec<_> = std::env::args().skip(1).collect();

    if args.len() > 1 {
        eprintln!("Usage: roz [script]");
        std::process::exit(64);
    } else if args.len() == 1 {
        roz::run_file(args.into_iter().next().unwrap())?;
    } else {
        roz::run_prompt()?;
    }

    Ok(())
}
