use std::fmt::Display;

// Main error type.
#[derive(Debug)]
pub struct Error {
    line: usize,
    message: String,
    ctx: String,
}

impl Error {
    pub fn new(line: usize, message: String, ctx: String) -> Self {
        Self {
            line, 
            message,
            ctx,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error '{}': {}", self.line, self.ctx, self.message)
    }
}

impl std::error::Error for Error {}
