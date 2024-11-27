use std::fmt::Display;
use super::token::Token;

/// Error type used in the scanner and parser.
#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
    message: String,
    ctx: String,
}

impl SyntaxError {
    pub fn new(line: usize, message: String, ctx: String) -> Self {
        Self {
            line, 
            message,
            ctx,
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Error '{}': {}", self.line, self.ctx, self.message)
    }
}

impl std::error::Error for SyntaxError {}

/// Error type used in the interpreter.
#[derive(Debug)]
pub struct RuntimeError {
    token: Token,
    message: String,
}

impl RuntimeError {
    pub fn new(message: String, token: Token) -> Self {
        Self {
            token,
            message,
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, 
            "[line {}] Runtime Error: {}", 
            self.token.line(),
            self.message,
        )
    }
}

impl std::error::Error for RuntimeError {}
