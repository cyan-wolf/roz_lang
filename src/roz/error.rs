use std::fmt::Display;
use super::token::Token;

/// Error type used in the scanner and parser.
#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
    message: String,
    ctx: Option<Token>,
}

impl SyntaxError {
    pub fn new(line: usize, message: String, ctx: Option<Token>) -> Self {
        Self {
            line, 
            message,
            ctx,
        }
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[line {}] Syntax Error", self.line)?;

        if let Some(ref ctx) = self.ctx {
            write!(f, " at '{}'", ctx)?;
        }
        write!(f, ": {}", self.message)
    }
}

impl std::error::Error for SyntaxError {}

/// Error type used in the interpreter.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    message: String,
    token: Token,
}

impl RuntimeError {
    pub fn new(message: String, token: Token) -> Self {
        Self {
            message,
            token,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
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

#[derive(Debug)]
pub struct ResolutionError {
    message: String,
    token: Token,
}

impl ResolutionError {
    pub fn new(message: String, token: Token) -> Self {
        Self { 
            message, 
            token, 
        }
    }
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, 
            "[line {}] Resolution Error: {}", 
            self.token.line(),
            self.message,
        )
    }
}

impl std::error::Error for ResolutionError {}

/// General error type.
#[derive(Debug)]
pub enum RozError {
    Syntax(Vec<SyntaxError>),
    Resolution(Vec<ResolutionError>),
    Runtime(RuntimeError),
}

impl Display for RozError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            &RozError::Syntax(ref errs) => {
                for i in 0..errs.len() {
                    write!(f, "{}", errs[i])?;

                    if i != errs.len() - 1 {
                        write!(f, "\n")?;
                    }
                }
                Ok(())
            },
            &RozError::Resolution(ref errs) => {
                for i in 0..errs.len() {
                    write!(f, "{}", errs[i])?;

                    if i != errs.len() - 1 {
                        write!(f, "\n")?;
                    }
                }
                Ok(())
            },
            &RozError::Runtime(ref err) => {
                write!(f, "{err}")
            },
        }
    }
}
