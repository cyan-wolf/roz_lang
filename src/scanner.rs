use crate::token::Token;
use crate::error::Error;

// TODO
pub struct Scanner;

// TODO
impl Scanner {
    pub fn new(content: Vec<char>) -> Self {
        Scanner
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Error> {
        Ok(vec![])
    }
}
