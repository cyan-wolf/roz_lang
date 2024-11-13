use crate::token::{Token, TokenKind};
use crate::error::Error;

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    loc: Location,
}

struct Location {
    start: u32,
    current: u32,
    line: u32,
}

impl Scanner {
    pub fn new(content: Vec<char>) -> Self {
        Scanner {
            source: content,
            tokens: vec![],
            loc: Location {
                start: 0,
                current: 0,
                line: 1,
            },
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.loc.start = self.loc.current;
            self.scan_token()?;
        }
        
        self.tokens.push(Token::new(TokenKind::Eof, self.loc.line));
        
        Ok(self.tokens)
    }

    pub fn scan_token(&mut self) -> Result<(), Error> {
        // TODO
        Ok(())
    }

    fn is_at_end(&self) -> bool {
        self.loc.current >= self.source.len() as u32
    }
}
