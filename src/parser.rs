use crate::{expr::Expr, token::{Token, TokenKind}};


pub struct Parser {
    tokens: Vec<Token>,
    curr: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            curr: 0,
        }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let expr = self.comparision();

        todo!()
    }

    fn comparision(&mut self) -> Expr {
        unimplemented!()
    }

    fn match_kind(&mut self, mut kinds: impl Iterator<Item = TokenKind>) -> bool {
        let matched = kinds.any(|k| self.check_curr(&k));

        if matched {
            self.advance();
        }
        matched
    }

    fn check_curr(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind() == kind
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.curr += 1;
        }
        self.prev().clone()
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.curr]
    }

    fn prev(&self) -> &Token {
        &self.tokens[self.curr - 1]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind(), TokenKind::Eof)
    }
}