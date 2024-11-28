use super::error::{RozError, SyntaxError};
use super::expr::{Expr, Value};
use super::stmt::Stmt;
use super::token::{Keyword, Literal, Op, Token, TokenKind};

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

    /// Parses the given sequence of tokens.
    pub fn parse(&mut self) -> Result<Vec<Stmt>, RozError> {
        let mut statements = vec![];

        while !self.is_at_end() {
            let stmt = self.statement()
                .map_err(|err| RozError::Syntax(vec![err]))?;

            statements.push(stmt);
        }

        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, SyntaxError> {
        if self.match_any([TokenKind::Keyword(Keyword::Print)]) {
            self.statement_print()
        } else {
            self.statement_expr()
        }
    }

    fn statement_print(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.expression()?;

        if self.check_curr(&TokenKind::Op(Op::Semicolon)) {
            self.advance();
        } else {
            let token = self.peek().clone();
            let err = SyntaxError::new(
                token.line(),
                "expected ';' after value".to_owned(),
                Some(token),
            );

            return Err(err);
        }

        Ok(Stmt::Print(expr))
    }

    fn statement_expr(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.expression()?;

        self.try_match(
            &TokenKind::Op(Op::Semicolon), 
            |_| "expected ';' after value".to_owned(),
        )?;

        Ok(Stmt::Expr(expr))
    }

    /// Parses an expression.
    fn expression(&mut self) -> Result<Expr, SyntaxError> {
        self.equality()
    }

    /// Parses an equality, such as `a == b` or `a != b`.
    fn equality(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.comparision()?;

        while self.match_any([TokenKind::Op(Op::BangEq), TokenKind::Op(Op::Equality)]) {
            let op = self.prev().clone();
            let right = self.comparision()?;
            expr = Expr::Binary(expr.as_box(), op, right.as_box())
        }
        Ok(expr)
    }

    /// Parses a comparison, such as `a < b` or `a >= b`.
    fn comparision(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.term()?;

        while self.match_any([
            TokenKind::Op(Op::Greater),
            TokenKind::Op(Op::GreaterEq),
            TokenKind::Op(Op::Less),
            TokenKind::Op(Op::LessEq),
        ]) {
            let op = self.prev().clone();
            let right = self.term()?;
            expr = Expr::Binary(expr.as_box(), op, right.as_box());
        }
        Ok(expr)
    }

    /// Parses a term, such as `a + b` or `a - b`.
    fn term(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.factor()?;

        while self.match_any([TokenKind::Op(Op::Plus), TokenKind::Op(Op::Minus)]) {
            let op = self.prev().clone();
            let right = self.factor()?;
            expr = Expr::Binary(expr.as_box(), op, right.as_box());
        }
        Ok(expr)
    }

    /// Parses a factor, such as `a * b` or `a / b`.
    fn factor(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.unary()?;

        while self.match_any([TokenKind::Op(Op::Star), TokenKind::Op(Op::Slash)]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            expr = Expr::Binary(expr.as_box(), op, right.as_box());
        }
        Ok(expr)
    }

    /// Parses a unary expression, such as `-a` or `!b`.
    fn unary(&mut self) -> Result<Expr, SyntaxError> {
        if self.match_any([TokenKind::Op(Op::Minus), TokenKind::Op(Op::Bang)]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            
            let expr = Expr::Unary(op, right.as_box());
            Ok(expr)
        } else {
            self.primary()
        }
    }

    /// Parses a primary expression, such as an individual value like `"abc"` or `34.5`.
    /// Also parses a grouping expression, such as `(1 + 2 * 8)`.
    fn primary(&mut self) -> Result<Expr, SyntaxError> {
        // NOTE: Every match arm needs to call `self.advance()`, to move the 
        // parser along when it matches a token.
        let expr = match self.peek().kind() {
            TokenKind::Keyword(Keyword::False) => {
                self.advance();
                Expr::Literal(Value::Bool(false))
            },
            TokenKind::Keyword(Keyword::True) => {
                self.advance();
                Expr::Literal(Value::Bool(true))
            },
            TokenKind::Keyword(Keyword::Nil) => {
                self.advance();
                Expr::Literal(Value::Nil)
            },
            TokenKind::Literal(Literal::Num(num)) => {
                let num = *num;
                self.advance();
                Expr::Literal(Value::Num(num))
            },
            TokenKind::Literal(Literal::Str(string)) => {
                let string = string.clone();
                self.advance();
                Expr::Literal(Value::Str(string))
            },
            TokenKind::Op(Op::LeftParen) => {
                self.advance();

                let expr = self.expression()?;

                // Look for a right parentheses and consume it.
                self.try_match(
                    &TokenKind::Op(Op::RightParen), 
                    |_|  "expected ')' after expression".to_owned(),
                )?;

                Expr::Grouping(expr.as_box())
            },
            _ => {
                let token = self.peek().clone();
                let err = SyntaxError::new(
                    token.line(),
                    "expected expression".to_owned(),
                    Some(token),
                );

                return Err(err);
            },
        };

        Ok(expr)
    }

    // Called to discard possibly erroneous tokens after an error is 
    // encountered while parsing.
    fn synchronize(&mut self) {
        // Consume the current token since it was probably erroneous.
        self.advance();

        while !self.is_at_end() {
            // If the last token marked the end of a statement, stop 
            // discarding tokens.
            if self.prev().kind() == &TokenKind::Op(Op::Semicolon) {
                return;
            }

            // These keywords usually mark the start of a statement, 
            // so if they're encountered it means that tokens no longer 
            // need to be discarded.
            if let TokenKind::Keyword(ref kw) = self.peek().kind() {
                if let Keyword::Class
                    | Keyword::Fun
                    | Keyword::For
                    | Keyword::If
                    | Keyword::Print
                    | Keyword::Return
                    | Keyword::Var
                    | Keyword::While = kw 
                {
                    return;
                }
            }

            // Consume (discard) possibly erroneous tokens.
            self.advance();
        }
    }

    /// Checks whether the current token is of any of the given kinds.
    /// If there is a match, then the parser advances.
    fn match_any(&mut self, kinds: impl IntoIterator<Item = TokenKind>) -> bool {
        // NOTE: This could be made more efficient using cached `HashSet`s,
        // instead of performing linear searches on `impl IntoIterator`.
        let matched = kinds
            .into_iter()
            .any(|k| self.check_curr(&k));

        if matched {
            self.advance();
        }
        matched
    }

    /// Tries to match the current token.
    /// If the match fails, return an error with a generated error message.
    fn try_match(&mut self, kind: &TokenKind, 
        err_msg_gen: impl FnOnce(&Token) -> String) -> Result<(), SyntaxError> 
    {
        if self.check_curr(kind) {
            self.advance();

            Ok(())
        } else {
            let token = self.peek().clone();
            let err = SyntaxError::new(
                token.line(),
                err_msg_gen(&token),
                Some(token),
            );

            Err(err)
        }
    }

    /// Checks whether the current token is of the given kind.
    fn check_curr(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind() == kind
        }
    }

    /// Moves on to the next token.
    fn advance(&mut self) {
        if !self.is_at_end() {
            self.curr += 1;
        }
    }

    /// Returns the current token.
    fn peek(&self) -> &Token {
        &self.tokens[self.curr]
    }

    /// Returns the previous token.
    fn prev(&self) -> &Token {
        &self.tokens[self.curr - 1]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek().kind(), TokenKind::Eof)
    }
}