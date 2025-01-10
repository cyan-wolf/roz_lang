use super::error::{RozError, SyntaxError};
use super::expr::{Expr, Value};
use super::stmt::{FunDecl, Stmt};
use super::token::{Keyword, Literal, Op, Token, TokenKind};
use super::util::ToBox;

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
        let mut errors = vec![];

        while !self.is_at_end() {
            match self.statement() {
                Ok(stmt) => {
                    statements.push(stmt);
                },
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                },
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(RozError::Syntax(errors))
        }
    }

    /// Parses a statement.
    fn statement(&mut self) -> Result<Stmt, SyntaxError> {
        if self.match_any([TokenKind::Keyword(Keyword::Var)]) {
            self.statement_var_decl()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Fun)]) {
            let stmt = Stmt::Fun(self.fn_decl()?);
            Ok(stmt)
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Class)]) {
            self.statement_class_decl()
        }
        else if self.match_any([TokenKind::Op(Op::LeftBrace)]) {
            let statements = self.statement_block()?;
            Ok(Stmt::Block(statements))
        }
        else if self.match_any([TokenKind::Keyword(Keyword::If)]) {
            self.statement_if()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::While)]) {
            self.statement_while()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::For)]) {
            self.statement_for()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Try)]) {
            self.statement_try()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Throw)]) {
            self.statement_throw()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Return)]) {
            self.statement_return()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Break)]) {
            self.statement_break()
        }
        else if self.match_any([TokenKind::Keyword(Keyword::Continue)]) {
            self.statement_continue()
        }
        else {
            self.statement_expr()
        }
    }

    /// Parses a statement which is just an expression followed by a semicolon.
    fn statement_expr(&mut self) -> Result<Stmt, SyntaxError> {
        let expr = self.expression()?;

        self.try_match(
            &TokenKind::Op(Op::Semicolon), 
            |_| "expected ';' after value".to_owned(),
        )?;

        Ok(Stmt::Expr(expr))
    }

    /// Parses a variable declaration, i.e. `var <name> = <expr>;`.
    fn statement_var_decl(&mut self) -> Result<Stmt, SyntaxError> {
        match self.peek().kind() {
            TokenKind::Literal(Literal::Ident(ident)) => {
                let ident = ident.clone();
                self.advance();

                let init = if self.match_any([TokenKind::Op(Op::Eq)]) {
                    self.expression()?
                } else {
                    Expr::Literal(Value::Nil)
                };

                self.try_match(
                    &TokenKind::Op(Op::Semicolon), 
                    |_| "expected ';' after declaration".to_owned(),
                )?;

                Ok(Stmt::DeclareVar { ident, init })
            },
            _ => {
                let token = self.peek().clone();

                let err = SyntaxError::new(
                    token.line(),
                    "expected variable name".to_owned(),
                    Some(token),
                );

                Err(err)
            },
        }
    }

    /// Parses a block statement, which is just a series of statements
    /// wrapped in braces `{}`.
    fn statement_block(&mut self) -> Result<Vec<Stmt>, SyntaxError> {
        let mut statements = vec![];

        while !self.check_curr(&TokenKind::Op(Op::RightBrace)) 
            && !self.is_at_end()
        {
            statements.push(self.statement()?);
        }

        self.try_match(
            &TokenKind::Op(Op::RightBrace),
            |_| "expected '}' after block".to_owned(),
        )?;
        
        Ok(statements)
    }

    /// Parses an if statement.
    fn statement_if(&mut self) -> Result<Stmt, SyntaxError> {
        let cond = self.expression()?;

        // Match a '{' before parsing the 'then' block.
        self.try_match(
            &TokenKind::Op(Op::LeftBrace),
            |_| "expected '{' after if condition".to_owned(),
        )?;
        let then_branch = self.statement_block()?;

        let else_branch = if self.match_any([
            TokenKind::Keyword(Keyword::Else),
        ]) {
            // Match a '{' before parsing the 'else' block.
            self.try_match(
                &TokenKind::Op(Op::LeftBrace),
                |_| "expected '{' after else".to_owned(),
            )?;
            let block_else = self.statement_block()?;

            Some(block_else)
        } else {
            None
        };

        let stmt = Stmt::If { cond, then_branch, else_branch };
        Ok(stmt)
    }

    fn statement_while(&mut self) -> Result<Stmt, SyntaxError> {
        let cond = self.expression()?;

        // Match a '{' before parsing the 'while' block.
        self.try_match(
            &TokenKind::Op(Op::LeftBrace),
            |_| "expected '{' after 'while' condition".to_owned(),
        )?;
        let body = self.statement_block()?;

        let stmt = Stmt::While { cond, body };
        Ok(stmt)
    }

    fn statement_for(&mut self) -> Result<Stmt, SyntaxError> {
        let init = if self.match_any([TokenKind::Keyword(Keyword::Var)]) {
            self.statement_var_decl()?
        } else {
            self.statement_expr()?
        };

        let cond = self.expression()?;
        // Match a ';' before parsing the next part of the 'for'.
        self.try_match(
            &TokenKind::Op(Op::Semicolon),
            |_| "expected ';' after 'for' condition".to_owned(),
        )?;

        let side_effect = self.expression()?;

        // Match a '{' before parsing the 'for' block.
        self.try_match(
            &TokenKind::Op(Op::LeftBrace),
            |_| "expected '{' after 'for' updater".to_owned(),
        )?;
        let body = self.statement_block()?;

        let stmt = Stmt::For {
            init: init.to_box(), 
            cond,
            side_effect,
            body,
        };

        Ok(stmt)
    }

    fn statement_try(&mut self) -> Result<Stmt, SyntaxError> {
        // Match a '{' before parsing the 'try' block.
        self.try_match(
            &TokenKind::Op(Op::LeftBrace),
            |_| "expected '{' after 'try'".to_owned(),
        )?;
        let try_branch = self.statement_block()?;

        let catch_branch = if self.match_any([
            TokenKind::Keyword(Keyword::Catch),
        ]) {
            let catch_error_name = if let TokenKind::Literal(Literal::Ident(..)) = self.peek().kind() {
                self.advance();
                self.prev().clone()
            } else {
                let token = self.peek().clone();
                let err = SyntaxError::new(
                    token.line(),
                    "expected error name".to_owned(),
                    Some(token),
                );
                return Err(err);
            };

            // Match a '{' before parsing the 'catch' block.
            self.try_match(
                &TokenKind::Op(Op::LeftBrace),
                |_| "expected '{' after catch".to_owned(),
            )?;
            let block_catch = self.statement_block()?;

            Some((catch_error_name, block_catch))
        } else {
            None
        };

        let finally_branch = if self.match_any([
            TokenKind::Keyword(Keyword::Finally),
        ]) {
            // Match a '{' before parsing the 'finally' block.
            self.try_match(
                &TokenKind::Op(Op::LeftBrace),
                |_| "expected '{' after finally".to_owned(),
            )?;
            let block_finally = self.statement_block()?;

            Some(block_finally)
        } else {
            None
        };

        let stmt = Stmt::Try { try_branch, catch_branch, finally_branch };
        Ok(stmt)
    }

    fn statement_throw(&mut self) -> Result<Stmt, SyntaxError> {
        let throw_keyword = self.prev().clone();
        let expr = self.expression()?;

        // Match a ';' to terminate the statement.
        self.try_match(
            &TokenKind::Op(Op::Semicolon),
            |_| "expected ';' after 'throw'".to_owned(),
        )?;

        let stmt = Stmt::Throw(throw_keyword, expr);
        Ok(stmt)
    }

    fn fn_decl(&mut self) -> Result<FunDecl, SyntaxError> {
        if let TokenKind::Literal(Literal::Ident(ident)) = self.peek().kind() {
            let name = ident.clone();
            self.advance();

            self.try_match(
                &TokenKind::Op(Op::LeftParen),
                |_| "expected '(' after function name".to_owned(),
            )?;
            let params = self.parameters()?;

            self.try_match(
                &TokenKind::Op(Op::RightParen),
                |_| "expected ')'".to_owned(),
            )?;

            self.try_match(
                &TokenKind::Op(Op::LeftBrace),
                |_| "expected '{' after parameter list".to_owned(),
            )?;
            let body = self.statement_block()?;

            let decl = FunDecl {name, params, body};
            Ok(decl)
        } 
        else {
            let token = self.peek().clone();
            let err = SyntaxError::new(
                token.line(),
                "expected identifier".to_owned(),
                Some(token),
            );

            Err(err)
        }
    }

    fn statement_class_decl(&mut self) -> Result<Stmt, SyntaxError> {
        let name = if let TokenKind::Literal(Literal::Ident(..)) = self.peek().kind() {
            self.advance();
            self.prev().clone()
        } else {
            let token = self.peek().clone();
            let err = SyntaxError::new(
                token.line(),
                "expected class name".to_owned(),
                Some(token),
            );
            return Err(err);
        };

        self.try_match(
            &TokenKind::Op(Op::LeftBrace),
            |_| "expected '{' after class name".to_owned(),
        )?;

        let mut methods = vec![];

        while !self.check_curr(&TokenKind::Op(Op::RightBrace)) && !self.is_at_end() {
            methods.push(self.fn_decl()?);
        }

        self.try_match(
            &TokenKind::Op(Op::RightBrace),
            |_| "expected '}' after class body".to_owned(),
        )?;
        
        let stmt = Stmt::Class { name, methods };
        Ok(stmt)
    }

    fn statement_return(&mut self) -> Result<Stmt, SyntaxError> {
        let keyword = self.prev().clone();

        let ret_value = if !self.check_curr(&TokenKind::Op(Op::Semicolon)) {
            self.expression()?
        } else {
            Expr::Literal(Value::Nil)
        };

        self.try_match(
            &TokenKind::Op(Op::Semicolon),
            |_| "expected ';' after return value".to_owned(), 
        )?;

        Ok(Stmt::Return { ctx: keyword, ret_value })
    }

    fn statement_break(&mut self) -> Result<Stmt, SyntaxError> {
        let keyword = self.prev().clone();

        self.try_match(
            &TokenKind::Op(Op::Semicolon),
            |_| "expected ';' after 'break'".to_owned(), 
        )?;

        Ok(Stmt::Break(keyword))
    }

    fn statement_continue(&mut self) -> Result<Stmt, SyntaxError> {
        let keyword = self.prev().clone();

        self.try_match(
            &TokenKind::Op(Op::Semicolon),
            |_| "expected ';' after 'continue'".to_owned(), 
        )?;

        Ok(Stmt::Continue(keyword))
    }

    /// Parses an expression.
    fn expression(&mut self) -> Result<Expr, SyntaxError> {
        self.assignment()
    }

    /// Parses an assignment expression.
    fn assignment(&mut self) -> Result<Expr, SyntaxError> {
        let expr = self.logic_or()?;

        if self.match_any([TokenKind::Op(Op::Eq)]) {
            let equals = self.prev().clone();
            let rvalue = self.assignment()?.to_box();

            // If the left hand side of the '=' was a simple variable access,
            // then the whole expression is a simple assignment.
            if let Expr::Var { lvalue, .. } = expr {
                // The jumps are set later in the resolver.
                let assign_expr = Expr::Assign { lvalue, rvalue, jumps: None };
                Ok(assign_expr)
            }
            // If the left hand side of the '=' was a property access 
            // (a "get" expression), then the whole expression is a "set" expression.
            else if let Expr::Get { source , property } = expr {
                let set_expr = Expr::Set { source, property, rvalue };
                Ok(set_expr)
            }
            else {
                let err = SyntaxError::new(
                    equals.line(),
                    "invalid assignment target".to_owned(),
                    Some(equals),
                );

                Err(err)
            }
        } else {
            Ok(expr)
        }
    }

    fn logic_or(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.logic_and()?;

        while self.match_any([TokenKind::Keyword(Keyword::Or)]) {
            let op = self.prev().clone();
            let right = self.logic_and()?;

            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }
        
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.equality()?;

        while self.match_any([TokenKind::Keyword(Keyword::And)]) {
            let op = self.prev().clone();
            let right = self.equality()?;

            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }
        
        Ok(expr)
    }

    /// Parses an equality, such as `a == b` or `a != b`.
    fn equality(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.comparision()?;

        while self.match_any([TokenKind::Op(Op::BangEq), TokenKind::Op(Op::Equality)]) {
            let op = self.prev().clone();
            let right = self.comparision()?;
            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
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
            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }
        Ok(expr)
    }

    /// Parses a term, such as `a + b` or `a - b`.
    fn term(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.mod_div_expr()?;

        while self.match_any([TokenKind::Op(Op::Plus), TokenKind::Op(Op::Minus)]) {
            let op = self.prev().clone();
            let right = self.mod_div_expr()?;
            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }
        Ok(expr)
    }

    /// Parses a 'mod' or a 'div' expression, such as `a mod b` or `a div b`.
    fn mod_div_expr(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.factor()?;

        while self.match_any([TokenKind::Keyword(Keyword::Mod), TokenKind::Keyword(Keyword::Div)]) {
            let op = self.prev().clone();
            let right = self.factor()?;
            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }

        Ok(expr)
    }

    /// Parses a factor, such as `a * b` or `a / b`.
    fn factor(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.unary()?;

        while self.match_any([TokenKind::Op(Op::Star), TokenKind::Op(Op::Slash)]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            expr = Expr::Binary { left: expr.to_box(), op, right: right.to_box() };
        }
        Ok(expr)
    }

    /// Parses a unary expression, such as `-a` or `!b`.
    fn unary(&mut self) -> Result<Expr, SyntaxError> {
        if self.match_any([
            TokenKind::Op(Op::Minus), 
            TokenKind::Op(Op::Bang), 
            TokenKind::Keyword(Keyword::TypeOf),
        ]) {
            let op = self.prev().clone();
            let right = self.unary()?;
            
            let expr = Expr::Unary { op, expr: right.to_box() };
            Ok(expr)
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, SyntaxError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_any([TokenKind::Op(Op::LeftParen)]) {
                let paren = self.prev().clone();
                let args = self.arguments()?;
                
                self.try_match(
                    &TokenKind::Op(Op::RightParen), 
                    |_| "expected ')'".to_owned(),
                )?;

                expr = Expr::Call {callee: expr.to_box(), args, ctx: paren };
            }
            else if self.match_any([TokenKind::Op(Op::Dot)]) {
                if let TokenKind::Literal(Literal::Ident(..)) = self.peek().kind() {
                    self.advance();
                    let ident = self.prev().clone();

                    expr = Expr::Get { source: expr.to_box(), property: ident };
                } else {
                    let token = self.peek().clone();
                    let error = SyntaxError::new(
                        token.line(),
                        "expected property name after '.'".to_owned(),
                        Some(token),
                    );
                    break Err(error);
                }
            }
            else {
                break Ok(expr);
            }
        }
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut args = vec![];

        // The parser doesn't match (advance) the ')' since `Parser::call` already does so.
        if self.check_curr(&TokenKind::Op(Op::RightParen)) {
            return Ok(args);
        }

        loop {
            args.push(self.expression()?);

            if args.len() > 255 {
                let ctx = self.peek().clone();
                let err = SyntaxError::new(
                    ctx.line(),
                    "cannot have more than 255 arguments".to_owned(),
                        Some(ctx),
                );

                break Err(err);
            }

            if !self.match_any([TokenKind::Op(Op::Comma)]) {
                break Ok(args);
            }
        }
    }

    fn parameters(&mut self) -> Result<Vec<Token>, SyntaxError> { 
        let mut params = vec![];

        // The parser doesn't match (advance) the ')' since 
        // `Parser::statement_fn_decl` already does so.
        if self.check_curr(&TokenKind::Op(Op::RightParen)) {
            return Ok(params);
        }

        loop {
            // Parse an individual parameter.
            if let TokenKind::Literal(Literal::Ident(..)) = self.peek().kind() {
                self.advance();

                let ident = self.prev().clone();
                params.push(ident);
            } else {
                let token = self.peek().clone();
                let err = SyntaxError::new(
                    token.line(),
                    "expected parameter".to_owned(),
                    Some(token),
                );
                break Err(err);
            }

            if params.len() > 255 {
                let ctx = self.peek().clone();
                let err = SyntaxError::new(
                    ctx.line(),
                    "cannot have more than 255 parameters".to_owned(),
                        Some(ctx),
                );

                break Err(err);
            }

            if !self.match_any([TokenKind::Op(Op::Comma)]) {
                break Ok(params);
            }
        }
    }

    fn list_elements(&mut self) -> Result<Vec<Expr>, SyntaxError> {
        let mut elems = vec![];

        // The parser doesn't match (advance) the ']' since `Parser::primary` already does so.
        if self.check_curr(&TokenKind::Op(Op::RightBracket)) {
            return Ok(elems);
        }

        loop {
            elems.push(self.expression()?);

            if elems.len() > 255 {
                let ctx = self.peek().clone();
                let err = SyntaxError::new(
                    ctx.line(),
                    "list literal cannot have more than 255 elements".to_owned(),
                        Some(ctx),
                );

                break Err(err);
            }

            if !self.match_any([TokenKind::Op(Op::Comma)]) {
                break Ok(elems);
            }
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
            TokenKind::Literal(Literal::Ident(_)) => {
                self.advance();
                // Set the jumps to None, since those are set later in the resolver.
                Expr::Var { lvalue: self.prev().clone(), jumps: None }
            },
            TokenKind::Keyword(Keyword::Me) => {
                self.advance();
                Expr::Me { ctx: self.prev().clone() }
            },
            TokenKind::Op(Op::LeftParen) => {
                self.advance();

                let expr = self.expression()?;

                // Look for a right parentheses and consume it.
                self.try_match(
                    &TokenKind::Op(Op::RightParen), 
                    |_|  "expected ')' after expression".to_owned(),
                )?;

                Expr::Grouping(expr.to_box())
            },
            TokenKind::Op(Op::LeftBracket) => {
                self.advance();

                let elems = self.list_elements()?;

                // Look for a ']' and consume it.
                self.try_match(
                    &TokenKind::Op(Op::RightBracket), 
                    |_|  "expected ']' after expression".to_owned(),
                )?;

                Expr::List(elems)
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