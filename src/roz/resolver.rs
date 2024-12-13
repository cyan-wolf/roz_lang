use std::collections::HashMap;

use super::{expr::Expr, interpreter::Interpreter, stmt::Stmt, token::Token};

pub struct Resolver {
    interpreter: Interpreter,
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
        }
    }

    fn resolve(&mut self, statements: Vec<Stmt>) {
        for stmt in statements {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => todo!(),
            Stmt::Print(expr) => todo!(),
            Stmt::DeclareVar(name, init) => {
                self.declare(name.clone());
                self.resolve_expr(init);
                self.define(name);
            },
            Stmt::Block(vec) => todo!(),
            Stmt::If(expr, vec, vec1) => todo!(),
            Stmt::While(expr, vec) => todo!(),
            Stmt::For(stmt, expr, expr1, vec) => todo!(),
            Stmt::Fun(_, vec, vec1) => todo!(),
            Stmt::Return(token, expr) => todo!(),
        }
    }

    fn resolve_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(value) => todo!(),
            Expr::Unary(token, expr) => todo!(),
            Expr::Binary(expr, token, expr1) => todo!(),
            Expr::Grouping(expr) => todo!(),
            Expr::Var(ident_token) => {
                if let Some(scope) = self.scopes.last() {
                    let ident = ident_token.extract_ident();
                    
                    if let Some(is_init) = scope.get(ident) {
                        if !is_init {
                            unimplemented!("can't read local variable in its own initializer")
                        }
                    }
                }

                self.resolve_local(ident_token);
            },
            Expr::Assign(token, expr) => todo!(),
            Expr::Call(expr, vec, token) => todo!(),
        }
    }

    fn resolve_local(&mut self, ident: Token) {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(ident.extract_ident()) {
                //self.interpreter.resolve(Expr::Var(ident), ...)
                todo!()
            }
        }

        todo!()
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) {
        // Add the variable to the scope, but mark it 
        // as `false` meaning that it hasn't been initialized yet.
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, false);
        }
    }

    fn define(&mut self, name: String) {
        // The variable, in theory, should be in the scope. Here 
        // it is marked as `true` to denote that its initializer has run.
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, true);
        }
    }
}
