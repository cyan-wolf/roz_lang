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
            Stmt::Expr(expr) => {
                self.resolve_expr(expr);
            },
            Stmt::Print(expr) => {
                self.resolve_expr(expr);
            },
            Stmt::DeclareVar(name, init) => {
                self.declare(name.clone());
                self.resolve_expr(init);
                self.define(name);
            },
            Stmt::Block(statements) => {
                self.resolve_block(statements);
            },
            Stmt::If(cond, then_branch, else_branch) => {
                self.resolve_expr(cond);
                self.resolve_block(then_branch);
                
                if let Some(else_branch) = else_branch {
                    self.resolve_block(else_branch);
                }
            },
            Stmt::While(cond, body) => {
                self.resolve_expr(cond);
                self.resolve_block(body);
            },
            Stmt::For(init, cond, side_effect, for_block) => {
                self.resolve_stmt(*init);
                self.resolve_expr(cond);
                self.resolve_expr(side_effect);

                self.resolve_block(for_block);
            },
            Stmt::Fun(name, params, body) => {
                self.declare(name.clone());
                self.define(name);

                self.resolve_fun(params, body);
            },
            Stmt::Return(_, expr) => {
                self.resolve_expr(expr);
            },
        }
    }

    fn resolve_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Literal(_value) => {
                // Do nothing, since a literal value
                // does not contain subexpressions.
            },
            Expr::Unary(_, expr) => {
                self.resolve_expr(*expr);
            },
            Expr::Binary(left, _, right) => {
                self.resolve_expr(*left);
                self.resolve_expr(*right);
            },
            Expr::Grouping(expr) => {
                self.resolve_expr(*expr);
            },
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
            Expr::Assign(lvalue, rvalue) => {
                self.resolve_expr(*rvalue);
                self.resolve_local(lvalue);
            },
            Expr::Call(callee, args, _) => {
                self.resolve_expr(*callee);
                for arg in args {
                    self.resolve_expr(arg);
                }
            },
        }
    }

    /// Same as `Resolver::resolve_stmt`, but resolves the statements 
    /// in a new scope.
    fn resolve_block(&mut self, statements: Vec<Stmt>) {
        self.begin_scope();
        self.resolve(statements);
        self.end_scope();
    }

    /// Look for the variable references by `ident` starting from the 
    /// outermost scope and checking all the way until the most general scope.
    /// Stops looking just before it reaches the global scope.
    fn resolve_local(&mut self, ident: Token) {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(ident.extract_ident()) {
                let outermost_scope_idx = self.scopes.len() - 1;
                
                // The number of scopes between the current (outermost) scope
                // and the scope where the variable was found (in this case, `i`).
                let scope_jumps = outermost_scope_idx - i;
                //self.interpreter.resolve(Expr::Var(ident), ...)
                todo!();

                // Stop searching since the variable was found.
                return;
            }
        }
    }

    fn resolve_fun(&mut self, params: Vec<Token>, body: Vec<Stmt>) {
        self.begin_scope();

        for param in params {
            let ident = param.extract_ident().to_owned();
            self.declare(ident.clone());
            self.define(ident);
        }
        
        // No need to use `Resolver::resolve_block`, since we already
        // began a scope in this function (`Resolver::resolve_fun`).
        self.resolve(body);
        self.end_scope();
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
