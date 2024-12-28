mod context;

use std::collections::HashMap;

use context::{Context, Effect};

use super::{
    error::{ResolutionError, RozError}, 
    expr::Expr, 
    stmt::{FunDecl, Stmt}, 
    token::Token,
};

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    errs: Vec<ResolutionError>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![],
            errs: vec![],
        }
    }

    /// Performs a resolution pass on a programs AST 
    /// (represented as a slice of statements).
    /// Note: The AST is mutated after resolution and may be in an invalid 
    /// state if this method returns a `Result::Err`.
    pub fn resolve_program(mut self, statements: &mut [Stmt]) -> Result<(), RozError> {
        // Treat the entire program as if it were a block.
        // This is to account for the global scope.
        self.resolve_scoped(statements, &Context::new());

        if self.errs.len() > 0 {
            return Err(RozError::Resolution(self.errs));
        }
        Ok(())
    }

    /// Resolves all the given statements without starting a new scope.
    fn resolve_statements(&mut self, statements: &mut [Stmt], ctx: &Context) {
        for stmt in statements {
            self.resolve_stmt(stmt, ctx);
        }
    }

    /// Resolves a single statement and any sub-statements/expressions.
    fn resolve_stmt(&mut self, stmt: &mut Stmt, ctx: &Context) {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr, ctx);
            },
            Stmt::DeclareVar { ident, init } => {
                self.declare(ident.clone());
                self.resolve_expr(init, ctx);
                self.define(ident.clone());
            },
            Stmt::Block(statements) => {
                self.resolve_scoped(statements, ctx);
            },
            Stmt::If { cond, then_branch, else_branch} => {
                self.resolve_expr(cond, ctx);
                self.resolve_scoped(then_branch, ctx);
                
                if let Some(else_branch) = else_branch {
                    self.resolve_scoped(else_branch, ctx);
                }
            },
            Stmt::While { cond, body } => {
                let ctx = ctx.clone().with_effect(Effect::InLoop);

                self.resolve_expr(cond, &ctx);
                self.resolve_scoped(body, &ctx);
            },
            Stmt::For { init, cond, side_effect, body } => {
                // A for loop creates an extra scope compared to a while loop.
                self.begin_scope();

                let ctx = ctx.clone().with_effect(Effect::InLoop);

                self.resolve_stmt(&mut *init, &ctx);
                self.resolve_expr(cond, &ctx);
                self.resolve_expr(side_effect, &ctx);

                self.resolve_scoped(body, &ctx);

                // A for loop creates an extra scope compared to a while loop.
                self.end_scope();
            },
            Stmt::Fun(fun_decl) => {
                self.resolve_fun(fun_decl, ctx);
            },
            Stmt::Class { ref name, methods } => {
                let ident = name.extract_ident().to_owned();
                self.declare(ident.clone());
                self.define(ident);

                for method_decl in methods {
                    // Begin the additional scope that contains 'me'.
                    self.begin_scope();

                    let ctx = ctx.clone().with_effect(Effect::InMethod);

                    // Add 'me' to the current scope.
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert("me".to_owned(), true);

                    // Resolve the method.
                    self.resolve_fun(method_decl, &ctx);

                    // End the additional scope that contains 'me'.
                    self.end_scope();
                }
            },
            Stmt::Return { ctx: ret_token, ret_value } => {
                // If a return statement is found and it is outside of a function,
                // then generate an error.
                if !ctx.has_effect(&Effect::InFunction) {
                    let error = ResolutionError::new(
                        "return statement outside of function".to_owned(),
                        ret_token.clone(),
                    );
                    self.errs.push(error);
                }
                self.resolve_expr(ret_value, ctx);
            },
            Stmt::Break(token) => {
                if !ctx.has_effect(&Effect::InLoop) {
                    let error = ResolutionError::new(
                        "break statement outside of loop".to_owned(),
                        token.clone(),
                    );
                    self.errs.push(error);
                }
            },
            Stmt::Continue(token) => {
                if !ctx.has_effect(&Effect::InLoop) {
                    let error = ResolutionError::new(
                        "continue statement outside of loop".to_owned(),
                        token.clone(),
                    );
                    self.errs.push(error);
                }
            },
        }
    }

    /// Resolves a single expression and any sub-expressions.
    fn resolve_expr(&mut self, expr: &mut Expr, ctx: &Context) {
        match expr {
            Expr::Literal(_value) => {
                // Do nothing, since a literal value
                // does not contain subexpressions.
            },
            Expr::Unary { op: _, expr } => {
                self.resolve_expr(&mut *expr, ctx);
            },
            Expr::Binary { left, op: _, right } => {
                self.resolve_expr(&mut *left, ctx);
                self.resolve_expr(&mut *right, ctx);
            },
            Expr::Grouping(expr) => {
                self.resolve_expr(&mut *expr, ctx);
            },
            Expr::List(elements) => {
                for elem in elements {
                    self.resolve_expr(elem, ctx);
                }
            },
            Expr::Var { ref lvalue, jumps } => {
                if let Some(scope) = self.scopes.last() {
                    let ident = lvalue.extract_ident();
                    
                    if let Some(is_init) = scope.get(ident) {
                        if !is_init {
                            let err = ResolutionError::new(
                                format!("can't read local variable '{ident}' in its own initializer"),
                                lvalue.clone(),
                            );
                            self.errs.push(err);
                        }
                    }
                }

                // Modify the AST to include the jump amount.
                *jumps = self.resolve_variable(lvalue);
            },
            Expr::Assign {ref lvalue, rvalue, jumps } => {
                self.resolve_expr(&mut *rvalue, ctx);

                // Modify the AST to include the jump amount.
                *jumps = self.resolve_variable(lvalue);
            },
            Expr::Call { callee, args, ctx: _ } => {
                self.resolve_expr(&mut *callee, ctx);
                for arg in args {
                    self.resolve_expr(arg, ctx);
                }
            },
            Expr::Get { source, property: _ } => {
                self.resolve_expr(source, ctx);
            },
            Expr::Set { source, property: _, rvalue } => {
                self.resolve_expr(source, ctx);
                self.resolve_expr(rvalue, ctx);

            },
            Expr::Me { ctx: ref ctx_token } => {
                // Generate an error if 'me' appears outside of a method.
                if !ctx.has_effect(&Effect::InMethod) {
                    let error = ResolutionError::new(
                        "keyword 'me' outside of method".to_owned(),
                        ctx_token.clone(),
                    );
                    self.errs.push(error);
                }

                // Change the current expression into an `Expr::Var`
                // before resolving.
                *expr = Expr::Var { lvalue: ctx_token.clone(), jumps: None };
                
                // This delegates to the `Expr::Var` resolution algorithm.
                self.resolve_expr(expr, ctx);
            },
        }
    }

    /// Same as `Resolver::resolve_stmt`, but resolves the statements 
    /// in a new scope.
    fn resolve_scoped(&mut self, statements: &mut [Stmt], ctx: &Context) {
        self.begin_scope();
        self.resolve_statements(statements, ctx);
        self.end_scope();
    }

    /// Look for the variable references by `ident` starting from the 
    /// outermost scope and checking all the way until the most general scope.
    /// Used for modifying the AST.
    fn resolve_variable(&mut self, ident: &Token) -> Option<usize> {
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(ident.extract_ident()) {
                let outermost_scope_idx = self.scopes.len() - 1;
                
                // The number of scopes between the current (outermost) scope
                // and the scope where the variable was found (in this case, `i`).
                return Some(outermost_scope_idx - i);
            }
        }
        return None; // unreachable?
    }

    /// Resolve a function/method and any sub-statements.
    fn resolve_fun(&mut self, fun_decl: &mut FunDecl, ctx: &Context) {
        let FunDecl { ref name, ref params, body } = fun_decl;
        
        let ctx = ctx
            .clone()
            .with_effect(Effect::InFunction)
            .without_effect(&Effect::InLoop); // functions reset the loop context

        self.declare(name.clone());
        self.define(name.clone());

        self.begin_scope();

        for param in params {
            let ident = param.extract_ident().to_owned();
            self.declare(ident.clone());
            self.define(ident);
        }
        
        // No need to use `Resolver::resolve_block`, since we already
        // began a scope in this function (`Resolver::resolve_fun`).
        self.resolve_statements(body, &ctx);
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
