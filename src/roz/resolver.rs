mod context;

use std::collections::HashMap;

use context::{Context, Effect};

use super::{error::{ResolutionError, RozError}, expr::Expr, stmt::{FunDecl, Stmt}, token::Token};

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
        self.resolve_scoped(statements, &mut Context::new());

        if self.errs.len() > 0 {
            return Err(RozError::Resolution(self.errs));
        }
        Ok(())
    }

    /// Resolves all the given statements without starting a new scope.
    fn resolve_statements(&mut self, statements: &mut [Stmt], ctx: &mut Context) {
        for stmt in statements {
            self.resolve_stmt(stmt, ctx);
        }
    }

    /// Resolves a single statement and any sub-statements/expressions.
    fn resolve_stmt(&mut self, stmt: &mut Stmt, ctx: &mut Context) {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr, ctx);
            },
            Stmt::Print(expr) => {
                self.resolve_expr(expr, ctx);
            },
            Stmt::DeclareVar(name, init) => {
                self.declare(name.clone());
                self.resolve_expr(init, ctx);
                self.define(name.clone());
            },
            Stmt::Block(statements) => {
                self.resolve_scoped(statements, ctx);
            },
            Stmt::If(cond, then_branch, else_branch) => {
                self.resolve_expr(cond, ctx);
                self.resolve_scoped(then_branch, ctx);
                
                if let Some(else_branch) = else_branch {
                    self.resolve_scoped(else_branch, ctx);
                }
            },
            Stmt::While(cond, body) => {
                ctx.add_effect(Effect::InLoop);
                self.resolve_expr(cond, ctx);
                self.resolve_scoped(body, ctx);
                ctx.remove_effect(&Effect::InLoop);
            },
            Stmt::For(init, cond, side_effect, for_block) => {
                // A for loop creates an extra scope compared to a while loop.
                self.begin_scope();

                ctx.add_effect(Effect::InLoop);
                self.resolve_stmt(&mut *init, ctx);
                self.resolve_expr(cond, ctx);
                self.resolve_expr(side_effect, ctx);

                self.resolve_scoped(for_block, ctx);
                ctx.remove_effect(&Effect::InLoop);

                // A for loop creates an extra scope compared to a while loop.
                self.end_scope();
            },
            Stmt::Fun(FunDecl {name, ref params, body}) => {
                self.declare(name.clone());
                self.define(name.clone());

                ctx.add_effect(Effect::InFunction);
                self.resolve_fun(params, body, ctx);
                ctx.remove_effect(&Effect::InFunction);
            },
            Stmt::Class(ref name, methods) => {
                let ident = name.extract_ident().to_owned();
                self.declare(ident.clone());
                self.define(ident);

                for decl in methods {
                    let FunDecl {name, ref params, body} = decl;
                    
                    // TODO: Find a way to avoid having to make this code be 
                    // copy-pasted from the Stmt::Fun(..) branch.
                    self.declare(name.clone());
                    self.define(name.clone());

                    ctx.add_effect(Effect::InFunction);
                    self.resolve_fun(params, body, ctx);
                    ctx.remove_effect(&Effect::InFunction);
                }
            },
            Stmt::Return(token, expr) => {
                // If a return statement is found and it is outside of a function,
                // then generate an error.
                if !ctx.has_effect(&Effect::InFunction) {
                    let error = ResolutionError::new(
                        "return statement outside of function".to_owned(),
                        token.clone(),
                    );
                    self.errs.push(error);
                }
                self.resolve_expr(expr, ctx);
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
    fn resolve_expr(&mut self, expr: &mut Expr, ctx: &mut Context) {
        match expr {
            Expr::Literal(_value) => {
                // Do nothing, since a literal value
                // does not contain subexpressions.
            },
            Expr::Unary(_, expr) => {
                self.resolve_expr(&mut *expr, ctx);
            },
            Expr::Binary(left, _, right) => {
                self.resolve_expr(&mut *left, ctx);
                self.resolve_expr(&mut *right, ctx);
            },
            Expr::Grouping(expr) => {
                self.resolve_expr(&mut *expr, ctx);
            },
            Expr::Var(ref ident_token, jumps) => {
                if let Some(scope) = self.scopes.last() {
                    let ident = ident_token.extract_ident();
                    
                    if let Some(is_init) = scope.get(ident) {
                        if !is_init {
                            let err = ResolutionError::new(
                                format!("can't read local variable '{ident}' in its own initializer"),
                                ident_token.clone(),
                            );
                            self.errs.push(err);
                        }
                    }
                }

                // Modify the AST to include the jump amount.
                *jumps = self.resolve_variable(ident_token);
            },
            Expr::Assign(ref lvalue, rvalue, jumps) => {
                self.resolve_expr(&mut *rvalue, ctx);

                // Modify the AST to include the jump amount.
                *jumps = self.resolve_variable(lvalue);
            },
            Expr::Call(callee, args, _) => {
                self.resolve_expr(&mut *callee, ctx);
                for arg in args {
                    self.resolve_expr(arg, ctx);
                }
            },
        }
    }

    /// Same as `Resolver::resolve_stmt`, but resolves the statements 
    /// in a new scope.
    fn resolve_scoped(&mut self, statements: &mut [Stmt], ctx: &mut Context) {
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

    /// Resolve a function and any sub-statements.
    fn resolve_fun(&mut self, params: &[Token], body: &mut [Stmt], ctx: &mut Context) {
        self.begin_scope();

        for param in params {
            let ident = param.extract_ident().to_owned();
            self.declare(ident.clone());
            self.define(ident);
        }
        
        // No need to use `Resolver::resolve_block`, since we already
        // began a scope in this function (`Resolver::resolve_fun`).
        self.resolve_statements(body, ctx);
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
