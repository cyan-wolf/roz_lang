mod context;

use std::collections::HashMap;

use context::{Context, Effect};

use super::{
    error::{ResolutionError, RozError}, 
    expr::{Expr, VarAccess}, 
    stmt::{FunDecl, Stmt}, 
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
            Stmt::Try { try_branch, catch_branch, finally_branch } => {
                self.resolve_scoped(try_branch, ctx);

                if let Some((error_name, catch_branch)) = catch_branch {
                    self.begin_scope();

                    let error_name = error_name.extract_ident().to_owned();
                    self.declare(error_name.clone());
                    self.define(error_name);

                    // Use `Resolver::resolve_statements` since we need to 
                    // manually define/declare the error's name.
                    self.resolve_statements(catch_branch, ctx);
                    self.end_scope();
                }
                
                if let Some(finally_branch) = finally_branch {
                    self.resolve_scoped(finally_branch, ctx);
                }
            },
            Stmt::Throw(_ctx_token, expr) => {
                self.resolve_expr(expr, ctx);
            },
            Stmt::Fun(fun_decl) => {
                self.resolve_fun(fun_decl, ctx);
            },
            Stmt::Class { ref name, methods, superclass } => {
                let ident = name.extract_ident().to_owned();
                self.declare(ident.clone());
                self.define(ident);

                let has_superclass = superclass.is_some();

                if let Some(superclass) = superclass {
                    // Check that a class doesn't try to inherit from itself.
                    if name.extract_ident() == superclass.lvalue.extract_ident() {
                        let err = ResolutionError::new(
                            "classes cannot inherit from themselves".to_owned(),
                            superclass.lvalue.clone(),
                        );
                        self.errs.push(err);
                    }

                    self.resolve_var_access(superclass);
                }

                // Since the class has a superclass, we know that 
                // an extra scope will be made in the interpreter 
                // to store it.
                if has_superclass {
                    self.begin_scope();

                    // Add 'super' to the newly created class scope.
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert("super".to_owned(), true);
                }

                for method_decl in methods {
                    // Begin the additional scope that contains 'me'.
                    self.begin_scope();

                    let ctx = {
                        let ctx = ctx.clone().with_effect(Effect::InMethod);

                        if has_superclass {
                            ctx.with_effect(Effect::InSubclass)
                        } else {
                            ctx
                        }
                    };

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

                // End the class' scope that contains "super".
                if has_superclass {
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
            Expr::Fun { params, body } => {
                let ctx = ctx
                    .clone()
                    .with_effect(Effect::InFunction)
                    .without_effect(&Effect::InLoop); // functions reset the loop context

                self.begin_scope();

                for param in params {
                    let ident = param.extract_ident().to_owned();
                    self.declare(ident.clone());
                    self.define(ident);
                }
        
                // No need to use `Resolver::resolve_block`, since we already
                // began a scope.
                self.resolve_statements(body, &ctx);

                self.end_scope();
            },
            Expr::Var(access) => {
                if let Some(scope) = self.scopes.last() {
                    let ident = access.lvalue.extract_ident();
                    
                    if let Some(is_init) = scope.get(ident) {
                        if !is_init {
                            let err = ResolutionError::new(
                                format!("can't read local variable '{ident}' in its own initializer"),
                                access.lvalue.clone(),
                            );
                            self.errs.push(err);
                        }
                    }
                }

                // Modify the AST to include the jump amount.
                self.resolve_var_access(access);
            },
            Expr::Assign { access, rvalue } => {
                self.resolve_expr(&mut *rvalue, ctx);

                // Modify the AST to include the jump amount.
                self.resolve_var_access(access);
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
            Expr::Me(access) => {
                // Generate an error if 'me' appears outside of a method.
                if !ctx.has_effect(&Effect::InMethod) {
                    let error = ResolutionError::new(
                        "keyword 'me' outside of method".to_owned(),
                        access.lvalue.clone(),
                    );
                    self.errs.push(error);
                }
                
                // Modify the AST to include the jump amount.
                self.resolve_var_access(access);
            },
            Expr::Super { access, property: _ } => {
                if !ctx.has_effect(&Effect::InMethod) {
                    let error = ResolutionError::new(
                        "keyword 'super' outside of method".to_owned(),
                        access.lvalue.clone(),
                    );
                    self.errs.push(error);
                }
                if !ctx.has_effect(&Effect::InSubclass) {
                    let error = ResolutionError::new(
                        "cannot use 'super' since class has no superclass".to_owned(),
                        access.lvalue.clone(),
                    );
                    self.errs.push(error);
                }

                // Modify the AST to include the jump amount.
                self.resolve_var_access(access);
            }
        }
    }

    /// Same as `Resolver::resolve_stmt`, but resolves the statements 
    /// in a new scope.
    fn resolve_scoped(&mut self, statements: &mut [Stmt], ctx: &Context) {
        self.begin_scope();
        self.resolve_statements(statements, ctx);
        self.end_scope();
    }

    /// Calculates how many scopes a variable has to jump to access its value.
    /// Note: This method modifies the AST by including the jump information
    /// in variable access nodes.
    fn resolve_var_access(&mut self, access: &mut VarAccess) {
        let VarAccess { lvalue, jumps } = access;

        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(lvalue.extract_ident()) {
                let outermost_scope_idx = self.scopes.len() - 1;
                
                // The number of scopes between the current (outermost) scope
                // and the scope where the variable was found (in this case, `i`).
                *jumps = Some(outermost_scope_idx - i);
            }
        }
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
