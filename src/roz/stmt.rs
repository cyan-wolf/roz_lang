use super::{expr::Expr, token::Token};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    DeclareVar { ident: String, init: Expr },
    Block(Vec<Stmt>),
    If { cond: Expr, then_branch: Vec<Stmt>, else_branch: Option<Vec<Stmt>> },
    While { cond: Expr, body: Vec<Stmt> },
    For { init: Box<Stmt>, cond: Expr, side_effect: Expr, body: Vec<Stmt> },
    Fun(FunDecl),
    Class { name: Token, methods: Vec<FunDecl> },
    Return { ctx: Token, ret_value: Expr },
    Break(Token),
    Continue(Token),
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    pub name: String,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}
