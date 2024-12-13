use super::{expr::Expr, token::Token};

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    DeclareVar(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Vec<Stmt>, Option<Vec<Stmt>>),
    While(Expr, Vec<Stmt>),
    For(Box<Stmt>, Expr, Expr, Vec<Stmt>),
    Fun(String, Vec<Token>, Vec<Stmt>),
    Return(Token, Expr),
}
