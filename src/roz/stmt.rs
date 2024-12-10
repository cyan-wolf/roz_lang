use super::{expr::Expr, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    DeclareVar(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Box<Stmt>, Expr, Expr, Box<Stmt>),
    Fun(String, Vec<Token>, Box<Stmt>),
    Return(Token, Expr),
}
