use super::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    DeclareVar(String, Expr),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}
