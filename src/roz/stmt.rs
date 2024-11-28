
use super::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    Print(Expr),
    Var(String, Expr),
}

