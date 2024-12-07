pub mod value;

pub use value::Value;

use std::fmt::Display;
use super::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Var(Token),
    Assign(Token, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>, Token),
}

impl Expr {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            Expr::Literal(value) => write!(f, "{value}"),
            Expr::Unary(token, expr) => write!(f, "({token} {expr})"),
            Expr::Binary(expr1, token, expr2) => {
                write!(f, "({token} {expr1} {expr2})")
            },
            Expr::Grouping(expr) => write!(f, "(group {expr})"),
            Expr::Var(ident) => write!(f, "var({ident})"),
            Expr::Assign(lvalue, expr) => write!(f, "({lvalue} = {expr})"),
            Expr::Call(callee, args, _) => write!(f, "({callee} calls {args:?})"),
        }
    }
}


