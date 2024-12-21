pub mod value;

pub use value::Value;

use std::fmt::Display;
use super::token::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Unary { op: Token, expr: Box<Expr> },
    Binary { left: Box<Expr>, op: Token, right: Box<Expr> },
    Grouping(Box<Expr>),
    Var { lvalue: Token, jumps: Option<usize> },
    Assign { lvalue: Token, rvalue: Box<Expr>, jumps: Option<usize> },
    Call {callee: Box<Expr>, args: Vec<Expr>, ctx: Token },
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
            Expr::Unary { op, expr } => write!(f, "({op} {expr})"),
            Expr::Binary { left, op, right } => {
                write!(f, "({op} {left} {right})")
            },
            Expr::Grouping(expr) => write!(f, "(group {expr})"),
            Expr::Var { lvalue, .. } => write!(f, "var({lvalue})"),
            Expr::Assign { lvalue, rvalue, .. } => write!(f, "({lvalue} = {rvalue})"),
            Expr::Call { callee, args, ctx: _ } => write!(f, "({callee} calls {args:?})"),
        }
    }
}


