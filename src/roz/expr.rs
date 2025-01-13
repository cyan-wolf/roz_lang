pub mod value;

pub use value::Value;

use std::fmt::Display;
use super::{stmt::Stmt, token::Token};

#[derive(Debug, Clone)]
pub struct VarAccess {
    pub lvalue: Token,
    pub jumps: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Value),
    Unary { op: Token, expr: Box<Expr> },
    Binary { left: Box<Expr>, op: Token, right: Box<Expr> },
    Grouping(Box<Expr>),
    List(Vec<Expr>),
    Fun {
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Var(VarAccess),
    Assign { access: VarAccess, rvalue: Box<Expr> },
    Call { callee: Box<Expr>, args: Vec<Expr>, ctx: Token },
    Me(VarAccess),
    Super { access: VarAccess, property: Token },
    Get { source: Box<Expr>, property: Token },
    Set { source: Box<Expr>, property: Token, rvalue: Box<Expr> },
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
            Expr::List(elems) => {
                write!(f, "[")?;

                for elem in elems {
                    write!(f, "{elem} ")?;
                }

                write!(f, "]")
            },
            Expr::Fun { .. } =>  write!(f, "(fun(..) {{..}})"),
            Expr::Var(VarAccess { lvalue, .. }) => write!(f, "var({lvalue})"),
            Expr::Me { .. } => write!(f, "var(me)"),
            Expr::Super { access: _, property } => write!(f, "super.{property}"),
            Expr::Assign { access: VarAccess { lvalue, .. }, rvalue, .. } => {
                write!(f, "({lvalue} = {rvalue})")
            },
            Expr::Call { callee, args, ctx: _ } => write!(f, "({callee} calls {args:?})"),
            Expr::Get { source, property } => write!(f, "(get {source}.{property})"),
            Expr::Set { source, property, rvalue } => {
                write!(f, "(set {source}.{property} = {rvalue})")
            },
        }
    }
}


