use std::fmt::Display;
use crate::token::Token;

pub enum Expr {
    Literal(Value),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
}

pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            Value::Num(num) => write!(f, "{num}"),
            Value::Str(str) => write!(f, "\"{str}\""),
            Value::Bool(bool) => write!(f, "{bool}"),
        }
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
        }
    }
}

impl Expr {
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}
