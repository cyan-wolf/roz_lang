use std::fmt::Display;
use super::token::Token;

pub enum Expr {
    Literal(Value),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    // For retrieving values in variables.
    // Contains the identifier and a token for context.
    Var(String, Token),
    // For assigning values to existing variables.
    Assign(Token, Box<Expr>),
}

impl Expr {
    pub fn as_box(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    Nil,
}

impl Value {
    /// Returns the type of the value as a string.
    pub fn get_type(&self) -> String {
        match self {
            Value::Num(_) => "<number>".to_owned(),
            Value::Str(_) => "<string>".to_owned(),
            Value::Bool(_) => "<boolean>".to_owned(),
            Value::Nil => "<nil>".to_owned(),
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            Value::Bool(bool) => *bool,
            Value::Nil => false,
            _ => true,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            Value::Num(num) => write!(f, "{num}"),
            Value::Str(str) => write!(f, "{str}"),
            Value::Bool(bool) => write!(f, "{bool}"),
            Value::Nil => write!(f, "nil"),
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
            Expr::Var(ident, _) => write!(f, "var({ident})"),
            Expr::Assign(lvalue, expr) => write!(f, "({lvalue} = {expr})"),
        }
    }
}


