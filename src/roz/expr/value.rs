use std::fmt::Display;

use crate::roz::{stmt::Stmt, token::Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    NativeFun(NativeFun),
    Fun(Vec<Token>, Vec<Stmt>),
    Nil,
}

impl Value {
    /// Returns the type of the value as a string.
    pub fn get_type(&self) -> String {
        match self {
            Value::Num(_) => "<number>".to_owned(),
            Value::Str(_) => "<string>".to_owned(),
            Value::Bool(_) => "<boolean>".to_owned(),
            Value::NativeFun(_) => "<native fun>".to_owned(),
            Value::Fun(_, _) => "<fun>".to_owned(),
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
            Value::NativeFun(_) => write!(f, "{{native function}}"),
            Value::Fun(..) => write!(f, "{{function}}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NativeFun {
    Println,
    Clock,
}

impl NativeFun {
    pub fn arity(&self) -> usize {
        match self {
            Self::Println => 1,
            Self::Clock => 0,
        }
    }
}
