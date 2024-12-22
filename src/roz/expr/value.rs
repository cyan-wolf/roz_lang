mod class;

pub use class::{Class, Instance};

use std::fmt::Display;

use crate::roz::{
    stmt::Stmt, 
    token::Token,
    interpreter::{Environment, RcCell},
};



#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    NativeFun(NativeFun),
    Fun(Option<String>, Vec<Token>, Vec<Stmt>, RcCell<Environment>),
    Class(Class),
    Instance(RcCell<Instance>),
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
            Value::Fun(..) => "<fun>".to_owned(),
            Value::Class(..) => "<class>".to_owned(),
            Value::Instance(instance) => {
                format!(
                    "<instanceof {class_name}>",
                    class_name = &instance.borrow().class().name,
                )
            },
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

    /// Compares two objects for general equality.
    /// Two values are equal if their type and contents are the same.
    /// Note: User defined functions cannot be compared, comparisons 
    /// between them are always false.
    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::NativeFun(a), Value::NativeFun(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            Value::Num(num) => write!(f, "{num}"),
            Value::Str(str) => write!(f, "{str}"),
            Value::Bool(bool) => write!(f, "{bool}"),
            Value::NativeFun(native_fun) => {
                write!(f, "{{native function {native_fun}}}")
            },
            Value::Fun(name, ..) => {
                if let Some(name) = name {
                    write!(f, "{{function {name}}}")
                } else {
                    write!(f, "{{function}}")
                }
            },
            Value::Class(Class { name, .. }) => write!(f, "{{class {name}}}"),
            Value::Instance(instance) => {
                todo!()
            },
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

impl Display for NativeFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NativeFun::Println => write!(f, "println"),
            NativeFun::Clock => write!(f, "clock"),
        }
    }
}
