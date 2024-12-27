mod class;
mod fun;

pub use fun::Fun;
pub use class::{Class, Instance};

use std::fmt::Display;
use std::rc::Rc;

use crate::roz::util::RcCell;

#[derive(Debug, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    List(RcCell<Vec<Value>>),
    NativeFun(NativeFun),
    NativeMethod(NativeMethod),
    Fun(Fun),
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
            Value::List(_) => "<list>".to_owned(),
            Value::NativeFun(_) => "<native fun>".to_owned(),
            Value::NativeMethod(_) => "<native method>".to_owned(),
            Value::Fun(..) => "<fun>".to_owned(),
            Value::Class(..) => "<class>".to_owned(),
            Value::Instance(instance) => {
                format!(
                    "<instance of {class_name}>",
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
            (Value::List(a), Value::List(b)) => {
                if a.borrow().len() != b.borrow().len() {
                    return false;
                }
                let contents_are_equal = a.borrow().iter()
                    .zip(b.borrow().iter())
                    .all(|(elem_a, elem_b)| elem_a.equals(elem_b));

                contents_are_equal
            },
            (Value::NativeFun(a), Value::NativeFun(b)) => a == b,
            (Value::Instance(inst1), Value::Instance(inst2)) => {
                // Two instances are equal if they point to the same allocation.
                Rc::ptr_eq(inst1, inst2)
            },
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
            Value::List(elements) => {
                write!(f, "[")?;

                for i in 0..elements.borrow().len() {
                    let elem = &elements.borrow()[i];
                    write!(f, "{elem}")?;

                    if i != elements.borrow().len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            },
            Value::NativeFun(native_fun) => {
                write!(f, "{{native function {native_fun}}}")
            },
            Value::NativeMethod(native_method) => {
                write!(f, "{{native method {native_method}}}")
            },
            Value::Fun(Fun { name, .. }) => {
                if let Some(name) = name {
                    write!(f, "{{function {name}}}")
                } else {
                    write!(f, "{{function}}")
                }
            },
            Value::Class(Class { name, .. }) => write!(f, "{{class {name}}}"),
            Value::Instance(instance) => {
                let class_name = instance.borrow()
                    .class()
                    .name
                    .to_owned();

                write!(f, "{{instance of {class_name}}}")
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
            Self::Println => write!(f, "println"),
            Self::Clock => write!(f, "clock"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum NativeMethod {
    StrLength(String),
    ListLength(RcCell<Vec<Value>>),
    ListGet(RcCell<Vec<Value>>),
    ListSet(RcCell<Vec<Value>>),
    ListClone(RcCell<Vec<Value>>),
    ListSort(RcCell<Vec<Value>>),
}

impl NativeMethod {
    pub fn arity(&self) -> usize {
        match self {
            Self::StrLength(..) => 0,
            Self::ListLength(..) => 0,
            Self::ListGet(..) => 1,
            Self::ListSet(..) => 2,
            Self::ListClone(..) => 0,
            Self::ListSort(..) => 0,
        }
    }
}

impl Display for NativeMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::StrLength(..) => write!(f, "length"),
            Self::ListLength(..) => write!(f, "length"),
            Self::ListGet(..) => write!(f, "get"),
            Self::ListSet(..) => write!(f, "set"),
            Self::ListClone(..) => write!(f, "clone"),
            Self::ListSort(..) => write!(f, "sort"),
        }
    }
}