use std::fmt::Display;

use crate::roz::interpreter::Interpreter;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Num(f64),
    Str(String),
    Bool(bool),
    NativeFn(NativeFn),
    Nil,
}

impl Value {
    /// Returns the type of the value as a string.
    pub fn get_type(&self) -> String {
        match self {
            Value::Num(_) => "<number>".to_owned(),
            Value::Str(_) => "<string>".to_owned(),
            Value::Bool(_) => "<boolean>".to_owned(),
            Value::NativeFn(_) => "<native fn>".to_owned(),
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

    pub fn is_callable(&self) -> bool {
        match self {
            Value::NativeFn(_) => true,
            _ => false,
        }
    }

    pub fn into_callable(self) -> Option<Callable> {
        match self {
            Value::NativeFn(native_fn) => Some(native_fn.into()),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &*self {
            Value::Num(num) => write!(f, "{num}"),
            Value::Str(str) => write!(f, "{str}"),
            Value::Bool(bool) => write!(f, "{bool}"),
            Value::NativeFn(_) => write!(f, "{{native function}}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum NativeFn {
    Clock,
}

impl From<NativeFn> for Callable {
    fn from(native_fn: NativeFn) -> Self {
        match native_fn {
            NativeFn::Clock => {
                let func = |interpreter, args| {
                    let now = std::time::Instant::now();
                    
                    unimplemented!()
                };

                Callable::new(0, Box::new(func))
            },
        }
    }
}

type CallableFunc = Box<dyn Fn(Interpreter, Vec<Value>) -> Value>;

pub struct Callable {
    arity: usize,
    func: CallableFunc,
}

impl Callable {
    pub fn new(arity: usize, func: CallableFunc) -> Self {
        Self {
            arity,
            func,
        }
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn into_call(self) -> CallableFunc {
        self.func
    }
}