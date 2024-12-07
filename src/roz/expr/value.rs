use std::fmt::Display;

use crate::roz::{error::RuntimeError, interpreter::Interpreter, token::Token};

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
    Println,
    Clock,
}

impl From<NativeFn> for Callable {
    fn from(native_fn: NativeFn) -> Self {
        match native_fn {
            NativeFn::Println => {
                let func = |_intepreter: &mut _, args: Vec<_>, _ctx| {
                    let arg = args.into_iter().next().unwrap();
                    println!("{arg}");
                    Ok(Value::Nil)
                };

                Callable::new(1, Box::new(func))
            },
            NativeFn::Clock => {
                use std::time;

                let func = |_interpreter: &mut _, _args, ctx| {
                    let now = time::SystemTime::now();

                    let elapsed = now.duration_since(time::UNIX_EPOCH)
                        .map_err(|_| {
                            RuntimeError::new(
                                "system time before Unix Epoch".to_owned(), 
                                ctx,
                            )
                        })?
                        .as_millis();

                    Ok(Value::Num((elapsed as f64) / 1000.0))
                };

                Callable::new(0, Box::new(func))
            },
        }
    }
}

/// A function callable from the interpreter itself.
pub type RuntimeCallable = 
    Box<dyn for<'a> Fn(&'a mut Interpreter, Vec<Value>, Token) -> Result<Value, RuntimeError>>;

pub struct Callable {
    arity: usize,
    func: RuntimeCallable,
}

impl Callable {
    pub fn new(arity: usize, func: RuntimeCallable) -> Self {
        Self {
            arity,
            func,
        }
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn into_runtime_callable(self) -> RuntimeCallable {
        self.func
    }
}