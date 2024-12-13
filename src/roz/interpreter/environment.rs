use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::roz::{
    error::RuntimeError, 
    expr::Value, 
    token::{Literal, Token, TokenKind},
};

/// A reference counted mutable pointer.
pub type RcCell<T> = Rc<RefCell<T>>;

#[derive(Debug)]
pub struct Environment {
    enclosing: Option<RcCell<Environment>>,
    map: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            map: HashMap::new(),
        }
    }

    pub fn with_enclosing(enclosing: RcCell<Environment>) -> Self {
        Self {
            enclosing: Some(enclosing),
            map: HashMap::new(),
        }
    }

    pub fn to_rc_cell(self) -> RcCell<Self> {
        Rc::new(RefCell::new(self))
    }

    pub fn define(&mut self, ident: String, val: Value) {
        self.map.insert(ident, val);
    }

    pub fn retrieve(&self, token: Token) -> Result<Value, RuntimeError> {
        let ident = token.extract_ident();

        if let Some(val) = self.map.get(ident) {
            Ok(val.clone())
        }
        else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow().retrieve(token)
        }
        else {
            let err = RuntimeError::new(
                format!("undefined variable name '{token}'"),
                token,
            );
            Err(err)
        }
    }

    pub fn assign(&mut self, lvalue: Token, value: Value) -> Result<(), RuntimeError> {
        let ident = lvalue.extract_ident();

        if let Some(entry) = self.map.get_mut(ident) {
            *entry = value;
            Ok(())
        }
        else if let Some(ref enclosing) = self.enclosing {
            enclosing.borrow_mut().assign(lvalue, value)
        }
        else {
            let err = RuntimeError::new(
                format!("undefined variable '{ident}'"),
                lvalue,
            );

            Err(err)
        }
    }
}
