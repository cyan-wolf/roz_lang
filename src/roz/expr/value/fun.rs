use std::rc::Rc;

use crate::roz::token::Token;
use crate::roz::stmt::Stmt;
use crate::roz::interpreter::Environment;
use crate::roz::util::{RcCell, ToRcCell};

use super::Value;

#[derive(Debug, Clone)]
/// Runtime representation of a function.
pub struct Fun {
    pub name: Option<String>,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub env: RcCell<Environment>,
}

impl Fun {
    /// Modifies the runtime function so that the given value is bound to the 
    /// identifier `me` within the method's environment.
    pub fn bind(&mut self, value: Value) {
        let mut new_env = Environment::with_enclosing(
            Rc::clone(&self.env)
        );
        new_env.define("me".to_owned(), value);
        self.env = new_env.to_rc_cell();
    }
}
