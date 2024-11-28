use std::collections::HashMap;

use crate::roz::{error::RuntimeError, expr::Value, token::Token};

pub struct Environment {
    map: HashMap<String, Value>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn define(&mut self, ident: String, val: Value) {
        self.map.insert(ident, val);
    }

    pub fn retrieve(&self, ident: &str, ctx: Token) -> Result<Value, RuntimeError> {
        self.map
            .get(ident)
            .cloned()
            .ok_or_else(|| {
                RuntimeError::new(
                    format!("undefined variable name '{ident}'"),
                    ctx,
                )
            })
    }
}
