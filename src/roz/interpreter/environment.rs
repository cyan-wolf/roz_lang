use std::collections::HashMap;

use crate::roz::{error::RuntimeError, expr::Value, token::{Literal, Token, TokenKind}};

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

    pub fn assign(&self, lvalue: Token, value: Value) -> Result<(), RuntimeError> {
        if let TokenKind::Literal(Literal::Ident(ident)) = lvalue.kind() {
            if self.map.contains_key(ident) {
                unimplemented!()
            } else {
                unimplemented!()
            }
        } else {
            panic!("unexpected error: left hand side of assignment was not a valid location");
        }
    }
}
