use std::collections::HashMap;

use crate::roz::{
    error::RuntimeError, 
    expr::Value, 
    token::{Literal, Token, TokenKind},
};

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

    pub fn retrieve(&self, token: Token) -> Result<Value, RuntimeError> {
        let ident = Self::get_ident_from_token(&token);

        self.map
            .get(ident)
            .cloned()
            .ok_or_else(|| {
                RuntimeError::new(
                    format!("undefined variable name '{token}'"),
                    token,
                )
            })
    }

    pub fn assign(&mut self, lvalue: Token, value: Value) -> Result<(), RuntimeError> {
        let ident = Self::get_ident_from_token(&lvalue);

        if let Some(entry) = self.map.get_mut(ident) {
            *entry = value;
            Ok(())
        } else {
            let err = RuntimeError::new(
                format!("undefined variable '{ident}'"),
                lvalue,
            );

            Err(err)
        }
    }

    fn get_ident_from_token(token: &Token) -> &str {
        match token.kind() {
            TokenKind::Literal(Literal::Ident(ident)) => ident,
            _ => panic!("unexpected error: '{token}' was not an identifier"),
        }
    }
}
