use std::collections::HashMap;

use crate::roz::{error::RuntimeError, token::Token};

use super::{Fun, Value};

#[derive(Debug, Clone)]
/// Runtime representation of a class.
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Fun>,
}

/// Runtime representation of a class instance.
#[derive(Debug, Clone)]
pub struct Instance {
    class: Class,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Class, fields: HashMap<String, Value>) -> Self {
        Self {
            class,
            fields,
        }
    }

    pub fn class(&self) -> &Class {
        &self.class
    }

    pub fn access(&self, property: Token) -> Result<Value, RuntimeError> {
        if let Some(field) = self.fields.get(property.extract_ident()) {
            Ok(field.clone())
        }
        else {
            let err = RuntimeError::new(
                format!("property '{property}' not found on instance"),
                property,
            );
            Err(err)        
        }
    }

    pub fn set(&mut self, property: Token, value: Value) -> Result<Value, RuntimeError> {
        self.fields.insert(
            property.extract_ident().to_owned(),
            value.clone(),
        );
        Ok(value)
    }
}