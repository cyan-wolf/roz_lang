use std::{collections::HashMap, rc::Rc};

use crate::roz::{error::RuntimeError, token::Token, util::RcCell};

use super::{Fun, Value};

#[derive(Debug, Clone)]
/// Runtime representation of a class.
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Fun>,
}

impl Class {
    pub fn find_method(&self, method_ident: &Token) -> Result<Fun, RuntimeError> {
        let method_name = method_ident.extract_ident();
        if let Some(method) = self.methods.get(method_name) {
            Ok(method.clone())
        }
        else {
            let error = RuntimeError::new(
                format!(
                    "method '{method_name}' not found on class '{class_name}'", 
                    class_name=self.name
                ),
                method_ident.clone(),
            );
            Err(error)
        }
    }
}

/// Runtime representation of a class instance.
#[derive(Debug)]
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

    pub fn access(this: RcCell<Self>, property: Token) -> Result<Value, RuntimeError> {
        if let Some(field) = this.borrow().fields.get(property.extract_ident()) {
            Ok(field.clone())
        }
        else if let Ok(mut method) = this.borrow().class.find_method(&property) {
            // Bind the method with this instance.
            method.bind(Value::Instance(Rc::clone(&this)));

            Ok(Value::Fun(method))
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