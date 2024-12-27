use std::{cell::RefCell, rc::Rc};

use super::{expr::{value::Instance, Expr, Value}, interpreter::Environment, stmt::Stmt};

// Utility trait for boxing an AST node.
pub trait ToBox {
    fn to_box(self) -> Box<Self>;
}

macro_rules! impl_to_box {
    ($for_type:ty) => {
        impl ToBox for $for_type {
            /// Heap-allocates the given value.
            fn to_box(self) -> Box<Self> {
                Box::new(self)
            }
        }
    };
}

// Implementations of `ToBox`.
impl_to_box!(Expr);
impl_to_box!(Stmt);

/// A reference counted mutable pointer.
pub type RcCell<T> = Rc<RefCell<T>>;

/// Utility trait for creating reference counted 
/// mutable pointers.
pub trait ToRcCell {
    fn to_rc_cell(self) -> RcCell<Self>;
}

macro_rules! impl_to_rc_cell {
    ($for_type:ty) => {
        impl ToRcCell for $for_type {
            /// Creates a reference counted mutable pointer
            /// to the given value.
            fn to_rc_cell(self) -> RcCell<Self> {
                Rc::new(RefCell::new(self))
            }
        }
    };
}

// Implement `ToRcCell`.
impl_to_rc_cell!(Environment);
impl_to_rc_cell!(Instance);
impl_to_rc_cell!(Vec<Value>);