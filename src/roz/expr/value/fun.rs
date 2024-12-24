use crate::roz::token::Token;
use crate::roz::stmt::Stmt;
use crate::roz::interpreter::Environment;
use crate::roz::util::RcCell;

#[derive(Debug, Clone)]
/// Runtime representation of a function.
pub struct Fun {
    pub name: Option<String>,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub env: RcCell<Environment>,
}
