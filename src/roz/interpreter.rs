mod environment;

use std::rc::Rc;

use environment::{Environment, RcCell};

use super::expr::{Expr, Value};
use super::stmt::Stmt;
use super::token::{Keyword, Op, TokenKind};
use super::error::{RozError, RuntimeError};

pub struct Interpreter {
    curr_env: RcCell<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            curr_env: Environment::new().to_rc_cell(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RozError> {
        for stmt in statements {
            self.execute(stmt)
                .map_err(RozError::Runtime)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.evaluate(expr)?; // value is discarded
            },
            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                println!("{val}");
            },
            Stmt::DeclareVar(ident, init) => {
                let init = self.evaluate(init)?;

                self.curr_env
                    .borrow_mut()
                    .define(ident, init);
            },
            Stmt::Block(statements) => {
                let new_env = Environment::with_enclosing(
                    Rc::clone(&self.curr_env),
                ).to_rc_cell();

                self.execute_block(
                    statements, 
                    new_env,
                )?;
            },
            Stmt::If(cond, branch_then, branch_else) => {
                let cond = self.evaluate(cond)?.to_bool();

                if cond {
                    self.execute(*branch_then)?;
                } 
                else if let Some(branch_else) = branch_else {
                    self.execute(*branch_else)?;
                }
            },
        }

        Ok(())
    }

    fn execute_block(&mut self, statements: Vec<Stmt>, new_env: RcCell<Environment>) -> Result<(), RuntimeError> {
        // Set the current environment to be the new one.
        let prev_env = Rc::clone(&self.curr_env);
        self.curr_env = new_env;

        // Execute the block's statements under this new environment, 
        // and collect their result.
        let result: Result<(), RuntimeError> = statements.into_iter()
            .map(|stmt| self.execute(stmt))
            .collect();

        // Reset the current environment.
        self.curr_env = prev_env;

        // Return the result after executing the statements.
        result
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(value) => Ok(value),
            Expr::Unary(token, expr) => {
                match token.kind() {
                    &TokenKind::Op(Op::Minus) => {
                        let operand = self.evaluate(*expr)?;

                        if let Value::Num(num) = operand {
                            Ok(Value::Num(-num))
                        } else {
                            let err = RuntimeError::new(
                                format!("operand `{operand}` was not a number"),
                                token, 
                            );
                            Err(err)
                        }
                    },
                    &TokenKind::Op(Op::Bang) => {
                        let operand = self.evaluate(*expr)?;

                        // Coerces any operand into a boolean before 
                        // applying the not operator.
                        Ok(Value::Bool(!operand.to_bool()))
                    },
                    &TokenKind::Keyword(Keyword::TypeOf) => {
                        let operand = self.evaluate(*expr)?;

                        Ok(Value::Str(operand.get_type()))
                    },
                    _ => {
                        let err = RuntimeError::new(
                            format!("unknown unary operator `{token}`"),
                            token, 
                        );
                        Err(err)
                    },
                }
            },
            Expr::Binary(expr1, token, expr2) => {
                match token.kind() {
                    &TokenKind::Op(Op::Plus) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Num(num1 + num2))
                            },
                            (Value::Str(str1), val) => {
                                Ok(Value::Str(format!("{str1}{val}")))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Minus) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Num(num1 - num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Star) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Num(num1 * num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Slash) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                if num2 == 0.0 {
                                    let err = RuntimeError::new(
                                        "division by zero".to_owned(),
                                        token,
                                    );
                                    return Err(err);
                                }

                                Ok(Value::Num(num1 / num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Less) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Bool(num1 < num2))
                            },
                            (Value::Str(str1), Value::Str(str2)) => {
                                Ok(Value::Bool(str1 < str2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::LessEq) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Bool(num1 <= num2))
                            },
                            (Value::Str(str1), Value::Str(str2)) => {
                                Ok(Value::Bool(str1 <= str2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Greater) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Bool(num1 > num2))
                            },
                            (Value::Str(str1), Value::Str(str2)) => {
                                Ok(Value::Bool(str1 > str2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::GreaterEq) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Bool(num1 >= num2))
                            },
                            (Value::Str(str1), Value::Str(str2)) => {
                                Ok(Value::Bool(str1 >= str2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        token, op1.get_type(), op2.get_type(),
                                    ),
                                    token, 
                                );
                                Err(err)
                            },
                        }
                    },
                    &TokenKind::Op(Op::Equality) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        Ok(Value::Bool(op1 == op2))
                    },
                    &TokenKind::Op(Op::BangEq) => {
                        let op1 = self.evaluate(*expr1)?;
                        let op2 = self.evaluate(*expr2)?;

                        Ok(Value::Bool(op1 != op2))
                    },
                    // Evaluates to the "truthy" value, if present.
                    &TokenKind::Keyword(Keyword::Or) => {
                        let op1 = self.evaluate(*expr1)?;

                        if !op1.to_bool() {
                            let op2 = self.evaluate(*expr2)?;
                            Ok(op2)
                        } else {
                            Ok(op1)
                        }
                    },
                    // Evaluates to the "falsey" value, if present.
                    &TokenKind::Keyword(Keyword::And) => {
                        let op1 = self.evaluate(*expr1)?;

                        if op1.to_bool() {
                            let op2 = self.evaluate(*expr2)?;
                            Ok(op2)
                        } else {
                            Ok(op1)
                        }
                    },
                    _ => {
                        let err = RuntimeError::new(
                            format!("unknown binary operator `{token}`"),
                            token, 
                        );
                        Err(err)
                    },
                }
            },
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Var(ident) => {
                self.curr_env
                    .borrow()
                    .retrieve(ident)
            },
            Expr::Assign(lvalue, expr) => {
                let rvalue = self.evaluate(*expr)?;

                self.curr_env
                    .borrow_mut()
                    .assign(lvalue, rvalue.clone())?;

                Ok(rvalue)
            },
        }
    }
}
