use super::expr::{Expr, Value};
use super::token::{Op, TokenKind};
use super::error::RuntimeError;

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&mut self, expr: Expr) -> Result<(), RuntimeError> {
        self.evaluate(expr)
            .map(|val| println!("{val}"))
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
                            (Value::Str(str1), Value::Str(str2)) => {
                                Ok(Value::Str(format!("{str1}{str2}")))
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
        }
    }
}
