pub mod environment;

pub use environment::Environment;

use core::f64;
use std::collections::HashMap;
use std::io::{BufRead, BufWriter};
use std::rc::Rc;
use super::expr::value::{Class, Fun, Instance, NativeFun, NativeMethod};
use super::expr::{Expr, Value};
use super::stmt::{FunDecl, Stmt};
use super::token::{Keyword, Op, Token, TokenKind};
use super::error::{RozError, RuntimeError};
use super::util::{RcCell, ToRcCell};

// Models runtime errors and other special control flow
// constructs (such as return, break, continue, etc.).
pub enum RuntimeOutcome {
    Error(RuntimeError),
    Return(Value),
    Break,
    Continue,
}

impl From<RuntimeError> for RuntimeOutcome {
    fn from(err: RuntimeError) -> Self {
        Self::Error(err)
    }
}

pub struct Interpreter {
    curr_env: RcCell<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            curr_env: Interpreter::globals().to_rc_cell(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), RozError> {
        for stmt in statements {
            match self.execute(stmt) {
                Err(RuntimeOutcome::Error(err)) => {
                    // Propogate `RuntimeError`s.
                    return Err(RozError::Runtime(err));
                },
                Err(RuntimeOutcome::Return(..)) => {
                    panic!("unexpected error: top-level return");
                },
                Err(RuntimeOutcome::Break) => {
                    panic!("unexpected error: top-level break");
                },
                Err(RuntimeOutcome::Continue) => {
                    panic!("unexpected error: top-level continue");
                },
                Ok(_) => {},
            }
        }

        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), RuntimeOutcome> {
        match stmt {
            Stmt::Expr(expr) => {
                let _ = self.evaluate(expr)?; // value is discarded
            },
            Stmt::DeclareVar { ident, init } => {
                let init = self.evaluate(init)?;

                self.curr_env
                    .borrow_mut()
                    .define(ident, init);
            },
            Stmt::Block(statements) => {
                self.execute_scoped(statements, self.new_env_with_enclosing())?;
            },
            Stmt::If { cond, then_branch, else_branch } => {
                let cond = self.evaluate(cond)?.to_bool();

                if cond {
                    self.execute_scoped(then_branch, self.new_env_with_enclosing())?;
                } 
                else if let Some(branch_else) = else_branch {
                    self.execute_scoped(branch_else, self.new_env_with_enclosing())?;
                }
            },
            Stmt::While { cond, body } => {
                while self.evaluate(cond.clone())?.to_bool() {
                    let outcome = self.execute_scoped(
                        body.clone(), 
                        self.new_env_with_enclosing(),
                    );

                    // Handle any break or continue statements that were in the body.
                    match outcome {
                        Err(RuntimeOutcome::Break) => {
                            break;
                        },
                        Err(RuntimeOutcome::Continue) => {
                            continue;
                        },
                        // Propogate othwer cases (such as a return statement, 
                        // or a runtime error).
                        otherwise => otherwise?,
                    }
                }
            },
            Stmt::For { init, cond, side_effect, body: body_for } => {
                // The body of the 'while' loop is the same as the 'for' 
                // loop body, except that is also runs the "side effect" afterwards.
                let body_while = body_for.into_iter()
                    .chain(std::iter::once(Stmt::Expr(side_effect)))
                    .collect();

                // Transform the for loop into an equivalent while loop.
                // A block is made to limit the scope of the initializer.
                let stmt = Stmt::Block(vec![
                    *init,
                    Stmt::While {
                        cond,
                        body: body_while,
                    },
                ]);
                
                self.execute(stmt)?;
            },
            Stmt::Fun(FunDecl {name, params, body}) => {
                // Make the function object. Stores a reference to the 
                // current environment (at definition time), which allows 
                // the implementation of closures.
                let fun = Value::Fun(Fun {
                    name: Some(name.clone()), 
                    params, 
                    body, 
                    env: Rc::clone(&self.curr_env),
                });

                self.curr_env
                    .borrow_mut()
                    .define(name, fun);
            },
            Stmt::Class { name, methods } => {
                let name = name.extract_ident();

                // Convert the method declarations into function values.
                let method_fun_vals: HashMap<_, _> = methods.into_iter()
                    .map(|method| {
                        (
                            method.name.clone(),
                            Fun {
                                name: Some(method.name), 
                                params: method.params, 
                                body: method.body, 
                                env: Rc::clone(&self.curr_env),
                            },
                        )
                    })
                    .collect();

                let class = Value::Class(Class { 
                    name: name.to_owned(), 
                    methods: method_fun_vals,
                });

                self.curr_env
                    .borrow_mut()
                    .define(name.to_owned(), class);
            },
            Stmt::Return { ctx: _, ret_value } => {
                let val = self.evaluate(ret_value)?;

                return Err(RuntimeOutcome::Return(val));
            },
            Stmt::Break(_) => {
                return Err(RuntimeOutcome::Break);
            }
            Stmt::Continue(_) => {
                return Err(RuntimeOutcome::Continue);
            }
        }

        Ok(())
    }

    fn execute_scoped(&mut self, statements: Vec<Stmt>, 
        new_env: RcCell<Environment>) -> Result<(), RuntimeOutcome> 
    {
        // Set the current environment to be the new one.
        let prev_env = Rc::clone(&self.curr_env);
        self.curr_env = new_env;

        // Execute the block's statements under this new environment, 
        // and collect their result.
        let result: Result<(), RuntimeOutcome> = statements.into_iter()
            .map(|stmt| self.execute(stmt))
            .collect();

        // Reset the current environment.
        self.curr_env = prev_env;

        // Return the result after executing the statements.
        result
    }

    fn evaluate(&mut self, expr: Expr) -> Result<Value, RuntimeOutcome> {
        match expr {
            Expr::Literal(value) => Ok(value),
            Expr::Unary { op, expr } => {
                match op.kind() {
                    &TokenKind::Op(Op::Minus) => {
                        let operand = self.evaluate(*expr)?;

                        if let Value::Num(num) = operand {
                            Ok(Value::Num(-num))
                        } else {
                            let err = RuntimeError::new(
                                format!("operand `{operand}` was not a number"),
                                op, 
                            );
                            Err(err)?
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
                            format!("unknown unary operator `{op}`"),
                            op, 
                        );
                        Err(err)?
                    },
                }
            },
            Expr::Binary { left, op, right } => {
                match op.kind() {
                    &TokenKind::Op(Op::Plus) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

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
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Minus) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Num(num1 - num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Star) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                Ok(Value::Num(num1 * num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Slash) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                if num2 == 0.0 {
                                    let err = RuntimeError::new(
                                        "division by zero".to_owned(),
                                        op,
                                    );
                                    return Err(err)?;
                                }

                                Ok(Value::Num(num1 / num2))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Keyword(Keyword::Mod) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                if num2 == 0.0 {
                                    let err = RuntimeError::new(
                                        "modulo zero".to_owned(),
                                        op,
                                    );
                                    return Err(err)?;
                                }

                                Ok(Value::Num(num1.rem_euclid(num2)))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Keyword(Keyword::Div) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        match (op1, op2) {
                            (Value::Num(num1), Value::Num(num2)) => {
                                if num2 == 0.0 {
                                    let err = RuntimeError::new(
                                        "division by zero".to_owned(),
                                        op,
                                    );
                                    return Err(err)?;
                                }

                                Ok(Value::Num(num1.div_euclid(num2)))
                            },
                            (op1, op2) => {
                                let err = RuntimeError::new(
                                    format!(
                                        "cannot apply {} to operands of types {} and {}",
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Less) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

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
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::LessEq) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

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
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Greater) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

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
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::GreaterEq) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

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
                                        op, op1.get_type(), op2.get_type(),
                                    ),
                                    op, 
                                );
                                Err(err)?
                            },
                        }
                    },
                    &TokenKind::Op(Op::Equality) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        Ok(Value::Bool(op1 == op2))
                    },
                    &TokenKind::Op(Op::BangEq) => {
                        let op1 = self.evaluate(*left)?;
                        let op2 = self.evaluate(*right)?;

                        Ok(Value::Bool(op1 != op2))
                    },
                    // Evaluates to the "truthy" value, if present.
                    &TokenKind::Keyword(Keyword::Or) => {
                        let op1 = self.evaluate(*left)?;

                        if !op1.to_bool() {
                            let op2 = self.evaluate(*right)?;
                            Ok(op2)
                        } else {
                            Ok(op1)
                        }
                    },
                    // Evaluates to the "falsey" value, if present.
                    &TokenKind::Keyword(Keyword::And) => {
                        let op1 = self.evaluate(*left)?;

                        if op1.to_bool() {
                            let op2 = self.evaluate(*right)?;
                            Ok(op2)
                        } else {
                            Ok(op1)
                        }
                    },
                    _ => {
                        let err = RuntimeError::new(
                            format!("unknown binary operator `{op}`"),
                            op, 
                        );
                        Err(err)?
                    },
                }
            },
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::List(exprs) => {
                let elems = exprs.into_iter()
                    .map(|expr| self.evaluate(expr))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Value::List(elems.to_rc_cell()))
            },
            Expr::Var { lvalue, jumps } => {
                let actual_env = self.find_actual_env(jumps);
                let val = actual_env
                    .borrow()
                    .retrieve(lvalue)?;

                Ok(val)
            },
            Expr::Assign { lvalue, rvalue, jumps } => {
                let rvalue = self.evaluate(*rvalue)?;

                let actual_env = self.find_actual_env(jumps);
                actual_env
                    .borrow_mut()
                    .assign(lvalue, rvalue.clone())?;

                Ok(rvalue)
            },
            Expr::Call { callee, args, ctx } => {
                let callee = self.evaluate(*callee)?;

                let args = args.into_iter()
                    .map(|arg| self.evaluate(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                self.try_call_value(callee, args, ctx)
            },
            Expr::Get { source, property } => {
                let source = self.evaluate(*source)?;

                match source {
                    Value::Instance(instance) => {
                        let val = Instance::access(instance, property)?;
                        Ok(val)
                    },
                    _ => {
                        let type_ = source.get_type();

                        self.find_builtin_property(source, &property)
                            .ok_or_else(|| {
                                let err = RuntimeError::new(
                                    format!("property '{property}' not found on type {type_}"),
                                    property,
                                );
                                RuntimeOutcome::Error(err)
                            })
                    },
                }
            },
            Expr::Set { source, property, rvalue } => {
                let source = self.evaluate(*source)?;

                match source {
                    Value::Instance(instance) => {
                        let value = self.evaluate(*rvalue)?;
                        let val = instance.borrow_mut().set(property, value)?;
                        Ok(val)
                    },
                    _ => {
                        let type_ = source.get_type();

                        let err = RuntimeError::new(
                            format!("value of type {type_} cannot be accessed"),
                            property,
                        );
                        Err(RuntimeOutcome::Error(err))
                    },
                }
            },
            Expr::Me { ctx: _ } => {
                // Note: 'me' expressions get resolved to `Expr::Var` expressions
                // before reaching the interpreter.
                unreachable!("unexpected error: unresolved keyword 'me'")
            },
        }
    }

    /// Tries to find the given property on a built-in type. Returns `None` if not found.
    fn find_builtin_property(&mut self, value: Value, property: &Token) -> Option<Value> {
        let method = match value {
            Value::Str(string) => {
                match property.extract_ident() {
                    "length" => Value::NativeMethod(NativeMethod::StrLength(string)),
                    _ => {
                        return None;
                    },
                }
            },
            Value::List(list) => {
                match property.extract_ident() {
                    "length" => Value::NativeMethod(NativeMethod::ListLength(list)),
                    "get" => Value::NativeMethod(NativeMethod::ListGet(list)),
                    "set" => Value::NativeMethod(NativeMethod::ListSet(list)),
                    "clone" => Value::NativeMethod(NativeMethod::ListClone(list)),
                    "sort" => Value::NativeMethod(NativeMethod::ListSort(list)),
                    "push" => Value::NativeMethod(NativeMethod::ListPush(list)),
                    "pop" => Value::NativeMethod(NativeMethod::ListPop(list)),
                    _ => return None,
                }
            },
            Value::Map(map) => {
                match property.extract_ident() {
                    "set" => Value::NativeMethod(NativeMethod::MapSet(map)),
                    "has" => Value::NativeMethod(NativeMethod::MapHas(map)),
                    "remove" => Value::NativeMethod(NativeMethod::MapRemove(map)),
                    "clone" => Value::NativeMethod(NativeMethod::MapClone(map)),
                    "keys" => Value::NativeMethod(NativeMethod::MapKeys(map)),
                    _ => return None,
                }
            }
            Value::Namespace(name) => {
                match &*name {
                    "io" => {
                        match property.extract_ident() {
                            "readLines" => Value::NativeFun(NativeFun::IOReadLines),
                            "readString" => Value::NativeFun(NativeFun::IOReadString),
                            "writeLines" => Value::NativeFun(NativeFun::IOWriteLines),
                            "writeString" => Value::NativeFun(NativeFun::IOWriteString),
                            "makeDir" => Value::NativeFun(NativeFun::IOMakeDir),
                            "exists" => Value::NativeFun(NativeFun::IOExists),
                            _ => return None,
                        }
                    },
                    "math" => {
                        match property.extract_ident() {
                            "sin" => Value::NativeFun(NativeFun::MathSin),
                            "cos" => Value::NativeFun(NativeFun::MathCos),
                            "tan" => Value::NativeFun(NativeFun::MathTan),
                            "pow" => Value::NativeFun(NativeFun::MathPow),
                            "sqrt" => Value::NativeFun(NativeFun::MathSqrt),
                            "pi" => Value::Num(f64::consts::PI),
                            "e" => Value::Num(f64::consts::E),
                            _ => return None,
                        }
                    },
                    _ => return None,
                }
            },
            _ => {
                return None;
            },
        };

        Some(method)
    }

    fn find_actual_env(&self, jumps: Option<usize>) -> RcCell<Environment> {
        let curr_env = Rc::clone(&self.curr_env);

        match jumps {
            // Perform the jumps to find the value of the variable.
            Some(jumps) => {
                Environment::ancestor(curr_env, jumps)
                    .expect("unexpected error: outer scope was None")
            },
            // There are no jumps, so the variable is in the global scope.
            None => curr_env
        }
    }

    fn try_call_value(&mut self, callee: Value, args: Vec<Value>, ctx: Token) -> Result<Value, RuntimeOutcome> {
        match callee {
            Value::Fun(Fun { name: _, params, body, env: env_at_def }) => {
                self.check_arity(&args, params.len(), &ctx)?;

                // The function's local environment is enclosed by the 
                // environment present at definition time; allows closures.
                let mut local_fun_scope = Environment::with_enclosing(env_at_def);

                // Bind the arguments to the parameters.
                for (param, arg) in params.into_iter().zip(args) {
                    let ident = param.extract_ident().to_owned();
                    local_fun_scope.define(ident, arg);
                }

                let fn_res = self.execute_scoped(
                    body, 
                    local_fun_scope.to_rc_cell()
                );
                match fn_res {
                    // Return the function's return value if one is present.
                    Err(RuntimeOutcome::Return(val)) => Ok(val),
                    res => {
                        // Propogate any errors and return `nil` as a default value.
                        res?;
                        Ok(Value::Nil)
                    },
                }
            },
            Value::NativeFun(native_fun) => {
                self.check_arity(&args, native_fun.arity(), &ctx)?;

                self.call_native_fun(native_fun, args, ctx)
            },
            Value::NativeMethod(native_method) => {
                self.check_arity(&args, native_method.arity(), &ctx)?;

                self.call_native_method(native_method, args, ctx)
            },
            Value::Class(mut class) => {
                // Create an empty instance.
                let instance = Instance::new(
                    class.clone(), 
                    HashMap::new(),
                ).to_rc_cell();

                if let Some(constructor) = class.methods.get_mut("init") {
                    // The arity of the class call is the same as the constructor's.
                    self.check_arity(&args, constructor.params.len(), &ctx)?;

                    // Bind the instance to the constructor.
                    constructor.bind(Value::Instance(Rc::clone(&instance)));

                    // Run the constructor.
                    self.try_call_value(Value::Fun(constructor.clone()), args, ctx)?;
                } 
                else {
                    // If there is no constructor, then the class call
                    // has an arity of 0.
                    self.check_arity(&args, 0, &ctx)?;
                }

                Ok(Value::Instance(instance))
            },
            _ => {
                let err = RuntimeError::new(
                    format!("value '{callee}' is not callable"),
                    ctx,
                );
                return Err(err)?;
            },
        }
    }

    fn check_arity(&self, args: &[Value], arity: usize, ctx: &Token) -> Result<(), RuntimeOutcome> {
        if args.len() != arity {
            let err = RuntimeError::new(
                format!("expected {} arguments, but got {}", arity, args.len()),
                ctx.clone(),
            );
            return Err(err)?;
        }
        Ok(())
    }

    fn check_list_index(&self, arg: Value, len: usize, ctx: &Token) -> Result<usize, RuntimeOutcome> {
        let len = len as f64;

        let index = match arg {
            Value::Num(num) => {
                if num.fract() != 0.0 {
                    let err = RuntimeError::new(
                        "index must be an integer".to_owned(),
                        ctx.clone(),
                    );
                    return Err(RuntimeOutcome::Error(err));
                }
                else if num >= 0.0 && num < len {
                    num as usize
                }
                else if num < 0.0 && -num <= len {
                    (len + num) as usize
                }
                else {
                    let err = RuntimeError::new(
                        format!("out of bounds: index was {num}, but length is {len}"),
                        ctx.clone(),
                    );
                    return Err(RuntimeOutcome::Error(err));
                }
            },
            _ => {
                let type_ = arg.get_type();

                let err = RuntimeError::new(
                    format!("cannot index list with a value of type {type_}"),
                    ctx.clone(),
                );
                return Err(RuntimeOutcome::Error(err));
            },
        };

        Ok(index)
    }

    fn call_native_fun(&mut self, native_fun: NativeFun, args: Vec<Value>, ctx: Token) -> Result<Value, RuntimeOutcome> {
        use std::fs::{self, File};
        use std::io::BufReader;
        use std::io::Write as _;

        match native_fun {
            NativeFun::Println => {
                let arg = args.into_iter().nth(0).unwrap();

                match arg {
                    // Special-case string formatting to remove the quotes.
                    Value::Str(string) => println!("{string}"),
                    _ => println!("{arg}"),
                };

                Ok(Value::Nil)
            },
            NativeFun::Clock => {
                use std::time;

                let now = time::SystemTime::now();

                let elapsed = now.duration_since(time::UNIX_EPOCH)
                    .map_err(|_| {
                        RuntimeError::new(
                            "system time before Unix Epoch".to_owned(), 
                            ctx,
                        )
                    })?
                    .as_millis();

                Ok(Value::Num((elapsed as f64) / 1000.0))
            },
            NativeFun::ToString => {
                let arg = args.into_iter().nth(0).unwrap();

                let string = match arg {
                    // Special-case string formatting to remove the quotes.
                    Value::Str(string) => string,
                    _ => format!("{arg}"),
                };
                Ok(Value::Str(string))
            },
            NativeFun::Map => {
                Ok(Value::Map(HashMap::new().to_rc_cell()))
            },
            NativeFun::IOReadLines => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(path) = arg {
                    File::open(path)
                        // Read the lines from the file.
                        .map(|f| {
                            let reader = BufReader::new(f);

                            // Map the individual lines from `String` -> `Value::Str`.
                            reader
                                .lines()
                                .map(|l| Value::Str(l.unwrap()))
                                .collect::<Vec<_>>()
                        })
                        // Convert the lines `Vec<_>` into a `Value::List`.
                        .map(|lines| Value::List(lines.to_rc_cell()))
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                } 
                else {
                    let type_ = arg.get_type();
                    let err = RuntimeError::new(
                        format!("path must be a string, was of type {type_}"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::IOReadString => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(path) = arg {
                    fs::read_to_string(path)
                        .map(Value::Str)
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                } 
                else {
                    let type_ = arg.get_type();
                    let err = RuntimeError::new(
                        format!("path must be a string, was of type {type_}"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::IOWriteLines => {
                let mut args = args.into_iter();
                let arg1 = args.next().unwrap();
                let arg2 = args.next().unwrap();

                if let (Value::Str(path), Value::List(lines)) = (arg1, arg2) {
                    File::create(path)
                        .and_then(|f| {
                            let mut writer = BufWriter::new(f);

                            // Write each line to the file.
                            // Special-case string formatting to remove the quotes.
                            for val in lines.borrow().iter() {
                                match val {
                                    Value::Str(line) => writeln!(writer, "{line}")?,
                                    _ => writeln!(writer, "{val}")?,
                                };
                            }
                            Ok(())
                        })
                        .map(|_| Value::Nil)
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                }
                else {
                    let err = RuntimeError::new(
                        format!("first argument must be a string, second argument must be a list"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::IOWriteString => {
                let mut args = args.into_iter();
                let arg1 = args.next().unwrap();
                let arg2 = args.next().unwrap();

                if let (Value::Str(path), Value::Str(content)) = (arg1, arg2) {
                    fs::write(path, content)
                        .map(|_| Value::Nil)
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                } 
                else {
                    let err = RuntimeError::new(
                        format!("arguments must be strings"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::IOExists => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(path) = arg {
                    fs::exists(path)
                        .map(Value::Bool)
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a string"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::IOMakeDir => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(path) = arg {
                    fs::create_dir(path)
                        .map(|_| Value::Nil)
                        .map_err(|io_err| {
                            let err = RuntimeError::new(
                                format!("{io_err}"),
                                ctx,
                            );
                            RuntimeOutcome::Error(err)
                        })
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a string"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::MathPow => {
                let mut args = args.into_iter();
                let arg1 = args.next().unwrap();
                let arg2 = args.next().unwrap();

                if let (Value::Num(base), Value::Num(exp)) = (arg1, arg2) {
                    Ok(Value::Num(base.powf(exp)))
                }
                else {
                    let err = RuntimeError::new(
                        format!("arguments must be numbers"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::MathSqrt => {
                let arg = args.into_iter().next().unwrap();

                if let Value::Num(num) = arg {
                    Ok(Value::Num(num.sqrt()))
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a number"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::MathSin => {
                let arg = args.into_iter().next().unwrap();

                if let Value::Num(num) = arg {
                    Ok(Value::Num(num.sin()))
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a number"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::MathCos => {
                let arg = args.into_iter().next().unwrap();

                if let Value::Num(num) = arg {
                    Ok(Value::Num(num.cos()))
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a number"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeFun::MathTan => {
                let arg = args.into_iter().next().unwrap();

                if let Value::Num(num) = arg {
                    Ok(Value::Num(num.tan()))
                }
                else {
                    let err = RuntimeError::new(
                        format!("argument must be a number"),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
        }
    }

    fn call_native_method(&mut self, native_method: NativeMethod, args: Vec<Value>, ctx: Token) -> Result<Value, RuntimeOutcome> {
        match native_method {
            NativeMethod::StrLength(string) => {
                Ok(Value::Num(string.len() as f64))
            },
            NativeMethod::ListLength(list) => {
                Ok(Value::Num(list.borrow().len() as f64))
            },
            NativeMethod::ListGet(list) => {
                let arg = args.into_iter().nth(0).unwrap();
                let index = self.check_list_index(arg, list.borrow().len(), &ctx)?;

                let elem = list.borrow()
                    .get(index)
                    .unwrap()
                    .clone();

                Ok(elem)
            },
            NativeMethod::ListSet(list) => {
                let mut args = args.into_iter();

                let index = self.check_list_index(args.next().unwrap(), list.borrow().len(), &ctx)?;
                let value = args.next().unwrap();

                list.borrow_mut()
                    .get_mut(index)
                    .map(|entry| *entry = value)
                    .unwrap();

                Ok(Value::Nil)
            },
            NativeMethod::ListClone(list) => {
                let clone = Vec::clone(&*list.borrow());
                Ok(Value::List(clone.to_rc_cell()))
            },
            NativeMethod::ListSort(list) => {
                // Flag to detect if `Value::partial_cmp` ever returns `None`.
                // This means that two values weren't comparable and the sort 
                // was invalid.
                let mut invalid_sort = false;

                list
                    .borrow_mut()
                    .sort_by(|a, b| {
                        a.partial_cmp(b)
                            .unwrap_or_else(|| {
                                invalid_sort = true;
                                std::cmp::Ordering::Equal
                            })
                    });

                if invalid_sort {
                    let err = RuntimeError::new(
                        format!("not all items in the list are comparable"),
                        ctx,
                    );
                    return Err(RuntimeOutcome::Error(err));
                }

                Ok(Value::Nil)
            },
            NativeMethod::ListPush(list) => {
                let elem = args.into_iter().nth(0).unwrap();
                list.borrow_mut().push(elem);
                Ok(Value::List(list))
            },
            NativeMethod::ListPop(list) => {
                list
                    .borrow_mut()
                    .pop()
                    .ok_or_else(|| {
                        let err = RuntimeError::new(
                            "empty list".to_owned(),
                            ctx,
                        );
                        RuntimeOutcome::Error(err)
                    })
            },
            NativeMethod::MapSet(map) => {
                let mut args = args.into_iter();
                let arg1 = args.next().unwrap();
                let arg2 = args.next().unwrap();

                if let Value::Str(key) = arg1 {
                    map.borrow_mut().insert(key, arg2);
                    Ok(Value::Map(map))
                }
                else {
                    let err = RuntimeError::new(
                        "key must be a string".to_owned(),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeMethod::MapHas(map) => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(key) = arg {
                    Ok(Value::Bool(map.borrow().contains_key(&key)))
                }
                else {
                    let err = RuntimeError::new(
                        "key must be a string".to_owned(),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeMethod::MapRemove(map) => {
                let arg = args.into_iter().nth(0).unwrap();

                if let Value::Str(key) = arg {
                    map.borrow_mut().remove(&key);
                    Ok(Value::Map(map))
                }
                else {
                    let err = RuntimeError::new(
                        "key must be a string".to_owned(),
                        ctx,
                    );
                    Err(RuntimeOutcome::Error(err))
                }
            },
            NativeMethod::MapClone(map) => {
                let clone = HashMap::clone(&*map.borrow());
                Ok(Value::Map(clone.to_rc_cell()))
            },
            NativeMethod::MapKeys(map) => {
                let keys: Vec<_> = map
                    .borrow()
                    .keys()
                    .map(|k| Value::Str(k.to_owned()))
                    .collect();
                
                Ok(Value::List(keys.to_rc_cell()))
            },
        }
    }

    /// Returns a new environment that has the current environment 
    /// as its outer (enclosing) environment.
    fn new_env_with_enclosing(&self) -> RcCell<Environment> {
        Environment::with_enclosing(
            Rc::clone(&self.curr_env),
        ).to_rc_cell()
    }

    fn globals() -> Environment {
        let mut globals = Environment::new();

        // Global functions.
        globals.define(
            "println".to_owned(), 
            Value::NativeFun(NativeFun::Println),
        );
        globals.define(
            "clock".to_owned(), 
            Value::NativeFun(NativeFun::Clock),
        );
        globals.define(
            "toString".to_owned(),
            Value::NativeFun(NativeFun::ToString),
        );
        globals.define(
            "Map".to_owned(),
            Value::NativeFun(NativeFun::Map),
        );
        // Namespaces.
        globals.define(
            "io".to_owned(),
            Value::Namespace("io".to_owned()),
        );
        globals.define(
            "math".to_owned(),
            Value::Namespace("math".to_owned()),
        );

        globals
    }
}
