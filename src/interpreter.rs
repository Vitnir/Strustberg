use crate::ast::{Expr, InfixOp, PrefixOp, Stmt};
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Str(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::Str(v) => write!(f, "{v}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    Message(String),
    Thrown(Value),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Message(s) => write!(f, "{s}"),
            RuntimeError::Thrown(v) => write!(f, "Thrown: {v}"),
        }
    }
}

impl Error for RuntimeError {}

#[derive(Debug, Default)]
pub struct Interpreter {
    scopes: Vec<HashMap<String, Value>>,
    consts: HashMap<String, Value>,
    const_mut_attempts: HashMap<String, u32>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            consts: HashMap::new(),
            const_mut_attempts: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result<(), RuntimeError> {
        self.exec_block(program, false)
    }

    fn exec_block(&mut self, block: &[Stmt], new_scope: bool) -> Result<(), RuntimeError> {
        if new_scope {
            self.scopes.push(HashMap::new());
        }

        let mut out = Ok(());
        for stmt in block {
            out = self.exec(stmt);
            if out.is_err() {
                break;
            }
        }

        if new_scope {
            self.scopes.pop();
        }

        out
    }

    fn exec(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Let { name, value } => {
                if self.consts.contains_key(name) {
                    return Err(RuntimeError::Message(format!(
                        "Cannot shadow const '{}'",
                        name
                    )));
                }
                let v = self.eval(value)?;
                self.set_current(name, v);
                Ok(())
            }

            Stmt::ConstLet { name, value } => {
                if self.consts.contains_key(name) {
                    return Err(RuntimeError::Message(format!(
                        "Const '{}' is already defined",
                        name
                    )));
                }
                if self.is_defined_var(name) {
                    return Err(RuntimeError::Message(format!(
                        "Name '{}' is already used by a variable",
                        name
                    )));
                }

                let v = self.eval(value)?;
                self.consts.insert(name.clone(), v);
                Ok(())
            }

            Stmt::AddAssign { name, value } => {
                let rhs = self.eval(value)?;
                let rhs_i = expect_int(&rhs, "AddAssign requires int")?;

                let cur_i = {
                    let cur_ref = self.get_ref(name)?;
                    expect_int(cur_ref, "AddAssign target must be int")?
                };

                self.assign_existing(name, Value::Int(cur_i + rhs_i))
            }

            Stmt::SubAssign { name, value } => {
                let rhs = self.eval(value)?;
                let rhs_i = expect_int(&rhs, "SubAssign requires int")?;

                let cur_i = {
                    let cur_ref = self.get_ref(name)?;
                    expect_int(cur_ref, "SubAssign target must be int")?
                };

                self.assign_existing(name, Value::Int(cur_i - rhs_i))
            }

            Stmt::Print(expr) => {
                let v = self.eval(expr)?;
                println!("{v}");
                Ok(())
            }

            Stmt::If { cond, then_block } => {
                let c = self.eval(cond)?;
                if is_truthy(&c) {
                    self.exec_block(then_block, true)?;
                }
                Ok(())
            }

            Stmt::While { cond, body } => {
                loop {
                    let c = self.eval(cond)?;
                    if !is_truthy(&c) {
                        break;
                    }
                    self.exec_block(body, true)?;
                }
                Ok(())
            }

            Stmt::TryCatch {
                try_block,
                catch_block,
            } => match self.exec_block(try_block, true) {
                Ok(()) => Ok(()),
                Err(RuntimeError::Thrown(_)) => {
                    self.exec_block(catch_block, true)?;
                    Ok(())
                }
                Err(e) => Err(e),
            },

            Stmt::Throw { message } => {
                let v = self.eval(message)?;
                Err(RuntimeError::Thrown(v))
            }

            Stmt::ExprStmt(e) => {
                let _ = self.eval(e)?;
                Ok(())
            }
        }
    }

    fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Int(v) => Ok(Value::Int(*v)),
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Ident(name) => Ok(self.get_ref(name)?.clone()),

            Expr::Prefix { op, rhs } => {
                let r = self.eval(rhs)?;
                match op {
                    PrefixOp::Neg => Ok(Value::Int(-expect_int(&r, "Unary '-' requires int")?)),
                    PrefixOp::Not => Ok(Value::Bool(!is_truthy(&r))),
                }
            }

            Expr::Infix { lhs, op, rhs } => {
                if *op == InfixOp::And {
                    let l = self.eval(lhs)?;
                    if !is_truthy(&l) {
                        return Ok(Value::Bool(false));
                    }
                    let r = self.eval(rhs)?;
                    return Ok(Value::Bool(is_truthy(&r)));
                }
                if *op == InfixOp::Or {
                    let l = self.eval(lhs)?;
                    if is_truthy(&l) {
                        return Ok(Value::Bool(true));
                    }
                    let r = self.eval(rhs)?;
                    return Ok(Value::Bool(is_truthy(&r)));
                }

                let l = self.eval(lhs)?;
                let r = self.eval(rhs)?;
                eval_infix(*op, l, r)
            }
        }
    }

    fn get_ref(&self, name: &str) -> Result<&Value, RuntimeError> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Ok(v);
            }
        }
        if let Some(v) = self.consts.get(name) {
            return Ok(v);
        }
        Err(RuntimeError::Message(format!(
            "Variable '{}' is undefined",
            name
        )))
    }

    fn assign_existing(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        if self.consts.contains_key(name) {
            let n = self
                .const_mut_attempts
                .entry(name.to_string())
                .and_modify(|c| *c += 1)
                .or_insert(1);

            let msg = if *n == 1 {
                "Das ist von ganz oben. Da rüttelt mir hier keiner dran."
            } else {
                "Das ist Chefsache. Thema beendet."
            };

            return Err(RuntimeError::Thrown(Value::Str(msg.to_string())));
        }

        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }

        Err(RuntimeError::Message(format!(
            "Variable '{}' is undefined",
            name
        )))
    }

    #[allow(dead_code)]
    pub fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        self.consts.get(name).cloned()
    }

    fn set_current(&mut self, name: &str, value: Value) {
        if let Some(last) = self.scopes.last_mut() {
            last.insert(name.to_string(), value);
        }
    }

    fn is_defined_var(&self, name: &str) -> bool {
        self.scopes.iter().any(|s| s.contains_key(name))
    }
}

fn is_truthy(v: &Value) -> bool {
    match v {
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Str(s) => !s.is_empty(),
    }
}

fn expect_int(v: &Value, msg: &str) -> Result<i64, RuntimeError> {
    match v {
        Value::Int(i) => Ok(*i),
        _ => Err(RuntimeError::Message(msg.to_string())),
    }
}

fn eval_infix(op: InfixOp, l: Value, r: Value) -> Result<Value, RuntimeError> {
    match op {
        InfixOp::Add => match (l, r) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Str(a), Value::Str(b)) => Ok(Value::Str(format!("{a}{b}"))),
            (Value::Str(a), b) => Ok(Value::Str(format!("{a}{b}"))),
            (a, Value::Str(b)) => Ok(Value::Str(format!("{a}{b}"))),
            _ => Err(RuntimeError::Message(
                "Operator '+' not supported for these types".to_string(),
            )),
        },

        InfixOp::Sub => Ok(Value::Int(
            expect_int(&l, "Operator '-' requires int")?
                - expect_int(&r, "Operator '-' requires int")?,
        )),
        InfixOp::Mul => Ok(Value::Int(
            expect_int(&l, "Operator '*' requires int")?
                * expect_int(&r, "Operator '*' requires int")?,
        )),
        InfixOp::Div => {
            let a = expect_int(&l, "Operator '/' requires int")?;
            let b = expect_int(&r, "Operator '/' requires int")?;
            if b == 0 {
                return Err(RuntimeError::Message("Division by zero".to_string()));
            }
            Ok(Value::Int(a / b))
        }

        InfixOp::Eq => Ok(Value::Bool(l == r)),
        InfixOp::Ne => Ok(Value::Bool(l != r)),

        InfixOp::Lt => Ok(Value::Bool(
            expect_int(&l, "Operator '<' requires int")?
                < expect_int(&r, "Operator '<' requires int")?,
        )),
        InfixOp::Le => Ok(Value::Bool(
            expect_int(&l, "Operator '<=' requires int")?
                <= expect_int(&r, "Operator '<=' requires int")?,
        )),
        InfixOp::Gt => Ok(Value::Bool(
            expect_int(&l, "Operator '>' requires int")?
                > expect_int(&r, "Operator '>' requires int")?,
        )),
        InfixOp::Ge => Ok(Value::Bool(
            expect_int(&l, "Operator '>=' requires int")?
                >= expect_int(&r, "Operator '>=' requires int")?,
        )),

        InfixOp::And | InfixOp::Or => unreachable!("handled via short-circuit"),
    }
}
