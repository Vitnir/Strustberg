#[derive(Debug, Clone)]
pub enum Stmt {
    Let { name: String, value: Expr },
    AddAssign { name: String, value: Expr },
    SubAssign { name: String, value: Expr },

    Print(Expr),

    If { cond: Expr, then_block: Vec<Stmt> },
    While { cond: Expr, body: Vec<Stmt> },

    TryCatch { try_block: Vec<Stmt>, catch_block: Vec<Stmt> },
    Throw { message: Expr },

    ExprStmt(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Bool(bool),
    Ident(String),

    Prefix { op: PrefixOp, rhs: Box<Expr> },
    Infix { lhs: Box<Expr>, op: InfixOp, rhs: Box<Expr> },
}

#[derive(Debug, Clone, Copy)]
pub enum PrefixOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    And,
    Or,
}
