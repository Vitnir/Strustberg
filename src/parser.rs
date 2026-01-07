use crate::ast::{Expr, InfixOp, PrefixOp, Stmt};
use crate::lexer::{LexedToken, Token};

pub fn parse(tokens: &[LexedToken]) -> Result<Vec<Stmt>, String> {
    let mut p = Parser::new(tokens);
    p.parse_program()
}

struct Parser<'a> {
    toks: &'a [LexedToken],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(toks: &'a [LexedToken]) -> Self {
        Self { toks, pos: 0 }
    }

    fn parse_program(&mut self) -> Result<Vec<Stmt>, String> {
        let mut out = Vec::new();
        while !self.is_eof() {
            out.push(self.parse_stmt()?);
        }
        Ok(out)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        match self.peek() {
            Token::LetKw => self.parse_let(),
            Token::ConstLetKw => self.parse_const_let(),
            Token::AddKw => self.parse_add_assign(),
            Token::SubKw => self.parse_sub_assign(),
            Token::PrintKw => self.parse_print(),
            Token::IfKw => self.parse_if(),
            Token::WhileKw => self.parse_while(),
            Token::TryKw => self.parse_try_catch(),
            Token::ThrowKw => self.parse_throw(),
            Token::RBrace => Err(self.err_here("Unexpected '}'".to_string())),
            Token::Eof => Err("Unexpected EOF".to_string()),
            _ => {
                let expr = self.parse_expr_bp(0)?;
                self.expect(Token::Semicolon)?;
                Ok(Stmt::ExprStmt(expr))
            }
        }
    }

    fn parse_let(&mut self) -> Result<Stmt, String> {
        self.expect(Token::LetKw)?;
        let name = self.expect_ident()?;
        self.expect(Token::IsKw)?;
        let value = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Let { name, value })
    }

    fn parse_const_let(&mut self) -> Result<Stmt, String> {
        self.expect(Token::ConstLetKw)?;
        let name = self.expect_ident()?;
        self.expect(Token::IsKw)?;
        let value = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::ConstLet { name, value })
    }

    fn parse_add_assign(&mut self) -> Result<Stmt, String> {
        self.expect(Token::AddKw)?;
        let name = self.expect_ident()?;
        let value = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::AddAssign { name, value })
    }

    fn parse_sub_assign(&mut self) -> Result<Stmt, String> {
        self.expect(Token::SubKw)?;
        let name = self.expect_ident()?;
        let value = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::SubAssign { name, value })
    }

    fn parse_print(&mut self) -> Result<Stmt, String> {
        self.expect(Token::PrintKw)?;
        let expr = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Print(expr))
    }

    fn parse_if(&mut self) -> Result<Stmt, String> {
        self.expect(Token::IfKw)?;
        self.expect(Token::LParen)?;
        let cond = self.parse_expr_bp(0)?;
        self.expect(Token::RParen)?;
        let then_block = self.parse_block()?;
        Ok(Stmt::If { cond, then_block })
    }

    fn parse_while(&mut self) -> Result<Stmt, String> {
        self.expect(Token::WhileKw)?;
        self.expect(Token::LParen)?;
        let cond = self.parse_expr_bp(0)?;
        self.expect(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(Stmt::While { cond, body })
    }

    fn parse_try_catch(&mut self) -> Result<Stmt, String> {
        self.expect(Token::TryKw)?;
        let try_block = self.parse_block()?;
        self.expect(Token::CatchKw)?;
        let catch_block = self.parse_block()?;
        Ok(Stmt::TryCatch {
            try_block,
            catch_block,
        })
    }

    fn parse_throw(&mut self) -> Result<Stmt, String> {
        self.expect(Token::ThrowKw)?;
        let message = self.parse_expr_bp(0)?;
        self.expect(Token::Semicolon)?;
        Ok(Stmt::Throw { message })
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, String> {
        self.expect(Token::LBrace)?;
        let mut out = Vec::new();
        while !matches!(self.peek(), Token::RBrace | Token::Eof) {
            out.push(self.parse_stmt()?);
        }
        self.expect(Token::RBrace)?;
        Ok(out)
    }

    // -------------------------
    // Pratt parser
    // -------------------------

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Expr, String> {
        let mut lhs = self.parse_prefix()?;

        loop {
            let op = match self.peek() {
                Token::Plus => InfixOp::Add,
                Token::Minus => InfixOp::Sub,
                Token::Star => InfixOp::Mul,
                Token::Slash => InfixOp::Div,

                Token::EqEq => InfixOp::Eq,
                Token::NotEq => InfixOp::Ne,
                Token::Lt => InfixOp::Lt,
                Token::Le => InfixOp::Le,
                Token::Gt => InfixOp::Gt,
                Token::Ge => InfixOp::Ge,

                Token::AndAnd => InfixOp::And,
                Token::OrOr => InfixOp::Or,

                _ => break,
            };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            self.next_owned(); // consume operator
            let rhs = self.parse_expr_bp(r_bp)?;
            lhs = Expr::Infix {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        match self.peek() {
            Token::Minus => {
                self.next_owned();
                let rhs = self.parse_expr_bp(prefix_binding_power())?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Neg,
                    rhs: Box::new(rhs),
                })
            }
            Token::Bang => {
                self.next_owned();
                let rhs = self.parse_expr_bp(prefix_binding_power())?;
                Ok(Expr::Prefix {
                    op: PrefixOp::Not,
                    rhs: Box::new(rhs),
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.next_owned() {
            Token::Int(v) => Ok(Expr::Int(v)),
            Token::Str(s) => Ok(Expr::Str(s)),
            Token::Bool(b) => Ok(Expr::Bool(b)),
            Token::Ident(s) => Ok(Expr::Ident(s)),
            Token::LParen => {
                let e = self.parse_expr_bp(0)?;
                self.expect(Token::RParen)?;
                Ok(e)
            }
            other => Err(self.err_here(format!(
                "Expected primary expression, got {:?}",
                other
            ))),
        }
    }

    // -------------------------
    // token helpers
    // -------------------------

    fn peek(&self) -> &Token {
        self.toks.get(self.pos).map(|t| &t.tok).unwrap_or(&Token::Eof)
    }

    fn next_owned(&mut self) -> Token {
        let t = self
            .toks
            .get(self.pos)
            .map(|t| t.tok.clone())
            .unwrap_or(Token::Eof);
        self.pos = self.pos.saturating_add(1);
        t
    }

    fn expect(&mut self, expected: Token) -> Result<(), String> {
        let got = self.next_owned();
        if got == expected {
            Ok(())
        } else {
            Err(self.err_here(format!("Expected {:?}, got {:?}", expected, got)))
        }
    }

    fn expect_ident(&mut self) -> Result<String, String> {
        match self.next_owned() {
            Token::Ident(s) => Ok(s),
            other => Err(self.err_here(format!("Expected identifier, got {:?}", other))),
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }

    fn err_here(&self, msg: String) -> String {
        if let Some(t) = self.toks.get(self.pos) {
            format!("{} at {}:{}", msg, t.line, t.col)
        } else {
            msg
        }
    }
}

fn prefix_binding_power() -> u8 {
    100
}

fn infix_binding_power(op: InfixOp) -> (u8, u8) {
    match op {
        InfixOp::Or => (1, 2),
        InfixOp::And => (3, 4),

        InfixOp::Eq | InfixOp::Ne => (5, 6),
        InfixOp::Lt | InfixOp::Le | InfixOp::Gt | InfixOp::Ge => (7, 8),

        InfixOp::Add | InfixOp::Sub => (9, 10),
        InfixOp::Mul | InfixOp::Div => (11, 12),
    }
}
