use strustberg::{lexer::lex, parser::parse};
use strustberg::ast::{Expr, InfixOp, Stmt};

fn parse_prog(src: &str) -> Vec<Stmt> {
    let tokens = lex(src).unwrap();
    parse(&tokens).unwrap()
}

#[test]
fn parse_let_statement() {
    let p = parse_prog(r#"Das kommt auf den Tisch x ist 5;"#);
    assert_eq!(p.len(), 1);

    match &p[0] {
        Stmt::Let { name, value } => {
            assert_eq!(name, "x");
            match value {
                Expr::Int(v) => assert_eq!(*v, 5),
                _ => panic!("expected int literal"),
            }
        }
        _ => panic!("expected let"),
    }
}

#[test]
fn parse_if_and_block() {
    let p = parse_prog(
        r#"
Das kommt auf den Tisch x ist 1;
Ist ja nicht mein Bier (x != 0) {
    Das packen wir oben drauf x 1;
}
"#,
    );

    assert_eq!(p.len(), 2);
    match &p[1] {
        Stmt::If { cond, then_block } => {
            assert!(!then_block.is_empty());
            // cond is infix !=
            match cond {
                Expr::Infix { op, .. } => assert_eq!(*op, InfixOp::Ne),
                _ => panic!("expected infix condition"),
            }
        }
        _ => panic!("expected if"),
    }
}

#[test]
fn parse_try_catch() {
    let p = parse_prog(
        r#"
MACHEN WIR MAL NE RUNDE RISIKO {
    Büro ist Krieg "kaputt";
} WENN'S KNALLT {
    Ich bin ja kein Mensch, ich bin ein System "ok";
}
"#,
    );

    assert_eq!(p.len(), 1);
    match &p[0] {
        Stmt::TryCatch { try_block, catch_block } => {
            assert_eq!(try_block.len(), 1);
            assert_eq!(catch_block.len(), 1);
        }
        _ => panic!("expected try/catch"),
    }
}

#[test]
fn precedence_mul_binds_tighter_than_add() {
    let p = parse_prog(r#"Das kommt auf den Tisch x ist 1 + 2 * 3;"#);
    match &p[0] {
        Stmt::Let { value, .. } => {
            // should be: 1 + (2 * 3)
            match value {
                Expr::Infix { op: InfixOp::Add, lhs, rhs } => {
                    matches!(**lhs, Expr::Int(1));
                    match &**rhs {
                        Expr::Infix { op: InfixOp::Mul, .. } => {}
                        _ => panic!("expected mul on rhs"),
                    }
                }
                _ => panic!("expected add at top"),
            }
        }
        _ => panic!("expected let"),
    }
}
