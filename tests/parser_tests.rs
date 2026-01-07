use strustberg::{lexer::lex, parser::parse};
use strustberg::ast::{Expr, Stmt};

fn parse_prog(src: &str) -> Vec<Stmt> {
    let tokens = lex(src).unwrap();
    parse(&tokens).unwrap()
}

#[test]
fn parse_const_let_statement() {
    let p = parse_prog(r#"Das ist von ganz oben. Da rüttelt mir hier keiner dran x ist 5;"#);
    assert_eq!(p.len(), 1);

    match &p[0] {
        Stmt::ConstLet { name, value } => {
            assert_eq!(name, "x");
            match value {
                Expr::Int(v) => assert_eq!(*v, 5),
                _ => panic!("expected int literal"),
            }
        }
        _ => panic!("expected const let"),
    }
}
