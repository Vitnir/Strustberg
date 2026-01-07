use strustberg::{lexer::lex, parser::parse};
use strustberg::interpreter::{Interpreter, Value, RuntimeError};

fn run(src: &str) -> Result<Interpreter, RuntimeError> {
    let tokens = lex(src).unwrap();
    let program = parse(&tokens).unwrap();
    let mut i = Interpreter::new();
    i.interpret(&program)?;
    Ok(i)
}

#[test]
fn let_and_add_assign_work() {
    let i = run(
        r#"
Das kommt auf den Tisch x ist 1;
Das packen wir oben drauf x 4;
"#,
    )
    .unwrap();

    assert_eq!(i.get_var("x"), Some(Value::Int(5)));
}

#[test]
fn while_loop_runs() {
    let i = run(
        r#"
Das kommt auf den Tisch x ist 1;
Das ist wie mit dem Joghurt (x < 4) {
    Das packen wir oben drauf x 1;
}
"#,
    )
    .unwrap();

    assert_eq!(i.get_var("x"), Some(Value::Int(4)));
}

#[test]
fn if_executes_only_when_truthy() {
    let i = run(
        r#"
Das kommt auf den Tisch x ist 0;
Ist ja nicht mein Bier (x != 0) {
    Das packen wir oben drauf x 1;
}
"#,
    )
    .unwrap();

    assert_eq!(i.get_var("x"), Some(Value::Int(0)));
}

#[test]
fn try_catch_catches_throw() {
    let i = run(
        r#"
Das kommt auf den Tisch x ist 1;
MACHEN WIR MAL NE RUNDE RISIKO {
    Büro ist Krieg "kaputt";
    Das packen wir oben drauf x 100; # must not execute
} WENN'S KNALLT {
    Das packen wir oben drauf x 5;
}
"#,
    )
    .unwrap();

    assert_eq!(i.get_var("x"), Some(Value::Int(6)));
}

#[test]
fn uncaught_throw_bubbles_up() {
    let err = run(r#"Büro ist Krieg "kaputt";"#).unwrap_err();
    match err {
        RuntimeError::Thrown(Value::Str(s)) => assert_eq!(s, "kaputt"),
        _ => panic!("expected thrown string"),
    }
}
