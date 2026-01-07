use strustberg::{lexer::lex, parser::parse};
use strustberg::interpreter::{Interpreter, RuntimeError, Value};

fn run_ok(src: &str) -> Interpreter {
    let tokens = lex(src).unwrap();
    let program = parse(&tokens).unwrap();
    let mut i = Interpreter::new();
    i.interpret(&program).unwrap();
    i
}

fn run_err(src: &str) -> RuntimeError {
    let tokens = lex(src).unwrap();
    let program = parse(&tokens).unwrap();
    let mut i = Interpreter::new();
    i.interpret(&program).unwrap_err()
}

#[test]
fn arithmetic_and_assignment_work() {
    let i = run_ok(
        r#"
Das kommt auf den Tisch x ist 1;
Das packen wir oben drauf x 4;
"#,
    );

    assert_eq!(i.get_var("x"), Some(Value::Int(5)));
}

#[test]
fn while_loop_executes() {
    let i = run_ok(
        r#"
Das kommt auf den Tisch x ist 1;
Das ist wie mit dem Joghurt (x < 4) {
    Das packen wir oben drauf x 1;
}
"#,
    );

    assert_eq!(i.get_var("x"), Some(Value::Int(4)));
}

#[test]
fn try_catch_handles_throw() {
    let i = run_ok(
        r#"
Das kommt auf den Tisch x ist 1;
MACHEN WIR MAL NE RUNDE RISIKO {
    Büro ist Krieg "boom";
} WENN'S KNALLT {
    Das packen wir oben drauf x 5;
}
"#,
    );

    assert_eq!(i.get_var("x"), Some(Value::Int(6)));
}

#[test]
fn const_is_immutable() {
    let err = run_err(
        r#"
Das ist von ganz oben. Da rüttelt mir hier keiner dran x ist 1;
Das packen wir oben drauf x 1;
"#,
    );

    assert_eq!(
        err,
        RuntimeError::Thrown(Value::Str(
            "Das ist von ganz oben. Da rüttelt mir hier keiner dran.".into()
        ))
    );
}

#[test]
fn uncaught_throw_bubbles_up() {
    let err = run_err(r#"Büro ist Krieg "kaputt";"#);

    assert_eq!(
        err,
        RuntimeError::Thrown(Value::Str("kaputt".into()))
    );
}
