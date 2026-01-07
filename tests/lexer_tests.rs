use strustberg::lexer::{lex, Token};

fn toks(src: &str) -> Vec<Token> {
    lex(src).unwrap().into_iter().map(|t| t.tok).collect()
}

#[test]
fn lex_const_phrase() {
    let t = toks(r#"Das ist von ganz oben. Da rüttelt mir hier keiner dran x ist 42;"#);
    assert!(t.contains(&Token::ConstLetKw));
    assert!(t.contains(&Token::Ident("x".to_string())));
    assert!(t.contains(&Token::IsKw));
    assert!(t.contains(&Token::Int(42)));
    assert!(t.contains(&Token::Semicolon));
}
