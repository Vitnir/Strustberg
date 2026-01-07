use strustberg::lexer::{lex, Token};

fn toks(src: &str) -> Vec<Token> {
    lex(src).unwrap().into_iter().map(|t| t.tok).collect()
}

#[test]
fn lex_phrase_keywords_and_aliases() {
    let src = r#"
Das kommt auf den Tisch x ist 5;
Ich bin ja kein Mensch, ich bin ein System "Hi";
UND DAMIT HAT SICH DAS
"#;

    let t = toks(src);

    assert!(t.contains(&Token::LetKw));
    assert!(t.contains(&Token::PrintKw));
    assert!(t.contains(&Token::RBrace)); // alias expands to RBrace
    assert!(t.contains(&Token::Ident("x".to_string())));
    assert!(t.contains(&Token::Int(5)));
    assert!(t.contains(&Token::Str("Hi".to_string())));
    assert!(t.iter().any(|x| matches!(x, Token::Semicolon)));
}

#[test]
fn lex_operators_and_bools() {
    let src = r#"
Das kommt auf den Tisch x ist 1 + 2 * 3;
Ist ja nicht mein Bier (x != 0 && true || false) { UND DAMIT HAT SICH DAS
"#;

    let t = toks(src);

    assert!(t.contains(&Token::Plus));
    assert!(t.contains(&Token::Star));
    assert!(t.contains(&Token::NotEq));
    assert!(t.contains(&Token::AndAnd));
    assert!(t.contains(&Token::OrOr));
    assert!(t.contains(&Token::Bool(true)));
    assert!(t.contains(&Token::Bool(false)));
}

#[test]
fn lex_comments_are_ignored() {
    let src = r#"
# this is a comment
Das kommt auf den Tisch x ist 1; # inline comment
"#;

    let t = toks(src);
    assert!(t.contains(&Token::LetKw));
    assert!(t.contains(&Token::Ident("x".to_string())));
    assert!(t.contains(&Token::Int(1)));
}
