mod ast;
mod interpreter;
mod lexer;
mod parser;

use std::env;

fn main() {
    let path = env::args()
        .nth(1)
        .unwrap_or_else(|| "example.strustberg".to_string());

    let source = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read '{}': {}", path, e);
            std::process::exit(1);
        }
    };

    let tokens = match lexer::lex(&source) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Lexer error: {}", e);
            std::process::exit(1);
        }
    };

    let program = match parser::parse(&tokens) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parser error: {}", e);
            std::process::exit(1);
        }
    };

    let mut interp = interpreter::Interpreter::new();
    if let Err(e) = interp.interpret(&program) {
        eprintln!("Runtime error: {}", e);
        std::process::exit(1);
    }
}
