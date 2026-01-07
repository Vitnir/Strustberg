#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    LetKw,
    ConstLetKw,
    AddKw,
    SubKw,
    PrintKw,
    IfKw,
    WhileKw,
    TryKw,
    CatchKw,
    ThrowKw,
    IsKw,

    LBrace,
    RBrace,
    LParen,
    RParen,
    Semicolon,

    Plus,
    Minus,
    Star,
    Slash,

    EqEq,
    NotEq,
    Lt,
    Le,
    Gt,
    Ge,
    AndAnd,
    OrOr,
    Bang,
    Assign,

    Ident(String),
    Int(i64),
    Str(String),
    Bool(bool),

    Eof,
}

#[derive(Debug, Clone)]
pub struct LexedToken {
    pub tok: Token,
    pub line: usize,
    pub col: usize,
}

pub fn lex(input: &str) -> Result<Vec<LexedToken>, String> {
    Lexer::new(input).lex_all()
}

struct Lexer<'a> {
    input: &'a str,
    pos: usize, // byte offset
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    fn lex_all(mut self) -> Result<Vec<LexedToken>, String> {
        let mut out = Vec::new();
        loop {
            self.skip_ws_and_comments();

            let (line, col) = (self.line, self.col);
            let tok = self.next_token()?;

            out.push(LexedToken {
                tok: tok.clone(),
                line,
                col,
            });

            if tok == Token::Eof {
                break;
            }
        }
        Ok(out)
    }

    fn next_token(&mut self) -> Result<Token, String> {
        if self.pos >= self.input.len() {
            return Ok(Token::Eof);
        }

        if let Some(t) = self.try_phrase_keywords()? {
            return Ok(t);
        }

        let c = self.peek_char().ok_or_else(|| "Unexpected EOF".to_string())?;

        // Delimiters
        match c {
            '{' => {
                self.bump();
                return Ok(Token::LBrace);
            }
            '}' => {
                self.bump();
                return Ok(Token::RBrace);
            }
            '(' => {
                self.bump();
                return Ok(Token::LParen);
            }
            ')' => {
                self.bump();
                return Ok(Token::RParen);
            }
            ';' => {
                self.bump();
                return Ok(Token::Semicolon);
            }
            _ => {}
        }

        // Two-char operators
        if self.try_consume("==") {
            return Ok(Token::EqEq);
        }
        if self.try_consume("!=") {
            return Ok(Token::NotEq);
        }
        if self.try_consume("<=") {
            return Ok(Token::Le);
        }
        if self.try_consume(">=") {
            return Ok(Token::Ge);
        }
        if self.try_consume("&&") {
            return Ok(Token::AndAnd);
        }
        if self.try_consume("||") {
            return Ok(Token::OrOr);
        }

        // Single-char operators
        match c {
            '+' => {
                self.bump();
                return Ok(Token::Plus);
            }
            '-' => {
                self.bump();
                return Ok(Token::Minus);
            }
            '*' => {
                self.bump();
                return Ok(Token::Star);
            }
            '/' => {
                self.bump();
                return Ok(Token::Slash);
            }
            '<' => {
                self.bump();
                return Ok(Token::Lt);
            }
            '>' => {
                self.bump();
                return Ok(Token::Gt);
            }
            '!' => {
                self.bump();
                return Ok(Token::Bang);
            }
            '=' => {
                self.bump();
                return Ok(Token::Assign);
            }
            '"' => return self.lex_string(),
            _ => {}
        }

        // Number
        if c.is_ascii_digit() {
            return self.lex_int();
        }

        // Identifier / small keywords
        if is_ident_start(c) {
            return self.lex_ident_or_kw();
        }

        Err(format!(
            "Unexpected character '{}' at {}:{}",
            c, self.line, self.col
        ))
    }

    fn try_phrase_keywords(&mut self) -> Result<Option<Token>, String> {
        const PHRASES: &[(&str, Token)] = &[
            ("Das kommt auf den Tisch", Token::LetKw),
            ("Das packen wir oben drauf", Token::AddKw),
            ("Das ziehen wir ab", Token::SubKw),
            ("Ich bin ja kein Mensch, ich bin ein System", Token::PrintKw),
            ("Ist ja nicht mein Bier", Token::IfKw),
            ("Das ist wie mit dem Joghurt", Token::WhileKw),
            ("MACHEN WIR MAL NE RUNDE RISIKO", Token::TryKw),
            ("WENN'S KNALLT", Token::CatchKw),
            ("Büro ist Krieg", Token::ThrowKw),
            ("Das ist von ganz oben. Da rüttelt mir hier keiner dran", Token::ConstLetKw),
            // Optional alias for closing brace:
            ("UND DAMIT HAT SICH DAS", Token::RBrace),
        ];

        for (phrase, tok) in PHRASES {
            if self.starts_with_phrase(phrase) {
                self.consume_phrase(phrase);
                return Ok(Some(tok.clone()));
            }
        }

        Ok(None)
    }

    fn starts_with_phrase(&self, phrase: &str) -> bool {
        let rem = &self.input[self.pos..];
        if !rem.starts_with(phrase) {
            return false;
        }

        // boundary: next char must be whitespace/delimiter/op or EOF
        let next = rem[phrase.len()..].chars().next();
        match next {
            None => true,
            Some(ch) => {
                ch.is_whitespace()
                    || "{}();".contains(ch)
                    || "+-*/=!<>".contains(ch)
                    || ch == '&'
                    || ch == '|'
            }
        }
    }

    fn consume_phrase(&mut self, phrase: &str) {
        for _ in phrase.chars() {
            self.bump();
        }
    }

    fn lex_int(&mut self) -> Result<Token, String> {
        let start = self.pos;
        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() {
                self.bump();
            } else {
                break;
            }
        }
        let s = &self.input[start..self.pos];
        let v = s.parse::<i64>().map_err(|_| {
            format!("Invalid integer '{}' at {}:{}", s, self.line, self.col)
        })?;
        Ok(Token::Int(v))
    }

    fn lex_string(&mut self) -> Result<Token, String> {
        self.bump(); // opening quote

        let mut out = String::new();
        while let Some(c) = self.peek_char() {
            match c {
                '"' => {
                    self.bump();
                    return Ok(Token::Str(out));
                }
                '\\' => {
                    self.bump();
                    let esc = self
                        .peek_char()
                        .ok_or_else(|| "Unterminated escape".to_string())?;
                    self.bump();
                    match esc {
                        'n' => out.push('\n'),
                        't' => out.push('\t'),
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        _ => {
                            return Err(format!(
                                "Unknown escape \\{} at {}:{}",
                                esc, self.line, self.col
                            ))
                        }
                    }
                }
                _ => {
                    out.push(c);
                    self.bump();
                }
            }
        }

        Err(format!(
            "Unterminated string literal at {}:{}",
            self.line, self.col
        ))
    }

    fn lex_ident_or_kw(&mut self) -> Result<Token, String> {
        let start = self.pos;
        self.bump(); // first char
        while let Some(c) = self.peek_char() {
            if is_ident_continue(c) {
                self.bump();
            } else {
                break;
            }
        }

        let s = &self.input[start..self.pos];
        Ok(match s {
            "ist" => Token::IsKw,
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Ident(s.to_string()),
        })
    }

    fn skip_ws_and_comments(&mut self) {
        loop {
            if self.pos >= self.input.len() {
                return;
            }

            let Some(c) = self.peek_char() else { return; };

            if c.is_whitespace() {
                self.bump();
                continue;
            }

            // '#' line comment
            if c == '#' {
                while let Some(ch) = self.peek_char() {
                    self.bump();
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }

            break;
        }
    }

    fn try_consume(&mut self, s: &str) -> bool {
        let rem = &self.input[self.pos..];
        if !rem.starts_with(s) {
            return false;
        }
        for _ in s.chars() {
            self.bump();
        }
        true
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }

    fn bump(&mut self) {
        let Some(c) = self.peek_char() else { return; };
        self.pos += c.len_utf8();

        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}

fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}
