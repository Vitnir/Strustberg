# Strustberg 🧾☕
*A serious programming language nobody understands.*

Strustberg is a **toy programming language written in Rust** that looks important, sounds managerial, and ultimately just does simple things like counting, looping, and printing values — while nobody around you understands what is actually happening.

Inspired by **ArnoldC**, powered by **Rust (Edition 2024)**, and flavored with iconic Stromberg quotes.

Perfect for:
- Obfuscating trivial logic in a very German way
- Hiding simple calculations behind corporate buzzwords
- Internal jokes, demos, talks, or just having fun
- Proving that readability is a social construct

---

## Features

- 🦀 Written in **Rust 2024**
- 🧠 **Pratt parser** with real expression precedence
- 🧱 `{}` blocks (with optional quote aliases)
- 🧮 Integer arithmetic (`+ - * /`)
- 🔀 Comparisons (`== != < <= > >=`)
- 🔗 Boolean logic (`&& || !`)
- 🔁 `while` loops
- ❓ `if` statements
- 💣 `throw` + `try/catch`
- 🧳 Scoped variables
- 📜 Extremely verbose keywords for extremely simple logic

---

## Example

```strustberg
Das kommt auf den Tisch x ist 1;
Ich bin ja kein Mensch, ich bin ein System "Start";

Das packen wir oben drauf x 4;
Ich bin ja kein Mensch, ich bin ein System x;

Ist ja nicht mein Bier (x != 0) {
    Ich bin ja kein Mensch, ich bin ein System "x ist ungleich 0";
}

Das ist wie mit dem Joghurt (x < 8) {
    Das packen wir oben drauf x 1;
    Ich bin ja kein Mensch, ich bin ein System x;
}

MACHEN WIR MAL NE RUNDE RISIKO {
    Büro ist Krieg "kaputt";
} WENN'S KNALLT {
    Ich bin ja kein Mensch, ich bin ein System "Exception gefangen, weiter geht's";
}

Ich bin ja kein Mensch, ich bin ein System "Ende";
```

# Output
```text
Start
5
x ist ungleich 0
6
7
8
Exception gefangen, weiter geht's
Ende
```

Yes — all of that just increments a number.

# Why?
Because sometimes you want to:

Hide business logic in plain sight

Make simple code look extremely important

Demonstrate parsers, interpreters, and language design

Confuse your colleagues (harmlessly)

Enjoy writing a language that sounds like middle management

Strustberg is not about performance, productivity, or maintainability.

Strustberg is about vibes.

# Syntax Overview
| Concept    | Example |
|-----------|---------|
| Variable  | `Das kommt auf den Tisch x ist 5;` |
| **Global Constant (immutable)** | `Das ist von ganz oben. Da rüttelt mir hier keiner dran x ist 42;` |
| Add       | `Das packen wir oben drauf x 1;` |
| Subtract | `Das ziehen wir ab x 2;` |
| Print    | `Ich bin ja kein Mensch, ich bin ein System x;` |
| If       | `Ist ja nicht mein Bier (x > 0) { ... }` |
| While    | `Das ist wie mit dem Joghurt (x < 10) { ... }` |
| Throw    | `Büro ist Krieg "kaputt";` |
| Try/Catch| `MACHEN WIR MAL NE RUNDE RISIKO { ... } WENN'S KNALLT { ... }` |

Blocks use { }, but closing braces can optionally be replaced with:

UND DAMIT HAT SICH DAS


Because why not.

Running
cargo run -- example.strustberg


Rust version used:

rustc 1.92.0
edition = 2024

```text
Project Structure
src/
├── ast.rs          # AST definitions
├── lexer.rs        # Tokenizer with quote-keywords
├── parser.rs       # Pratt parser + block handling
├── interpreter.rs  # Scoped interpreter with throw/catch
└── main.rs
```

# Disclaimer

Strustberg is:
- ❌ Not meant for production
- ❌ Not meant for clarity
- ❌ Not meant for beginners

Strustberg is:

- ✅ A parser/interpreter playground
- ✅ A joke language with real internals
- ✅ A reminder that syntax is optional

License

Do whatever you want.
If someone seriously uses this in production, that’s on them.

“Ich sag mal so: technisch gesehen läuft das.”
