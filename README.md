# lex-just-parse

`lex-just-parse` is a simple and easy-to-use lexing and parsing crate for Rust. It provides a fast, stream-based lexical analyzer (`Lexer`) and combinator-style parser utilities (`Parser`) to assist in developing custom programming languages, DSLs, or handling structured text parsing needs.

## Features

- **Stream-based Lexing:** Sequentially reads and tokenizes string sources on demand.
- **Built-in Token Kinds:** Comes with standard token categories including numbers (binary, octal, decimal, hex), strings, identifiers, and operators.
- **Parser Combinators:** Intuitive parser utilities (`and_then`, `or_else`) for robust AST building.
- **Source Location Tracking:** Tracks line and column positions automatically.

## Example Usage

Here is a basic example of how to instantiate the lexer and parse tokens from a string.

```rust
use lex_just_parse::lexer::{Lexer, TokenKind};

fn main() {
    let source = "var x = 42;";
    let mut lexer = Lexer::new(source);

    // Consume tokens until EOF
    loop {
        let token = lexer.next();
        
        println!("{:?} `{}` at {}", token.kind, token.unescape(), token.loc);
        
        if token.is_eof() {
            break;
        }
    }
}
```

## Note on Publishing

Currently, this library is intended for local project usage and is not available on `crates.io`. If you need to add custom keywords to the language (e.g., `let`, `fn`, `var`), you must manually modify the `lex_identfier` method in `src/lexer.rs`. A refactor to support dynamic keyword configuration is planned before public release.
