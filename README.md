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
    
    // Initialize the lexer and configure accepted keywords
    let mut lexer = Lexer::new(source).with_keywords(&["var", "let", "fn"]);

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

## Publishing Status

This library is currently used across local projects but is fully configurable for standalone usage. The lexical analyzer previously required hardcoding keywords, but it has since been updated to support dynamic custom keywords via the `Lexer::with_keywords()` builder method, making it suitable for generic language parsing.
