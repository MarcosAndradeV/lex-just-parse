# lex-just-parse

`lex-just-parse` is a fast, lightweight, and easy-to-use lexing and parsing crate for Rust. It provides a stream-based lexical analyzer (`Lexer`) with built-in string interning and combinator-style parser utilities (`Parser`) to assist in developing programming languages, DSLs, or structured text parsers.

## Features

- **Stream-based Lexing:** Sequentially reads and tokenizes string sources on demand via `next_token()` and `peek()`.
- **Built-in String Interning:** All token source strings are automatically interned into `'static` string slices (`TokenSource`), providing zero-allocation copies and fast pointer-level equality checks.
- **Rich Token Categories:** Supports identifiers, keywords, numbers in multiple bases (binary, octal, decimal, hex), string literals, character literals (with unicode/hex escapes), preprocessor directives, and single/multi-character operators.
- **Custom Rules & Hooks:** Intercept tokenization with `with_pre_rule` and `with_post_rule` to produce `TokenKind::Custom(u32)` or `TokenKind::CustomTag(&'static str)`.
- **Backtracking & Savepoints:** Save and restore lexer positions effortlessly (`save()` / `restore()`) for speculative parsing and lookaheads.
- **Source Location Tracking:** Tracks source file, line, and column coordinates automatically (`file_path:line:col`).
- **Parser Combinators:** Ergonomic combinators (`many`, `many1`, `sep_by`, `and_then`, `or_else`, `inspect`, `inspect_fail`, `success`) and the `try_parse!` macro for robust AST construction.

---

## Quick Start

Add `lex-just-parse` to your `Cargo.toml`:

```toml
[dependencies]
lex-just-parse = "1.3"
```

### Basic Lexing Example

```rust
use lex_just_parse::lexer::{Lexer, TokenKind};

fn main() {
    let source = "var x = 42;";

    // Initialize the lexer with a file path and the source string,
    // and configure recognized keywords:
    let mut lexer = Lexer::new("main.lang", source).with_keywords(&["var", "let", "fn"]);

    // Consume tokens sequentially
    loop {
        let token = lexer.next_token();

        println!("{:?} `{}` at {}", token.kind, token.unescape(), token.loc);

        if token.is_eof() {
            break;
        }
    }
}
```

---

## Key Capabilities

### 1. Built-in String Interning (`TokenSource`)

All token textual representations are wrapped in `TokenSource`, which stores an interned `&'static str`. This makes token cloning cheap and allows direct equality comparisons against `&str`, `&String`, or `String`:

```rust
let token = lexer.next_token();
assert_eq!(token.source(), "var");
assert_eq!(token.source, "var"); // TokenSource implements PartialEq<str>
```

### 2. Character Literals & Escape Sequences

The lexer supports character literals including escape sequences, hex codes (`\xHH`), and Unicode scalar values (`\u{...}`):

```rust
let mut lexer = Lexer::new("test", "'a' '\\n' '🔥' '\\x41' '\\u{1F525}'");
let tok = lexer.next_token();
assert_eq!(tok.kind, TokenKind::CharacterLiteral);
assert_eq!(tok.unescape(), "a");
```

### 3. Savepoints & Backtracking

When parsing ambiguous grammar branches, save the lexer state and restore it if a branch fails:

```rust
let savepoint = lexer.save();

let tok1 = lexer.next_token();
let tok2 = lexer.next_token();

// Revert back to the saved position
lexer.restore(savepoint);
```

### 4. Custom Token Rules

Register custom pre-rules (evaluated before standard token rules) or post-rules (evaluated as fallbacks) using `RuleFn`:

```rust
use lex_just_parse::lexer::{Lexer, Token, TokenKind};

fn parse_mention(lex: &mut Lexer) -> Option<Token> {
    let save = lex.save();
    let tok = lex.next_token();
    if tok.kind == TokenKind::At {
        let ident = lex.next_token();
        if ident.kind == TokenKind::Identifier {
            return Some(Token::new(
                TokenKind::CustomTag("mention"),
                tok.loc,
                format!("@{}", ident.source()),
            ));
        }
    }
    lex.restore(save);
    None
}

let mut lexer = Lexer::new("input", "@alice")
    .with_pre_rule(parse_mention);

let tok = lexer.next_token();
assert_eq!(tok.kind, TokenKind::CustomTag("mention"));
assert_eq!(tok.source(), "@alice");
```

### 5. Parser Combinators

The `parser` module provides combinators for building ASTs from token streams:

```rust
use lex_just_parse::lexer::{Lexer, TokenKind};
use lex_just_parse::parser::{many, sep_by, Parser, RefLexer};

fn parse_ident<'lex>(lex: RefLexer<'lex>, expected: &str) -> Parser<'lex, String, String> {
    let tok = lex.peek().clone();
    if tok.kind == TokenKind::Identifier && tok.source() == expected {
        lex.next_token(); // consume token
        Parser::Success(lex, tok.source.to_string())
    } else {
        Parser::Fail(lex, format!("Expected identifier '{expected}'"))
    }
}

let mut lexer = Lexer::new("test", "item item item");
let result = many(&mut lexer, |l| parse_ident(l, "item"));

match result {
    Parser::Success(_lex, items) => {
        assert_eq!(items.len(), 3);
    }
    Parser::Fail(_lex, err) => eprintln!("Parse error: {err}"),
}
```

The `try_parse!` macro provides early-return syntax similar to Rust's `?` operator:

```rust
use lex_just_parse::try_parse;
use lex_just_parse::parser::{Parser, RefLexer};

fn parse_pair<'lex>(mut lex: RefLexer<'lex>) -> Parser<'lex, (String, String), String> {
    let first = try_parse!(lex, parse_ident(lex, "first"));
    let second = try_parse!(lex, parse_ident(lex, "second"));
    Parser::Success(lex, (first, second))
}
```

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
