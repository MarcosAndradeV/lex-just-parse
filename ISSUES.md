# Project Issues & TODOs

## 1. Customizable Keywords in Lexer
Currently, keywords in `lex-just-parse` are hardcoded or need to be manually added to the `lex_identfier` function inside `src/lexer.rs` (e.g., un-commenting the `"var" => TokenKind::Var` line).

**Problem:** 
If this library is published to `crates.io`, users will not be able to customize or add their own language keywords without forking the crate.

**Action Item:**
Refactor the `Lexer` to support injecting custom keywords dynamically. Potential solutions could involve:
- Passing a keyword mapping (e.g., `HashMap<&str, TokenKind>`) to the lexer upon initialization.
- Implementing a generic approach mapping specific string literals to a `TokenKind::Keyword(String)` variant.
- Providing a builder pattern: `Lexer::builder().with_keyword("let", TokenKind::Let).build()`.

**Status:** Needs to be resolved before publishing to `crates.io`.
