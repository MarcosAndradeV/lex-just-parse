//! # lex-just-parse
//!
//! `lex-just-parse` is a fast, stream-based lexing and parsing crate for Rust.
//! It provides a lexical analyzer ([`Lexer`](lexer::Lexer)) with built-in string interning
//! and combinator-style parser utilities ([`Parser`](parser::Parser)) to assist in developing
//! custom languages, DSLs, or structured text parsing needs.
//!
//! ## Overview
//!
//! - **[`lexer`]**: Contains the `Lexer` and token definitions for tokenizing string inputs.
//! - **[`parser`]**: Contains the `Parser` combinators for building ASTs.
//!
//! ## Example
//!
//! ```rust
//! use lex_just_parse::lexer::{Lexer, TokenKind};
//!
//! let mut lexer = Lexer::new("main.lang", "let x = 42;")
//!     .with_keywords(&["let"]);
//!
//! let tok1 = lexer.next_token();
//! assert_eq!(tok1.kind, TokenKind::Keyword);
//! assert_eq!(tok1.source(), "let");
//!
//! let tok2 = lexer.next_token();
//! assert_eq!(tok2.kind, TokenKind::Identifier);
//! assert_eq!(tok2.source(), "x");
//! ```

pub mod lexer;
pub mod parser;
