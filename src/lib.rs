//! # lex-just-parse
//! 
//! `lex-just-parse` is a simple and easy-to-use lexing and parsing crate for Rust.
//! It provides a fast, stream-based lexical analyzer (`Lexer`) and a combinator-style parser (`Parser`)
//! to assist in developing custom languages, DSLs, or any structured text parsing needs.
//!
//! ## Overview
//!
//! - **[`lexer`]**: Contains the `Lexer` and token definitions for tokenizing string inputs.
//! - **[`parser`]**: Contains the `Parser` utilities for building ASTs.

pub mod lexer;
pub mod parser;
