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

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, NumberBase, TokenKind};

    #[test]
    fn test_real_numbers() {
        let source = "1.0 2.64 89.0 0.123";
        let mut lex = Lexer::new(source);
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::RealNumber);
        assert_eq!(t.source(), "1.0");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::RealNumber);
        assert_eq!(t.source(), "2.64");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::RealNumber);
        assert_eq!(t.source(), "89.0");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::RealNumber);
        assert_eq!(t.source(), "0.123");
    }

    #[test]
    fn test_numbers() {
        let source = "123 0xFF 0o755 0b0101";
        let mut lex = Lexer::new(source);
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::Number(NumberBase::D));
        assert_eq!(t.source(), "123");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::Number(NumberBase::X));
        assert_eq!(t.source(), "FF");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::Number(NumberBase::O));
        assert_eq!(t.source(), "755");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::Number(NumberBase::B));
        assert_eq!(t.source(), "0101");
    }
}
