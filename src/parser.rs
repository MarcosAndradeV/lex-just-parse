//! Combinator parsing utilities.
//!
//! Provides the core `Parser` type and related type definitions 
//! for building lexer-driven parsers.

use crate::lexer::Lexer;

/// A mutable reference to a `Lexer`, commonly used by parser functions to consume tokens.
pub type RefLexer<'lex> = &'lex mut Lexer<'lex>;

/// Represents a typical parser function signature that takes a lexer reference and returns a `Parser` result.
pub type ParserFn<'lex, T, E> = fn(lex: RefLexer) -> Parser<T, E>;

#[macro_export]
/// The `?` operator for [`Parser`]
macro_rules! try_parse {
    ($f:expr) => {
        match $f {
            Parser::Success(lexer, expr) => (lexer, expr),
            Parser::Fail(lexer, e) => return Parser::Fail(lexer, e),
        }
    };
}

/// Represents the result of a parsing operation.
///
/// A parser either succeeds with an advanced lexer and a parsed value `T`, or fails
/// and returns the unchanged lexer state along with an error `E`.
pub enum Parser<'lex, T, E> {
    Success(RefLexer<'lex>, T),
    Fail(RefLexer<'lex>, E),
}

impl<'lex, T, E> Parser<'lex, T, E> {
    /// Chains another parsing attempt if the current parser failed.
    pub fn or_else<F>(self, f: F) -> Self
    where
        F: FnOnce(RefLexer) -> Parser<T, E>,
    {
        match self {
            Parser::Success(..) => self,
            Parser::Fail(lexer, ..) => f(lexer),
        }
    }

    /// Chains a subsequent parser if the current parser succeeds.
    pub fn and_then<U, F>(self, f: F) -> Parser<'lex, U, E>
    where
        F: FnOnce(RefLexer<'lex>, T) -> Parser<'lex, U, E>,
    {
        match self {
            Parser::Success(lexer, e) => f(lexer, e),
            Parser::Fail(lexer, e) => Parser::Fail(lexer, e),
        }
    }

    /// Converts this parser result into a standard `Result`,
    /// returning the parsed item or the lexer and error pair upon failure.
    pub fn success(self) -> Result<T, (RefLexer<'lex>, E)> {
        match self {
            Parser::Success(_, e) => Ok(e),
            Parser::Fail(lex, e) => Err((lex, e)),
        }
    }
}
