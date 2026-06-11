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
    ($lex:ident, $f:expr) => {
        match $f {
            Parser::Success(lexer, expr) => {
                $lex = lexer;
                expr
            }
            Parser::Fail(lexer, e) => {
                $lex = lexer;
                return Parser::Fail($lex, e);
            }
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

/// Parses zero or more occurrences of `parser` until it fails.
/// Returns the collected items and the updated lexer.
pub fn many<'lex, T, E, F>(mut lex: RefLexer<'lex>, parser: F) -> Parser<'lex, Vec<T>, E>
where
    F: Fn(RefLexer<'lex>) -> Parser<'lex, T, E>,
{
    let mut results = Vec::new();
    loop {
        match parser(lex) {
            Parser::Success(next_lex, val) => {
                results.push(val);
                lex = next_lex;
            }
            Parser::Fail(next_lex, _) => {
                return Parser::Success(next_lex, results);
            }
        }
    }
}

/// Parses one or more occurrences of `parser`.
/// Returns Fail if the first attempt fails.
pub fn many1<'lex, T, E, F>(lex: RefLexer<'lex>, parser: F) -> Parser<'lex, Vec<T>, E>
where
    F: Fn(RefLexer<'lex>) -> Parser<'lex, T, E>,
{
    match parser(lex) {
        Parser::Success(lex, first_val) => {
            let mut results = vec![first_val];
            let mut current_lex = lex;
            loop {
                match parser(current_lex) {
                    Parser::Success(next_lex, val) => {
                        results.push(val);
                        current_lex = next_lex;
                    }
                    Parser::Fail(next_lex, _) => {
                        return Parser::Success(next_lex, results);
                    }
                }
            }
        }
        Parser::Fail(lex, err) => Parser::Fail(lex, err),
    }
}

/// Parses zero or more occurrences of `parser` separated by `separator`.
pub fn sep_by<'lex, T, S, E, F, G>(
    lex: RefLexer<'lex>,
    parser: F,
    separator: G,
) -> Parser<'lex, Vec<T>, E>
where
    F: Fn(RefLexer<'lex>) -> Parser<'lex, T, E>,
    G: Fn(RefLexer<'lex>) -> Parser<'lex, S, E>,
{
    match parser(lex) {
        Parser::Success(lex, first_val) => {
            let mut results = vec![first_val];
            let mut current_lex = lex;
            loop {
                match separator(current_lex) {
                    Parser::Success(sep_lex, _) => {
                        match parser(sep_lex) {
                            Parser::Success(next_lex, val) => {
                                results.push(val);
                                current_lex = next_lex;
                            }
                            Parser::Fail(fail_lex, _) => {
                                return Parser::Success(fail_lex, results);
                            }
                        }
                    }
                    Parser::Fail(next_lex, _) => {
                        return Parser::Success(next_lex, results);
                    }
                }
            }
        }
        Parser::Fail(lex, _) => Parser::Success(lex, Vec::new()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{Lexer, TokenKind};

    fn parse_ident<'lex>(lex: RefLexer<'lex>, expected: &str) -> Parser<'lex, String, String> {
        let tok = lex.peek().clone();
        if tok.kind == TokenKind::Identifier && tok.source() == expected {
            lex.next(); // consume
            Parser::Success(lex, tok.source.to_string())
        } else {
            Parser::Fail(
                lex,
                format!("Expected identifier '{}', got {:?}", expected, tok.kind),
            )
        }
    }

    #[test]
    fn test_parser_success_and_fail_variants() {
        let mut lexer = Lexer::new("abc");
        let result_success = parse_ident(&mut lexer, "abc");
        match result_success {
            Parser::Success(_, val) => assert_eq!(val, "abc"),
            _ => panic!("Expected Success"),
        }

        let mut lexer = Lexer::new("xyz");
        let result_fail = parse_ident(&mut lexer, "abc");
        match result_fail {
            Parser::Fail(_, err) => assert!(err.contains("Expected identifier 'abc'")),
            _ => panic!("Expected Fail"),
        }
    }

    #[test]
    fn test_parser_success_method() {
        let mut lexer = Lexer::new("abc");
        let result = parse_ident(&mut lexer, "abc").success();
        assert_eq!(result.ok().unwrap(), "abc");

        let mut lexer2 = Lexer::new("xyz");
        let result2 = parse_ident(&mut lexer2, "abc").success();
        assert!(result2.is_err());
        let (remaining_lexer, err) = result2.err().unwrap();
        assert_eq!(remaining_lexer.next().source(), "xyz");
        assert!(err.contains("Expected identifier 'abc'"));
    }

    #[test]
    fn test_parser_or_else() {
        // Test standard or_else functionality where first parser fails and second succeeds
        let mut lexer = Lexer::new("xyz");
        let result = parse_ident(&mut lexer, "abc").or_else(|lex| parse_ident(lex, "xyz"));

        match result {
            Parser::Success(_, val) => assert_eq!(val, "xyz"),
            _ => panic!("Expected success after or_else fallback"),
        }

        // Test or_else where first parser succeeds (second should not run)
        let mut lexer = Lexer::new("abc");
        let result = parse_ident(&mut lexer, "abc")
            .or_else(|_lex| panic!("Should not execute or_else fallback when first succeeded"));
        match result {
            Parser::Success(_, val) => assert_eq!(val, "abc"),
            _ => panic!("Expected success"),
        }
    }

    #[test]
    fn test_parser_and_then() {
        // Test chaining with and_then
        // We want to parse "abc" then "xyz"
        let mut lexer = Lexer::new("abc xyz");
        let result = parse_ident(&mut lexer, "abc").and_then(|lex, first_val| {
            parse_ident(lex, "xyz")
                .and_then(|lex, second_val| Parser::Success(lex, (first_val, second_val)))
        });

        match result {
            Parser::Success(_, (v1, v2)) => {
                assert_eq!(v1, "abc");
                assert_eq!(v2, "xyz");
            }
            _ => panic!("Expected success for chained and_then"),
        }

        // Test and_then failure propagation
        let mut lexer2 = Lexer::new("abc error");
        let result2 =
            parse_ident(&mut lexer2, "abc").and_then(|lex, _first_val| parse_ident(lex, "xyz"));
        match result2 {
            Parser::Fail(_, err) => assert!(err.contains("Expected identifier 'xyz'")),
            _ => panic!("Expected Fail"),
        }

        // Test and_then when first parser fails (second should not run)
        let mut lexer3 = Lexer::new("error xyz");
        let result3 = parse_ident(&mut lexer3, "abc").and_then(
            |_lex, _first_val| -> Parser<String, String> {
                panic!("Should not execute and_then function when first failed")
            },
        );
        match result3 {
            Parser::Fail(_, err) => assert!(err.contains("Expected identifier 'abc'")),
            _ => panic!("Expected Fail"),
        }
    }

    #[test]
    fn test_try_parse_macro() {
        // Define a parser function that parses "abc xyz" using try_parse!
        fn parse_pair<'lex>(lex: RefLexer<'lex>) -> Parser<'lex, (String, String), String> {
            let (lex, first) = try_parse!(parse_ident(lex, "abc"));
            let (lex, second) = try_parse!(parse_ident(lex, "xyz"));
            Parser::Success(lex, (first, second))
        }

        let mut lexer = Lexer::new("abc xyz");
        let res = parse_pair(&mut lexer);
        match res {
            Parser::Success(_, (v1, v2)) => {
                assert_eq!(v1, "abc");
                assert_eq!(v2, "xyz");
            }
            _ => panic!("Expected success using try_parse!"),
        }

        let mut lexer2 = Lexer::new("abc err");
        let res2 = parse_pair(&mut lexer2);
        match res2 {
            Parser::Fail(_, err) => assert!(err.contains("Expected identifier 'xyz'")),
            _ => panic!("Expected Fail using try_parse!"),
        }
    }

    #[test]
    fn test_try_parse_macro_in_place() {
        fn parse_pair_in_place<'lex>(mut lex: RefLexer<'lex>) -> Parser<'lex, (String, String), String> {
            let first = try_parse!(lex, parse_ident(lex, "abc"));
            let second = try_parse!(lex, parse_ident(lex, "xyz"));
            Parser::Success(lex, (first, second))
        }

        let mut lexer = Lexer::new("abc xyz");
        let res = parse_pair_in_place(&mut lexer);
        match res {
            Parser::Success(_, (v1, v2)) => {
                assert_eq!(v1, "abc");
                assert_eq!(v2, "xyz");
            }
            _ => panic!("Expected success using try_parse! in-place"),
        }

        let mut lexer2 = Lexer::new("abc err");
        let res2 = parse_pair_in_place(&mut lexer2);
        match res2 {
            Parser::Fail(_, err) => assert!(err.contains("Expected identifier 'xyz'")),
            _ => panic!("Expected Fail using try_parse! in-place"),
        }
    }

    #[test]
    fn test_combinator_many() {
        let mut lexer = Lexer::new("abc abc abc xyz");
        let result = many(&mut lexer, |l| parse_ident(l, "abc"));
        match result {
            Parser::Success(remaining_lexer, items) => {
                assert_eq!(items, vec!["abc", "abc", "abc"]);
                assert_eq!(remaining_lexer.next().source(), "xyz");
            }
            _ => panic!("Expected Success for many"),
        }
        
        let mut lexer2 = Lexer::new("xyz");
        let result2 = many(&mut lexer2, |l| parse_ident(l, "abc"));
        match result2 {
            Parser::Success(remaining_lexer, items) => {
                assert!(items.is_empty());
                assert_eq!(remaining_lexer.next().source(), "xyz");
            }
            _ => panic!("Expected Success for many with empty results"),
        }
    }

    #[test]
    fn test_combinator_many1() {
        let mut lexer = Lexer::new("abc abc xyz");
        let result = many1(&mut lexer, |l| parse_ident(l, "abc"));
        match result {
            Parser::Success(remaining_lexer, items) => {
                assert_eq!(items, vec!["abc", "abc"]);
                assert_eq!(remaining_lexer.next().source(), "xyz");
            }
            _ => panic!("Expected Success for many1"),
        }

        let mut lexer2 = Lexer::new("xyz");
        let result2 = many1(&mut lexer2, |l| parse_ident(l, "abc"));
        match result2 {
            Parser::Fail(remaining_lexer, err) => {
                assert!(err.contains("Expected identifier 'abc'"));
                assert_eq!(remaining_lexer.next().source(), "xyz");
            }
            _ => panic!("Expected Fail for many1 on immediately failing parser"),
        }
    }

    #[test]
    fn test_combinator_sep_by() {
        fn parse_comma<'lex>(lex: RefLexer<'lex>) -> Parser<'lex, (), String> {
            let tok = lex.peek().clone();
            if tok.kind == TokenKind::Comma {
                lex.next();
                Parser::Success(lex, ())
            } else {
                Parser::Fail(lex, "Expected comma".to_string())
            }
        }

        let mut lexer = Lexer::new("abc , abc , abc ;");
        let result = sep_by(&mut lexer, |l| parse_ident(l, "abc"), parse_comma);
        match result {
            Parser::Success(remaining_lexer, items) => {
                assert_eq!(items, vec!["abc", "abc", "abc"]);
                assert_eq!(remaining_lexer.next().kind, TokenKind::SemiColon);
            }
            _ => panic!("Expected Success for sep_by"),
        }

        let mut lexer2 = Lexer::new("xyz");
        let result2 = sep_by(&mut lexer2, |l| parse_ident(l, "abc"), parse_comma);
        match result2 {
            Parser::Success(remaining_lexer, items) => {
                assert!(items.is_empty());
                assert_eq!(remaining_lexer.next().source(), "xyz");
            }
            _ => panic!("Expected Success for sep_by on empty sequence"),
        }
    }
}
