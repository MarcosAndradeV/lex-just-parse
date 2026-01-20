pub mod parser;
pub mod lexer;

#[macro_export]
/// Expect a token kind
macro_rules! parser_expect {
    ($lex:expr, $kind:expr, $fail:expr) => {
        if $lex.peek().kind != $kind {
            return Parser::Fail($lex, $fail);
        }
        $lex.next()
    };
}

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
