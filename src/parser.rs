use crate::lexer::Lexer;

pub type RefLexer<'lex> = &'lex mut Lexer<'lex>;

pub type ParserFn<'lex, T, E> = fn(lex: RefLexer) -> Parser<T, E>;

pub enum Parser<'lex, T, E> {
    Success(RefLexer<'lex>, T),
    Fail(RefLexer<'lex>, E),
}

impl<'lex, T, E> Parser<'lex, T, E> {
    pub fn or_else<F>(self, f: F) -> Self
    where
        F: FnOnce(RefLexer) -> Parser<T, E>,
    {
        match self {
            Parser::Success(..) => self,
            Parser::Fail(lexer, ..) => f(lexer),
        }
    }

    pub fn and_then<U, F>(self, f: F) -> Parser<'lex, U, E>
    where
        F: FnOnce(RefLexer<'lex>, T) -> Parser<'lex, U, E>,
    {
        match self {
            Parser::Success(lexer, e) => f(lexer, e),
            Parser::Fail(lexer, e) => Parser::Fail(lexer, e),
        }
    }

    pub fn success(self) -> Result<T, E> {
        match self {
            Parser::Success(_, e) => Ok(e),
            Parser::Fail(_, e) => Err(e),
        }
    }
}
