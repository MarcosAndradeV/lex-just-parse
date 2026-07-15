//! Provides lexical analysis features.
//!
//! This module contains the `Lexer` and the tokens it generates.

use std::fmt;

/// A stream-based lexical analyzer capable of interpreting string sources.
///
/// `Lexer` sequentially reads the underlying string slice and produces
/// tokens on demand via the [`next()`](Self::next) and [`peek()`](Self::peek) methods.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    data: Vec<char>,
    pos: usize,
    byte_pos: usize,
    loc: Loc,
    peeked: Option<Token>,
    keywords: Vec<&'src str>,
}

impl<'src> Lexer<'src> {
    #[cfg(feature = "interning")]
    /// Creates a new `Lexer` given a source string slice.
    pub fn new(file_path: impl Into<String>, source: &'src str) -> Self {
        Self {
            source,
            data: source.chars().collect(),
            loc: Loc::new(file_path, 1, 1),
            pos: 0,
            byte_pos: 0,
            peeked: None,
            keywords: Vec::new(),
        }
    }

    #[cfg(not(feature = "interning"))]
    /// Creates a new `Lexer` given a source string slice.
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            data: source.chars().collect(),
            loc: Loc::new(1, 1),
            pos: 0,
            byte_pos: 0,
            peeked: None,
            keywords: Vec::new(),
        }
    }

    /// Configures the lexer with a set of predefined keywords to recognize.
    pub fn with_keywords(mut self, keywords: &[&'src str]) -> Self {
        self.keywords = keywords.to_vec();
        self
    }

    /// Returns the next token in the stream, consuming it in the process.
    /// If an EOF is reached, it will continue to return `EOF` tokens.
    pub fn next(&mut self) -> Token {
        if let Some(peek) = self.peeked.take() {
            peek
        } else {
            self.next_token()
        }
    }

    /// Returns a reference to the next token without consuming it.
    /// Subsequent calls to `peek()` or `next()` will return this same token.
    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    fn advance(&mut self) -> char {
        let ch = self.read_char();
        self.byte_pos += ch.len_utf8();
        self.pos += 1;
        self.loc.next(ch);
        ch
    }

    fn read_char(&mut self) -> char {
        let pos = self.pos;
        if pos >= self.data.len() {
            '\0'
        } else {
            self.data[pos]
        }
    }

    fn next_token(&mut self) -> Token {
        while self.pos <= self.data.len() {
            let begin_byte = self.byte_pos;
            let ch = self.advance();
            let loc = self.loc;

            let tok = match ch {
                '/' if self.read_char() == '/' => {
                    while self.advance() != '\n' {}
                    continue;
                }
                '#' => {
                    let ch = self.read_char();
                    if self.byte_pos == 1 && ch == '!' {
                        while self.advance() != '\n' {}
                        continue;
                    }
                    loop {
                        let ch = self.read_char();
                        if ch.is_alphanumeric() || ch == '_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    Token::new(
                        TokenKind::Directive,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '-' if self.read_char() == '>' => {
                    self.advance();
                    Token::new(
                        TokenKind::Arrow,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '=' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(
                        TokenKind::EqEq,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                ':' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(
                        TokenKind::Assign,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '<' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(
                        TokenKind::LtEq,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '>' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(
                        TokenKind::GtEq,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '!' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(
                        TokenKind::NotEq,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '&' if self.read_char() == '&' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoubleAmpersand,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '|' if self.read_char() == '|' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoublePipe,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                ':' if self.read_char() == ':' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoubleColon,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                '.' if self.read_char() == '.' && self.read_char() == '.' => {
                    self.advance();
                    self.advance();
                    Token::new(
                        TokenKind::Ellipsis,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    )
                }
                ch if ch.is_alphabetic() || ch == '_' => return self.lex_identifier(begin_byte),
                '0'..='9' => return self.lex_number(begin_byte),
                '"' => return self.lex_string(begin_byte),

                ',' => Token::new(
                    TokenKind::Comma,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                ';' => Token::new(
                    TokenKind::SemiColon,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                ':' => Token::new(
                    TokenKind::Colon,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '\\' => Token::new(
                    TokenKind::BackSlash,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '=' => Token::new(
                    TokenKind::Eq,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '<' => Token::new(
                    TokenKind::Lt,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '>' => Token::new(
                    TokenKind::Gt,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '!' => Token::new(
                    TokenKind::Bang,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '+' => {
                    let next = self.read_char();
                    if next == '+' {
                        self.advance();
                        Token::new(
                            TokenKind::Concat,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else if next == '=' {
                        self.advance();
                        Token::new(
                            TokenKind::PlusEq,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else {
                        Token::new(
                            TokenKind::Plus,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    }
                }
                '-' => {
                    let next = self.read_char();
                    if next == '>' {
                        self.advance();
                        Token::new(
                            TokenKind::Arrow,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else if next == '=' {
                        self.advance();
                        Token::new(
                            TokenKind::MinusEq,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else {
                        Token::new(
                            TokenKind::Minus,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    }
                }
                '.' => Token::new(
                    TokenKind::Dot,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '*' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(
                            TokenKind::AsteriskEq,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else {
                        Token::new(
                            TokenKind::Asterisk,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    }
                }
                '/' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(
                            TokenKind::SlashEq,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else {
                        Token::new(
                            TokenKind::Slash,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    }
                }
                '%' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(
                            TokenKind::ModEq,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    } else {
                        Token::new(
                            TokenKind::Mod,
                            loc,
                            self.source[begin_byte..self.byte_pos].into(),
                        )
                    }
                }
                '$' => Token::new(
                    TokenKind::Dollar,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '@' => Token::new(
                    TokenKind::At,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '?' => Token::new(
                    TokenKind::Question,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '&' => Token::new(
                    TokenKind::Ampersand,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '^' => Token::new(
                    TokenKind::Caret,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '|' => Token::new(
                    TokenKind::Pipe,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '(' => Token::new(
                    TokenKind::OpenParen,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                ')' => Token::new(
                    TokenKind::CloseParen,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '[' => Token::new(
                    TokenKind::OpenBracket,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                ']' => Token::new(
                    TokenKind::CloseBracket,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '{' => Token::new(
                    TokenKind::OpenCurly,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '}' => Token::new(
                    TokenKind::CloseCurly,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),

                ch if ch.is_whitespace() => continue,
                '\0' => return Token::new(TokenKind::EOF, self.loc, "\0".into()),
                _ => {
                    return Token::new(
                        TokenKind::UnexpectedCharacter,
                        self.loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    );
                }
            };
            return tok;
        }

        Token::new(TokenKind::EOF, self.loc, "".into())
    }

    fn lex_identifier(&mut self, begin_byte: usize) -> Token {
        let loc = self.loc;
        #[allow(unused_mut)]
        let mut kind = TokenKind::Identifier;
        loop {
            let ch = self.read_char();
            if ch.is_alphanumeric() || ch == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let ident = &self.source[begin_byte..self.byte_pos];

        if self.keywords.contains(&ident) {
            kind = TokenKind::Keyword;
        }

        Token::new(kind, loc, ident.into())
    }

    fn lex_number(&mut self, begin_byte: usize) -> Token {
        let loc = self.loc;
        let end;
        let mut base = 10;

        // Check for base prefix (0x, 0b, 0o)
        let next = self.read_char();
        match next {
            'x' | 'X' => {
                base = 16;
                self.advance(); // 0
                self.advance(); // x
            }
            'b' | 'B' => {
                base = 2;
                self.advance(); // 0
                self.advance(); // b
            }
            'o' | 'O' => {
                base = 8;
                self.advance(); // 0
                self.advance(); // o
            }
            _ => {}
        }

        // Read digits according to base
        loop {
            let c = self.read_char();
            let valid = match base {
                2 => matches!(c, '0' | '1'),
                8 => matches!(c, '0'..='7'),
                10 if c == '.' => {
                    self.advance();
                    loop {
                        let c = self.read_char();
                        if !c.is_ascii_digit() {
                            break;
                        }
                        self.advance();
                    }
                    end = self.byte_pos;
                    let num_str = &self.source[begin_byte..end];
                    return Token::new(TokenKind::RealNumber, loc, (*num_str).into());
                }
                10 => c.is_ascii_digit(),
                16 => c.is_ascii_hexdigit(),
                _ => false,
            };
            if !valid {
                break;
            }
            self.advance();
        }

        end = self.byte_pos;

        let num_str = &self.source[begin_byte..end]
            .trim_start_matches("0x")
            .trim_start_matches("0X")
            .trim_start_matches("0b")
            .trim_start_matches("0B")
            .trim_start_matches("0o")
            .trim_start_matches("0O");
        let kind = TokenKind::Number(NumberBase::from(base));

        Token::new(kind, loc, (*num_str).into())
    }

    fn lex_string(&mut self, begin_byte: usize) -> Token {
        // let mut buffer = String::new();
        let loc = self.loc;
        loop {
            let ch = self.read_char();
            match ch {
                '"' => {
                    self.advance();
                    break;
                }
                '\0' => {
                    return Token::new(
                        TokenKind::UnterminatedStringLiteral,
                        loc,
                        self.source[begin_byte..self.byte_pos].into(),
                    );
                }
                '\\' => {
                    self.advance();
                    let esc = self.read_char();
                    match esc {
                        'r' => {}  // buffer.push('\r'),
                        'n' => {}  // buffer.push('\n'),
                        '"' => {}  // buffer.push('"'),
                        '\'' => {} // buffer.push('\''),
                        '\\' => {} // buffer.push('\\'),
                        '0' => {}  // buffer.push('\0'),
                        _ => {
                            return Token::new(
                                TokenKind::InvalidEscapeSequence,
                                loc,
                                self.source[begin_byte..self.byte_pos].into(),
                            );
                        }
                    }
                }
                _ => {} // buffer.push(ch as char),
            }
            self.advance();
        }

        Token::new(
            TokenKind::StringLiteral,
            loc,
            self.source[begin_byte..self.byte_pos].into(),
        )
    }
}

/// A type representing a token's source string, which can be either an owned `String`
/// or a leaked `&'static str` when the `interning` feature is enabled.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenSource(
    #[cfg(feature = "interning")] pub &'static str,
    #[cfg(not(feature = "interning"))] pub String,
);

impl std::ops::Deref for TokenSource {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        #[cfg(feature = "interning")]
        {
            self.0
        }
        #[cfg(not(feature = "interning"))]
        {
            &self.0
        }
    }
}

impl fmt::Display for TokenSource {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl From<&str> for TokenSource {
    #[inline]
    fn from(s: &str) -> Self {
        #[cfg(feature = "interning")]
        {
            Self(intern(s))
        }
        #[cfg(not(feature = "interning"))]
        {
            Self(s.to_string())
        }
    }
}

impl From<String> for TokenSource {
    #[inline]
    fn from(s: String) -> Self {
        #[cfg(feature = "interning")]
        {
            Self(intern(&s))
        }
        #[cfg(not(feature = "interning"))]
        {
            Self(s)
        }
    }
}

impl From<&String> for TokenSource {
    #[inline]
    fn from(s: &String) -> Self {
        #[cfg(feature = "interning")]
        {
            Self(intern(s.as_str()))
        }
        #[cfg(not(feature = "interning"))]
        {
            Self(s.clone())
        }
    }
}

#[cfg(feature = "interning")]
static INTERNER: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<&'static str>>> =
    std::sync::OnceLock::new();

#[cfg(feature = "interning")]
fn intern(s: &str) -> &'static str {
    let mut interner = INTERNER
        .get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()))
        .lock()
        .unwrap();
    if let Some(interned) = interner.get(s) {
        interned
    } else {
        let leaked: &'static str = Box::leak(s.to_string().into_boxed_str());
        interner.insert(leaked);
        leaked
    }
}

/// Represents a single analyzed token with its kind, source location, and original string segment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    // source: &'static str,
    pub source: TokenSource,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::UnexpectedCharacter => {
                write!(f, "Unexpected Character `{}`", self.source.escape_default())
            }
            TokenKind::InvalidEscapeSequence => {
                write!(
                    f,
                    "Invalid Escape Sequence `{}`",
                    self.source.escape_default()
                )
            }
            TokenKind::UnterminatedStringLiteral => {
                write!(
                    f,
                    "Unterminated String Literal `{}`",
                    self.source.escape_default()
                )
            }
            TokenKind::StringLiteral => write!(f, "{}", self.source.escape_default()),
            TokenKind::CharacterLiteral => write!(f, "{}", self.source.escape_default()),
            _ => write!(f, "{}", self.source),
        }
    }
}

impl Token {
    /// Returns a string slice of the original text this token represents.
    pub fn source(&self) -> &str {
        // unsafe { transmute::<&'static str, &str>(self.source) }
        &self.source
    }

    /// Creates a new `Token` from a given kind, location, and source string.
    pub fn new(kind: TokenKind, loc: Loc, source: TokenSource) -> Self {
        Self {
            kind,
            loc,
            // source: unsafe { transmute::<&str, &'static str>(source) },
            source,
        }
    }

    /// Returns whether this token represents the End of File (`EOF`).
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }

    /// Attempts to unescape this token as a string literal.
    pub fn unescape(&self) -> String {
        match self.kind {
            TokenKind::StringLiteral => token_string_unescape(self.source()),
            _ => todo!(),
        }
    }
}
pub fn token_string_unescape(source: &str) -> String {
    let mut buffer = String::new();
    let mut esc = false;
    let mut src = source.chars();
    src.next();
    for ch in src {
        match ch {
            ch if esc => {
                match ch {
                    'r' => buffer.push('\r'),
                    'n' => buffer.push('\n'),
                    '"' => buffer.push('"'),
                    '\'' => buffer.push('\''),
                    '\\' => buffer.push('\\'),
                    '0' => buffer.push('\0'),
                    _ => return buffer,
                }
                esc = false;
            }
            '"' => return buffer,
            '\\' => {
                esc = true;
                continue;
            }
            _ => buffer.push(ch),
        }
    }
    buffer
}

/// The specific type or category of a parsed token.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    #[default]
    EOF,
    UnexpectedCharacter,
    InvalidEscapeSequence,
    UnterminatedStringLiteral,

    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,

    Identifier,
    Keyword,

    Directive,

    RealNumber,
    StringLiteral,
    CharacterLiteral,

    Dot,
    Ellipsis,
    Comma,
    Colon,
    DoubleColon,
    SemiColon,
    Arrow,
    BackSlash,

    Assign,
    PlusEq,
    MinusEq,
    AsteriskEq,
    SlashEq,
    ModEq,
    Bang,
    Plus,
    Concat,
    Minus,
    Asterisk,
    Slash,
    Eq,
    EqEq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Mod,
    Ampersand,
    Pipe,
    Caret,
    DoubleAmpersand,
    DoublePipe,

    Dollar,
    At,
    Question,
    InvalidNumber,

    Number(NumberBase),
}

/// The numerical base of a parsed number token (e.g., Binary, Octal, Decimal, Hexadecimal).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberBase {
    B,
    O,
    D,
    X,
}
impl NumberBase {
    pub fn radix(&self) -> u32 {
        match self {
            NumberBase::B => 2,
            NumberBase::O => 8,
            NumberBase::D => 10,
            NumberBase::X => 16,
        }
    }
}

impl From<u32> for NumberBase {
    fn from(value: u32) -> Self {
        match value {
            2 => Self::B,
            8 => Self::O,
            10 => Self::D,
            16 => Self::X,
            _ => panic!("Unkwon base"),
        }
    }
}

impl From<NumberBase> for u32 {
    fn from(val: NumberBase) -> Self {
        match val {
            NumberBase::B => 2,
            NumberBase::O => 8,
            NumberBase::D => 10,
            NumberBase::X => 16,
        }
    }
}

impl TokenKind {
    pub fn is_assign_kind(&self) -> bool {
        matches!(
            self,
            Self::Assign
                | Self::Eq
                | Self::PlusEq
                | Self::MinusEq
                | Self::AsteriskEq
                | Self::SlashEq
                | Self::ModEq
        )
    }
}

/// Captures physical location in the parsed source, specifically the line and column number.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
    #[cfg(feature = "interning")]
    file_path: &'static str
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #[cfg(feature = "interning")]
        {
            write!(f, "{}:{}:{}", self.file_path, self.line, self.col)
        }
        #[cfg(not(feature = "interning"))]
        {
            write!(f, "{}:{}", self.line, self.col)
        }
    }
}

impl Loc {
    #[cfg(feature = "interning")]
    pub fn new(file_path: impl Into<String>, line: usize, col: usize) -> Self {
        Self { file_path: intern(&file_path.into()), line, col }
    }
    #[cfg(not(feature = "interning"))]
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    pub fn next_column(&mut self) {
        self.col += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.col = 1;
    }

    pub fn next(&mut self, c: char) {
        match c {
            '\n' => self.next_line(),
            '\t' => {
                let ts = 8;
                self.col = (self.col / ts) * ts + ts;
            }
            c if c.is_control() => {}
            _ => {
                // For proper UTF-8 support, we could use unicode-width crate
                // to get the display width of characters, but for simplicity
                // we'll treat all non-control characters as width 1
                self.next_column();
            }
        }
    }

    #[cfg(feature = "interning")]
    pub fn file_path(&self) -> &&'static str {
        &self.file_path
    }
}

#[cfg(not(feature = "interning"))]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_init_and_eof() {
        let mut lexer = Lexer::new("");
        let tok = lexer.next();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert!(tok.is_eof());

        let tok2 = lexer.next();
        assert_eq!(tok2.kind, TokenKind::EOF);
    }

    #[test]
    fn test_lexer_peek() {
        let mut lexer = Lexer::new("abc");
        let peeked = lexer.peek().clone();
        assert_eq!(peeked.kind, TokenKind::Identifier);
        assert_eq!(peeked.source(), "abc");
        let next = lexer.next();
        assert_eq!(next, peeked);
        assert_eq!(lexer.next().kind, TokenKind::EOF);
    }

    #[test]
    fn test_comment_skipping() {
        let source = "   // this is a line comment\n  identifier";
        let mut lexer = Lexer::new(source);
        let tok = lexer.next();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.source(), "identifier");
        assert_eq!(tok.loc.line, 2);
        assert_eq!(lexer.next().kind, TokenKind::EOF);
    }

    #[test]
    fn test_shebang_skipping() {
        let source = "#!/usr/bin/env rust\nidentifier";
        let mut lexer = Lexer::new(source);
        let tok = lexer.next();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.source(), "identifier");
        assert_eq!(tok.loc.line, 2);
    }

    #[test]
    fn test_keywords() {
        let source = "var let my_ident";
        let mut lexer = Lexer::new(source).with_keywords(&["var", "let"]);
        let t1 = lexer.next();
        assert_eq!(t1.kind, TokenKind::Keyword);
        assert_eq!(t1.source(), "var");
        let t2 = lexer.next();
        assert_eq!(t2.kind, TokenKind::Keyword);
        assert_eq!(t2.source(), "let");
        let t3 = lexer.next();
        assert_eq!(t3.kind, TokenKind::Identifier);
        assert_eq!(t3.source(), "my_ident");
    }

    #[test]
    fn test_identifiers() {
        let source = "a _a a123 _123_abc";
        let mut lexer = Lexer::new(source);
        let idents = ["a", "_a", "a123", "_123_abc"];
        for expected in idents {
            let tok = lexer.next();
            assert_eq!(tok.kind, TokenKind::Identifier);
            assert_eq!(tok.source(), expected);
        }
    }

    #[test]
    fn test_location_tracking() {
        let source = "a\n\tb";
        let mut lexer = Lexer::new(source);
        let t1 = lexer.next();
        assert_eq!(t1.source(), "a");
        assert_eq!(t1.loc, Loc::new(1, 2));
        let t2 = lexer.next();
        assert_eq!(t2.source(), "b");
        // After 'a', we have '\n'. line becomes 2, col becomes 1.
        // Then we have '\t'. col is calculated as:
        // self.col = (self.col / ts) * ts + ts;
        // ts = 8. (1 / 8)*8 + 8 = 8.
        // And when 'b' is read, self.loc.next('b') advances the col to 9.
        assert_eq!(t2.loc, Loc::new(2, 9));
    }

    #[test]
    fn test_multi_char_operators() {
        let source = "-> == := <= >= != && || :: ...";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next().kind, TokenKind::Arrow);
        assert_eq!(lex.next().kind, TokenKind::EqEq);
        assert_eq!(lex.next().kind, TokenKind::Assign);
        assert_eq!(lex.next().kind, TokenKind::LtEq);
        assert_eq!(lex.next().kind, TokenKind::GtEq);
        assert_eq!(lex.next().kind, TokenKind::NotEq);
        assert_eq!(lex.next().kind, TokenKind::DoubleAmpersand);
        assert_eq!(lex.next().kind, TokenKind::DoublePipe);
        assert_eq!(lex.next().kind, TokenKind::DoubleColon);
        assert_eq!(lex.next().kind, TokenKind::Ellipsis);
    }

    #[test]
    fn test_single_and_compound_operators() {
        let source = ", ; : \\ = < > ! + ++ += - -= . * *= / /= % %= $ & ^ | ( ) [ ] { } @ ?";
        let mut lex = Lexer::new(source);
        assert_eq!(lex.next().kind, TokenKind::Comma);
        assert_eq!(lex.next().kind, TokenKind::SemiColon);
        assert_eq!(lex.next().kind, TokenKind::Colon);
        assert_eq!(lex.next().kind, TokenKind::BackSlash);
        assert_eq!(lex.next().kind, TokenKind::Eq);
        assert_eq!(lex.next().kind, TokenKind::Lt);
        assert_eq!(lex.next().kind, TokenKind::Gt);
        assert_eq!(lex.next().kind, TokenKind::Bang);
        assert_eq!(lex.next().kind, TokenKind::Plus);
        assert_eq!(lex.next().kind, TokenKind::Concat); // ++
        assert_eq!(lex.next().kind, TokenKind::PlusEq); // +=
        assert_eq!(lex.next().kind, TokenKind::Minus);
        assert_eq!(lex.next().kind, TokenKind::MinusEq); // -=
        assert_eq!(lex.next().kind, TokenKind::Dot);
        assert_eq!(lex.next().kind, TokenKind::Asterisk);
        assert_eq!(lex.next().kind, TokenKind::AsteriskEq); // *=
        assert_eq!(lex.next().kind, TokenKind::Slash);
        assert_eq!(lex.next().kind, TokenKind::SlashEq); // /=
        assert_eq!(lex.next().kind, TokenKind::Mod);
        assert_eq!(lex.next().kind, TokenKind::ModEq); // %=
        assert_eq!(lex.next().kind, TokenKind::Dollar);
        assert_eq!(lex.next().kind, TokenKind::Ampersand);
        assert_eq!(lex.next().kind, TokenKind::Caret);
        assert_eq!(lex.next().kind, TokenKind::Pipe);
        assert_eq!(lex.next().kind, TokenKind::OpenParen);
        assert_eq!(lex.next().kind, TokenKind::CloseParen);
        assert_eq!(lex.next().kind, TokenKind::OpenBracket);
        assert_eq!(lex.next().kind, TokenKind::CloseBracket);
        assert_eq!(lex.next().kind, TokenKind::OpenCurly);
        assert_eq!(lex.next().kind, TokenKind::CloseCurly);
        assert_eq!(lex.next().kind, TokenKind::At);
        assert_eq!(lex.next().kind, TokenKind::Question);
    }

    #[test]
    fn test_directives() {
        let mut lex = Lexer::new("#define ABC");
        let tok = lex.next();
        assert_eq!(tok.kind, TokenKind::Directive);
        assert_eq!(tok.source(), "#define");

        let mut lex2 = Lexer::new("#!/bin/bash\n#include");
        let tok2 = lex2.next();
        assert_eq!(tok2.kind, TokenKind::Directive);
        assert_eq!(tok2.source(), "#include");

        let mut lex3 = Lexer::new(" #!");
        let tok3 = lex3.next();
        assert_eq!(tok3.kind, TokenKind::Directive);
        assert_eq!(tok3.source(), "#");
    }

    #[test]
    fn test_numeric_bases() {
        let source = "123 0b101 0o755 0xFF 1.23";
        let mut lex = Lexer::new(source);

        let t1 = lex.next();
        assert_eq!(t1.kind, TokenKind::Number(NumberBase::D));
        assert_eq!(t1.source(), "123");

        let t2 = lex.next();
        assert_eq!(t2.kind, TokenKind::Number(NumberBase::B));
        assert_eq!(t2.source(), "101");

        let t3 = lex.next();
        assert_eq!(t3.kind, TokenKind::Number(NumberBase::O));
        assert_eq!(t3.source(), "755");

        let t4 = lex.next();
        assert_eq!(t4.kind, TokenKind::Number(NumberBase::X));
        assert_eq!(t4.source(), "FF");

        let t5 = lex.next();
        assert_eq!(t5.kind, TokenKind::RealNumber);
        assert_eq!(t5.source(), "1.23");
    }

    #[test]
    fn test_number_base_conversions() {
        assert_eq!(NumberBase::B.radix(), 2);
        assert_eq!(NumberBase::O.radix(), 8);
        assert_eq!(NumberBase::D.radix(), 10);
        assert_eq!(NumberBase::X.radix(), 16);

        assert_eq!(NumberBase::from(2), NumberBase::B);
        assert_eq!(NumberBase::from(8), NumberBase::O);
        assert_eq!(NumberBase::from(10), NumberBase::D);
        assert_eq!(NumberBase::from(16), NumberBase::X);

        assert_eq!(u32::from(NumberBase::B), 2);
        assert_eq!(u32::from(NumberBase::O), 8);
        assert_eq!(u32::from(NumberBase::D), 10);
        assert_eq!(u32::from(NumberBase::X), 16);
    }

    #[test]
    #[should_panic(expected = "Unkwon base")]
    fn test_number_base_panic() {
        let _ = NumberBase::from(3);
    }

    #[test]
    fn test_string_literals() {
        let mut lex = Lexer::new("\"hello\"");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::StringLiteral);
        assert_eq!(t.source(), "\"hello\"");
        assert_eq!(t.unescape(), "hello");

        let mut lex = Lexer::new("\"hello\\nworld\"");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::StringLiteral);
        assert_eq!(t.unescape(), "hello\nworld");

        let mut lex = Lexer::new("\"hello\\x\"");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::InvalidEscapeSequence);
        assert_eq!(t.source(), "\"hello\\");

        let mut lex = Lexer::new("\"hello");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::UnterminatedStringLiteral);
        assert_eq!(t.source(), "\"hello");
    }

    #[test]
    fn test_token_helpers_and_display() {
        let loc = Loc::new(5, 10);
        let token = Token::new(TokenKind::Identifier, loc, "foo".into());
        assert!(!token.is_eof());
        assert_eq!(format!("{}", loc), "5:10");
        assert_eq!(format!("{}", token), "foo");

        let eof_token = Token::new(TokenKind::EOF, loc, "".into());
        assert!(eof_token.is_eof());
        assert_eq!(format!("{}", eof_token), "EOF");

        let err_token = Token::new(TokenKind::UnexpectedCharacter, loc, "@".into());
        assert_eq!(format!("{}", err_token), "Unexpected Character `@`");

        let esc_err = Token::new(TokenKind::InvalidEscapeSequence, loc, "\\x".into());
        assert_eq!(format!("{}", esc_err), "Invalid Escape Sequence `\\\\x`");

        let unterminated = Token::new(TokenKind::UnterminatedStringLiteral, loc, "\"abc".into());
        assert_eq!(
            format!("{}", unterminated),
            "Unterminated String Literal `\\\"abc`"
        );

        let str_tok = Token::new(TokenKind::StringLiteral, loc, "\"abc\"".into());
        assert_eq!(format!("{}", str_tok), "\\\"abc\\\"");

        let char_tok = Token::new(TokenKind::CharacterLiteral, loc, "'a'".into());
        assert_eq!(format!("{}", char_tok), "\\'a\\'");
    }

    #[test]
    fn test_is_assign_kind() {
        assert!(TokenKind::Assign.is_assign_kind());
        assert!(TokenKind::Eq.is_assign_kind());
        assert!(TokenKind::PlusEq.is_assign_kind());
        assert!(TokenKind::MinusEq.is_assign_kind());
        assert!(TokenKind::AsteriskEq.is_assign_kind());
        assert!(TokenKind::SlashEq.is_assign_kind());
        assert!(TokenKind::ModEq.is_assign_kind());
        assert!(!TokenKind::Plus.is_assign_kind());
        assert!(!TokenKind::Identifier.is_assign_kind());
    }

    #[test]
    fn test_unexpected_character() {
        let mut lex = Lexer::new("~");
        let t = lex.next();
        assert_eq!(t.kind, TokenKind::UnexpectedCharacter);
        assert_eq!(t.source(), "~");
    }
}

#[cfg(feature = "interning")]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning_pointer_equality() {
        let source = "my_var my_var";
        let mut lex = Lexer::new(file!(), source);
        let t1 = lex.next();
        let t2 = lex.next();
        assert_eq!(t1.source(), "my_var");
        assert_eq!(t2.source(), "my_var");

        #[cfg(feature = "interning")]
        {
            let p1 = t1.source.0;
            let p2 = t2.source.0;
            assert!(std::ptr::eq(p1, p2));
        }
    }
}
