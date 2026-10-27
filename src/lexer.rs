//! Provides lexical analysis features.
//!
//! This module contains the `Lexer` and the tokens it generates.

use std::fmt;

/// A custom rule function for lexing custom tokens.
pub type RuleFn = for<'a, 'src> fn(&'a mut Lexer<'src>) -> Option<Token<'src>>;

/// A stream-based lexical analyzer capable of interpreting string sources.
///
/// `Lexer` sequentially reads the underlying string slice and produces
/// tokens on demand via the [`next_token()`](Self::next_token) and [`peek()`](Self::peek) methods.
#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    byte_pos: usize,
    loc: Loc,
    peeked: Option<Token<'src>>,
    keywords: Vec<&'src str>,
    pre_rules: Vec<RuleFn>,
    post_rules: Vec<RuleFn>,
    in_pre_rule: bool,
    in_post_rule: bool,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LexerSavePoint<'src> {
    byte_pos: usize,
    loc: Loc,
    peeked: Option<Token<'src>>,
}

impl<'src> Lexer<'src> {
    /// Creates a new `Lexer` given a file path and a source string slice.
    pub fn new(file_path: impl Into<String>, source: &'src str) -> Self {
        Self {
            source,
            loc: Loc::new(file_path, 1, 1),
            byte_pos: 0,
            peeked: None,
            keywords: Vec::new(),
            pre_rules: Vec::new(),
            post_rules: Vec::new(),
            in_pre_rule: false,
            in_post_rule: false,
        }
    }

    /// Creates a new `Lexer` from a source string slice using `"<anonymous>"` as the default file path.
    pub fn from_source(source: &'src str) -> Self {
        Self::new("<anonymous>", source)
    }

    pub fn save(&self) -> LexerSavePoint<'src> {
        LexerSavePoint {
            byte_pos: self.byte_pos,
            loc: self.loc,
            peeked: self.peeked,
        }
    }

    pub fn restore(&mut self, savepoint: LexerSavePoint<'src>) {
        let LexerSavePoint {
            byte_pos,
            loc,
            peeked,
        } = savepoint;
        self.byte_pos = byte_pos;
        self.loc = loc;
        self.peeked = peeked;
    }

    /// Configures the lexer with a set of predefined keywords to recognize.
    pub fn with_keywords(mut self, keywords: &[&'src str]) -> Self {
        self.keywords = keywords.to_vec();
        self
    }

    /// Registers a custom rule to be run before standard token matching.
    pub fn with_pre_rule(mut self, rule: RuleFn) -> Self {
        self.pre_rules.push(rule);
        self
    }

    /// Registers a custom rule to be run after standard token matching (as a fallback).
    pub fn with_post_rule(mut self, rule: RuleFn) -> Self {
        self.post_rules.push(rule);
        self
    }

    /// Adds a custom rule to be run before standard token matching.
    pub fn add_pre_rule(&mut self, rule: RuleFn) {
        self.pre_rules.push(rule);
    }

    /// Adds a custom rule to be run after standard token matching (as a fallback).
    pub fn add_post_rule(&mut self, rule: RuleFn) {
        self.post_rules.push(rule);
    }

    /// Returns the next token in the stream, consuming it in the process.
    /// If an EOF is reached, it will continue to return `EOF` tokens.
    pub fn next_token(&mut self) -> Token<'src> {
        if let Some(peek) = self.peeked.take() {
            peek
        } else {
            self._next_token()
        }
    }

    /// Returns a reference to the next token without consuming it.
    /// Subsequent calls to `peek()` or `next_token()` will return this same token.
    pub fn peek(&mut self) -> &Token<'src> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next_token());
        }
        self.peeked.as_ref().unwrap()
    }

    pub fn advance_by_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn advance(&mut self) -> char {
        if let Some(ch) = self
            .source
            .get(self.byte_pos..)
            .and_then(|s| s.chars().next())
        {
            self.byte_pos += ch.len_utf8();
            self.loc.next(ch);
            ch
        } else {
            '\0'
        }
    }

    fn read_char(&self) -> char {
        self.source
            .get(self.byte_pos..)
            .and_then(|s| s.chars().next())
            .unwrap_or('\0')
    }

    #[inline]
    fn slice_source(&self, begin_byte: usize) -> TokenSource<'src> {
        (&self.source[begin_byte..self.byte_pos]).into()
    }

    fn _next_token(&mut self) -> Token<'src> {
        while self.byte_pos < self.source.len() {
            if !self.in_pre_rule {
                self.in_pre_rule = true;
                let num_pre = self.pre_rules.len();
                for i in 0..num_pre {
                    let rule = self.pre_rules[i];
                    let savepoint = self.save();
                    if let Some(tok) = rule(self) {
                        self.in_pre_rule = false;
                        return tok;
                    }
                    self.restore(savepoint);
                }
                self.in_pre_rule = false;
            }

            let begin_byte = self.byte_pos;
            let loc = self.loc;
            let ch = self.advance();

            let tok = match ch {
                '/' if self.read_char() == '/' => {
                    while !matches!(self.advance(), '\n' | '\0') {}
                    continue;
                }
                '#' => {
                    let ch = self.read_char();
                    if begin_byte == 0 && ch == '!' {
                        while !matches!(self.advance(), '\n' | '\0') {}
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
                    Token::new(TokenKind::Directive, loc, self.slice_source(begin_byte))
                }
                '-' if self.read_char() == '>' => {
                    self.advance();
                    Token::new(TokenKind::Arrow, loc, self.slice_source(begin_byte))
                }
                '=' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(TokenKind::Eq, loc, self.slice_source(begin_byte))
                }
                ':' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(TokenKind::ColonAssign, loc, self.slice_source(begin_byte))
                }
                '<' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(TokenKind::LtEq, loc, self.slice_source(begin_byte))
                }
                '>' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(TokenKind::GtEq, loc, self.slice_source(begin_byte))
                }
                '!' if self.read_char() == '=' => {
                    self.advance();
                    Token::new(TokenKind::NotEq, loc, self.slice_source(begin_byte))
                }
                '&' if self.read_char() == '&' => {
                    self.advance();
                    Token::new(
                        TokenKind::DoubleAmpersand,
                        loc,
                        self.slice_source(begin_byte),
                    )
                }
                '|' if self.read_char() == '|' => {
                    self.advance();
                    Token::new(TokenKind::DoublePipe, loc, self.slice_source(begin_byte))
                }
                ':' if self.read_char() == ':' => {
                    self.advance();
                    Token::new(TokenKind::DoubleColon, loc, self.slice_source(begin_byte))
                }
                '.' if self.source[self.byte_pos..].starts_with("..") => {
                    self.advance();
                    self.advance();
                    Token::new(TokenKind::Ellipsis, loc, self.slice_source(begin_byte))
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    return self.lex_identifier(begin_byte, loc);
                }
                '0'..='9' => return self.lex_number(begin_byte, loc, ch),
                '"' => return self.lex_string(begin_byte, loc),
                '\'' => return self.lex_char(begin_byte, loc),

                ',' => Token::new(TokenKind::Comma, loc, self.slice_source(begin_byte)),
                ';' => Token::new(TokenKind::SemiColon, loc, self.slice_source(begin_byte)),
                ':' => Token::new(TokenKind::Colon, loc, self.slice_source(begin_byte)),
                '\\' => Token::new(TokenKind::BackSlash, loc, self.slice_source(begin_byte)),
                '=' => Token::new(TokenKind::Assign, loc, self.slice_source(begin_byte)),
                '<' => Token::new(TokenKind::Lt, loc, self.slice_source(begin_byte)),
                '>' => Token::new(TokenKind::Gt, loc, self.slice_source(begin_byte)),
                '!' => Token::new(TokenKind::Bang, loc, self.slice_source(begin_byte)),
                '+' => {
                    let next = self.read_char();
                    if next == '+' {
                        self.advance();
                        Token::new(TokenKind::Concat, loc, self.slice_source(begin_byte))
                    } else if next == '=' {
                        self.advance();
                        Token::new(TokenKind::PlusEq, loc, self.slice_source(begin_byte))
                    } else {
                        Token::new(TokenKind::Plus, loc, self.slice_source(begin_byte))
                    }
                }
                '-' => {
                    let next = self.read_char();
                    if next == '>' {
                        self.advance();
                        Token::new(TokenKind::Arrow, loc, self.slice_source(begin_byte))
                    } else if next == '=' {
                        self.advance();
                        Token::new(TokenKind::MinusEq, loc, self.slice_source(begin_byte))
                    } else {
                        Token::new(TokenKind::Minus, loc, self.slice_source(begin_byte))
                    }
                }
                '.' => Token::new(TokenKind::Dot, loc, self.slice_source(begin_byte)),
                '*' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(TokenKind::AsteriskEq, loc, self.slice_source(begin_byte))
                    } else {
                        Token::new(TokenKind::Asterisk, loc, self.slice_source(begin_byte))
                    }
                }
                '/' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(TokenKind::SlashEq, loc, self.slice_source(begin_byte))
                    } else {
                        Token::new(TokenKind::Slash, loc, self.slice_source(begin_byte))
                    }
                }
                '%' => {
                    let next = self.read_char();
                    if next == '=' {
                        self.advance();
                        Token::new(TokenKind::ModEq, loc, self.slice_source(begin_byte))
                    } else {
                        Token::new(TokenKind::Mod, loc, self.slice_source(begin_byte))
                    }
                }
                '$' => Token::new(TokenKind::Dollar, loc, self.slice_source(begin_byte)),
                '@' => Token::new(TokenKind::At, loc, self.slice_source(begin_byte)),
                '?' => Token::new(TokenKind::Question, loc, self.slice_source(begin_byte)),
                '&' => Token::new(TokenKind::Ampersand, loc, self.slice_source(begin_byte)),
                '^' => Token::new(TokenKind::Caret, loc, self.slice_source(begin_byte)),
                '|' => Token::new(TokenKind::Pipe, loc, self.slice_source(begin_byte)),
                '(' => Token::new(TokenKind::OpenParen, loc, self.slice_source(begin_byte)),
                ')' => Token::new(TokenKind::CloseParen, loc, self.slice_source(begin_byte)),
                '[' => Token::new(TokenKind::OpenBracket, loc, self.slice_source(begin_byte)),
                ']' => Token::new(TokenKind::CloseBracket, loc, self.slice_source(begin_byte)),
                '{' => Token::new(TokenKind::OpenCurly, loc, self.slice_source(begin_byte)),
                '}' => Token::new(TokenKind::CloseCurly, loc, self.slice_source(begin_byte)),

                ch if ch.is_whitespace() => continue,
                '\0' => return Token::new(TokenKind::EOF, loc, "\0".into()),
                _ => {
                    if !self.in_post_rule {
                        self.in_post_rule = true;
                        let num_post = self.post_rules.len();
                        for i in 0..num_post {
                            let rule = self.post_rules[i];
                            let savepoint = self.save();
                            if let Some(tok) = rule(self) {
                                self.in_post_rule = false;
                                return tok;
                            }
                            self.restore(savepoint);
                        }
                        self.in_post_rule = false;
                    }
                    return Token::new(
                        TokenKind::UnexpectedCharacter,
                        loc,
                        self.slice_source(begin_byte),
                    );
                }
            };
            return tok;
        }

        Token::new(TokenKind::EOF, self.loc, "".into())
    }

    fn lex_identifier(&mut self, begin_byte: usize, loc: Loc) -> Token<'src> {
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

    fn lex_number(&mut self, begin_byte: usize, loc: Loc, first_ch: char) -> Token<'src> {
        let mut base = 10;

        // Check for base prefix (0x, 0b, 0o) only when leading digit is '0'
        if first_ch == '0' {
            match self.read_char() {
                'x' | 'X' => {
                    base = 16;
                    self.advance(); // consume 'x' / 'X'
                }
                'b' | 'B' => {
                    base = 2;
                    self.advance(); // consume 'b' / 'B'
                }
                'o' | 'O' => {
                    base = 8;
                    self.advance(); // consume 'o' / 'O'
                }
                _ => {}
            }
        }

        let digits_start_byte = self.byte_pos;
        let mut is_real = false;

        // Read digits according to base
        loop {
            let c = self.read_char();
            let valid = match base {
                2 => matches!(c, '0' | '1'),
                8 => matches!(c, '0'..='7'),
                10 if c == '.' && !is_real && !self.source[self.byte_pos..].starts_with("..") => {
                    is_real = true;
                    self.advance();
                    while self.read_char().is_ascii_digit() {
                        self.advance();
                    }
                    break;
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

        let next_ch = self.read_char();
        let has_invalid_suffix = next_ch.is_alphanumeric() || next_ch == '_';
        let missing_prefixed_digits = base != 10 && self.byte_pos == digits_start_byte;

        if has_invalid_suffix || missing_prefixed_digits {
            while {
                let c = self.read_char();
                c.is_alphanumeric() || c == '_'
            } {
                self.advance();
            }
            let invalid_str = &self.source[begin_byte..self.byte_pos];
            return Token::new(TokenKind::InvalidNumber, loc, invalid_str.into());
        }

        let end = self.byte_pos;
        if is_real {
            let num_str = &self.source[begin_byte..end];
            return Token::new(TokenKind::RealNumber, loc, num_str.into());
        }

        let num_str = if base == 10 {
            &self.source[begin_byte..end]
        } else {
            &self.source[digits_start_byte..end]
        };
        let kind = TokenKind::Number(NumberBase::try_from(base).unwrap());

        Token::new(kind, loc, num_str.into())
    }

    fn lex_escape_sequence(&mut self) -> bool {
        let esc = self.read_char();
        match esc {
            'r' | 'n' | 't' | '"' | '\'' | '\\' | '0' => {
                self.advance();
                true
            }
            'x' => {
                self.advance(); // consume 'x'
                let h1 = self.read_char();
                if h1.is_ascii_hexdigit() {
                    self.advance();
                    let h2 = self.read_char();
                    if h2.is_ascii_hexdigit() {
                        self.advance();
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            'u' => {
                self.advance(); // consume 'u'
                if self.read_char() == '{' {
                    self.advance();
                    let hex_start = self.byte_pos;
                    let mut has_digits = false;
                    loop {
                        let c = self.read_char();
                        if c.is_ascii_hexdigit() {
                            has_digits = true;
                            self.advance();
                        } else if c == '}' && has_digits {
                            let hex_str = &self.source[hex_start..self.byte_pos];
                            self.advance();
                            return u32::from_str_radix(hex_str, 16)
                                .ok()
                                .and_then(char::from_u32)
                                .is_some();
                        } else {
                            return false;
                        }
                    }
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn lex_string(&mut self, begin_byte: usize, loc: Loc) -> Token<'src> {
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
                        self.slice_source(begin_byte),
                    );
                }
                '\\' => {
                    self.advance(); // consume '\'
                    if !self.lex_escape_sequence() {
                        return Token::new(
                            TokenKind::InvalidEscapeSequence,
                            loc,
                            self.slice_source(begin_byte),
                        );
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }

        Token::new(TokenKind::StringLiteral, loc, self.slice_source(begin_byte))
    }

    fn lex_char(&mut self, begin_byte: usize, loc: Loc) -> Token<'src> {
        let next_ch = self.read_char();

        match next_ch {
            '\'' => {
                // Empty character literal ''
                self.advance();
                Token::new(
                    TokenKind::EmptyCharacterLiteral,
                    loc,
                    self.slice_source(begin_byte),
                )
            }
            '\0' | '\n' => {
                // Unterminated character literal ' or '\n
                Token::new(
                    TokenKind::UnterminatedCharacterLiteral,
                    loc,
                    self.slice_source(begin_byte),
                )
            }
            '\\' => {
                self.advance(); // consume '\'
                if !self.lex_escape_sequence() {
                    return Token::new(
                        TokenKind::InvalidEscapeSequence,
                        loc,
                        self.slice_source(begin_byte),
                    );
                }

                if self.read_char() == '\'' {
                    self.advance();
                    Token::new(
                        TokenKind::CharacterLiteral,
                        loc,
                        self.slice_source(begin_byte),
                    )
                } else {
                    Token::new(
                        TokenKind::UnterminatedCharacterLiteral,
                        loc,
                        self.slice_source(begin_byte),
                    )
                }
            }
            _ => {
                // Single character or UTF-8 character (e.g. 'a', '🔥')
                self.advance();
                if self.read_char() == '\'' {
                    self.advance();
                    Token::new(
                        TokenKind::CharacterLiteral,
                        loc,
                        self.slice_source(begin_byte),
                    )
                } else {
                    Token::new(
                        TokenKind::UnterminatedCharacterLiteral,
                        loc,
                        self.slice_source(begin_byte),
                    )
                }
            }
        }
    }

    pub fn curr_loc(&self) -> Loc {
        self.loc
    }

    pub fn curr_ends_with_char(&self, c: char) -> bool {
        if self.byte_pos > self.source.len() {
            return false;
        }
        self.source[..self.byte_pos].ends_with(c)
    }

    pub fn curr_ends_with_str(&self, s: &str) -> bool {
        if self.byte_pos > self.source.len() {
            return false;
        }
        self.source[..self.byte_pos].ends_with(s)
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.is_eof() { None } else { Some(tok) }
    }
}

/// A type representing a token source string (`&'src str`), which is either
/// borrowed directly when the `zero-copy` feature is enabled or interned by default.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenSource<'src>(pub &'src str);

impl<'src> TokenSource<'src> {
    pub fn as_str(&self) -> &'src str {
        self.0
    }

    /// Attempts to unescape this [`TokenSource`] as a string or character literal,
    /// returning the raw string if it is not quoted.
    pub fn unescape(&self) -> String {
        token_string_unescape(self.0)
    }
}

impl<'src> PartialEq<str> for TokenSource<'src> {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl<'src> PartialEq<&str> for TokenSource<'src> {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}

impl<'src> PartialEq<String> for TokenSource<'src> {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.0 == other
    }
}

impl<'src> std::ops::Deref for TokenSource<'src> {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.0
    }
}

impl<'src> fmt::Display for TokenSource<'src> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

#[cfg(feature = "zero-copy")]
impl<'src> From<&'src str> for TokenSource<'src> {
    #[inline]
    fn from(s: &'src str) -> Self {
        Self(s)
    }
}

#[cfg(not(feature = "zero-copy"))]
impl<'src> From<&str> for TokenSource<'src> {
    #[inline]
    fn from(s: &str) -> Self {
        Self(intern(s))
    }
}

impl<'src> From<String> for TokenSource<'src> {
    #[inline]
    fn from(s: String) -> Self {
        Self(intern(&s))
    }
}

impl<'src> From<&String> for TokenSource<'src> {
    #[inline]
    fn from(s: &String) -> Self {
        Self(intern(s.as_str()))
    }
}

static INTERNER: std::sync::LazyLock<std::sync::Mutex<std::collections::HashSet<&'static str>>> =
    std::sync::LazyLock::new(|| std::sync::Mutex::new(std::collections::HashSet::new()));

fn intern(s: &str) -> &'static str {
    let mut interner = INTERNER.lock().unwrap();
    if let Some(interned) = interner.get(s) {
        interned
    } else {
        let leaked: &'static str = Box::leak(s.to_string().into_boxed_str());
        interner.insert(leaked);
        leaked
    }
}

/// Represents a single analyzed token with its kind, source location, and original string segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub loc: Loc,
    pub source: TokenSource<'src>,
}

impl<'src> fmt::Display for Token<'src> {
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
            TokenKind::UnterminatedCharacterLiteral => {
                write!(
                    f,
                    "Unterminated Character Literal `{}`",
                    self.source.escape_default()
                )
            }
            TokenKind::EmptyCharacterLiteral => {
                write!(f, "Empty Character Literal")
            }
            TokenKind::StringLiteral => write!(f, "{}", self.source.escape_default()),
            TokenKind::CharacterLiteral => write!(f, "{}", self.source.escape_default()),
            _ => write!(f, "{}", self.source),
        }
    }
}

impl<'src> Token<'src> {
    /// Returns a string slice of the original text this token represents.
    pub fn source(&self) -> &'src str {
        self.source.0
    }

    /// Creates a new `Token` from a given kind, location, and source string.
    pub fn new(kind: TokenKind, loc: Loc, source: TokenSource<'src>) -> Self {
        Self { kind, loc, source }
    }

    /// Returns whether this token represents the End of File (`EOF`).
    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }

    /// Attempts to unescape this token if it is a string or character literal,
    /// or returns the raw token source string otherwise.
    pub fn unescape(&self) -> String {
        match self.kind {
            TokenKind::StringLiteral | TokenKind::CharacterLiteral => {
                token_string_unescape(self.source())
            }
            _ => self.source().to_string(),
        }
    }
}

pub fn token_string_unescape(source: &str) -> String {
    let mut chars = source.chars().peekable();
    let quote = match chars.peek().copied() {
        Some(q @ ('"' | '\'')) => {
            chars.next();
            q
        }
        _ => return source.to_string(),
    };

    let mut buffer = String::new();
    while let Some(ch) = chars.next() {
        if ch == quote {
            break;
        }
        if ch == '\\' {
            match chars.next() {
                Some('r') => buffer.push('\r'),
                Some('n') => buffer.push('\n'),
                Some('t') => buffer.push('\t'),
                Some('"') => buffer.push('"'),
                Some('\'') => buffer.push('\''),
                Some('\\') => buffer.push('\\'),
                Some('0') => buffer.push('\0'),
                Some('x') => {
                    let mut hex = String::with_capacity(2);
                    if let Some(h1) = chars.next() {
                        hex.push(h1);
                    }
                    if let Some(h2) = chars.next() {
                        hex.push(h2);
                    }
                    if hex.len() == 2 {
                        if let Ok(val) = u8::from_str_radix(&hex, 16) {
                            buffer.push(val as char);
                        } else {
                            return buffer;
                        }
                    } else {
                        return buffer;
                    }
                }
                Some('u') if chars.peek() == Some(&'{') => {
                    chars.next(); // consume '{'
                    let mut hex = String::new();
                    let mut closed = false;
                    for c in chars.by_ref() {
                        if c == '}' {
                            closed = true;
                            break;
                        }
                        hex.push(c);
                    }
                    if closed
                        && !hex.is_empty()
                        && let Ok(val) = u32::from_str_radix(&hex, 16)
                        && let Some(decoded) = char::from_u32(val)
                    {
                        buffer.push(decoded);
                        continue;
                    }
                    return buffer;
                }
                _ => return buffer,
            }
        } else {
            buffer.push(ch);
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
    UnterminatedCharacterLiteral,
    EmptyCharacterLiteral,

    Custom(u32),
    CustomTag(&'static str),

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

    /// The `:=` operator.
    ColonAssign,
    /// The `=` operator.
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
    /// The `==` operator.
    Eq,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberBaseError {
    UnknownBase,
}

impl fmt::Display for NumberBaseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumberBaseError::UnknownBase => write!(f, "Unknown number base"),
        }
    }
}

impl std::error::Error for NumberBaseError {}

impl TryFrom<u32> for NumberBase {
    type Error = NumberBaseError;
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            2 => Ok(Self::B),
            8 => Ok(Self::O),
            10 => Ok(Self::D),
            16 => Ok(Self::X),
            _ => Err(Self::Error::UnknownBase),
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
            Self::ColonAssign
                | Self::Assign
                | Self::PlusEq
                | Self::MinusEq
                | Self::AsteriskEq
                | Self::SlashEq
                | Self::ModEq
        )
    }
}

/// Captures physical location in the parsed source, specifically the file path, line, and column number.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    pub line: usize,
    pub col: usize,

    file_path: &'static str,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file_path, self.line, self.col)
    }
}

impl Loc {
    pub fn new(file_path: impl Into<String>, line: usize, col: usize) -> Self {
        Self {
            file_path: intern(&file_path.into()),
            line,
            col,
        }
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

    pub fn file_path(&self) -> &'static str {
        self.file_path
    }
}

// #[cfg(not(feature = "interning"))]
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_init_and_eof() {
        let mut lexer = Lexer::new("", "");
        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::EOF);
        assert!(tok.is_eof());

        let tok2 = lexer.next_token();
        assert_eq!(tok2.kind, TokenKind::EOF);
    }

    #[test]
    fn test_lexer_peek() {
        let mut lexer = Lexer::new("<test>", "abc");
        let peeked = lexer.peek().clone();
        assert_eq!(peeked.kind, TokenKind::Identifier);
        assert_eq!(peeked.source(), "abc");
        let next = lexer.next_token();
        assert_eq!(next, peeked);
        assert_eq!(lexer.next_token().kind, TokenKind::EOF);
    }

    #[test]
    fn test_comment_skipping() {
        let source = "   // this is a line comment\n  identifier";
        let mut lexer = Lexer::new("<test>", source);
        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.source(), "identifier");
        assert_eq!(tok.loc.line, 2);
        assert_eq!(lexer.next_token().kind, TokenKind::EOF);
    }

    #[test]
    fn test_shebang_skipping() {
        let source = "#!/usr/bin/env rust\nidentifier";
        let mut lexer = Lexer::new("<test>", source);
        let tok = lexer.next_token();
        assert_eq!(tok.kind, TokenKind::Identifier);
        assert_eq!(tok.source(), "identifier");
        assert_eq!(tok.loc.line, 2);
    }

    #[test]
    fn test_keywords() {
        let source = "var let my_ident";
        let mut lexer = Lexer::new("<test>", source).with_keywords(&["var", "let"]);
        let t1 = lexer.next_token();
        assert_eq!(t1.kind, TokenKind::Keyword);
        assert_eq!(t1.source(), "var");
        let t2 = lexer.next_token();
        assert_eq!(t2.kind, TokenKind::Keyword);
        assert_eq!(t2.source(), "let");
        let t3 = lexer.next_token();
        assert_eq!(t3.kind, TokenKind::Identifier);
        assert_eq!(t3.source(), "my_ident");
    }

    #[test]
    fn test_identifiers() {
        let source = "a _a a123 _123_abc";
        let mut lexer = Lexer::new("<test>", source);
        let idents = ["a", "_a", "a123", "_123_abc"];
        for expected in idents {
            let tok = lexer.next_token();
            assert_eq!(tok.kind, TokenKind::Identifier);
            assert_eq!(tok.source(), expected);
        }
    }

    #[test]
    fn test_location_tracking() {
        let source = "a\n\tb";
        let mut lexer = Lexer::new("<test>", source);
        let t1 = lexer.next_token();
        assert_eq!(t1.source(), "a");
        assert_eq!(t1.loc, Loc::new("<test>", 1, 1));
        assert_eq!(t1.loc.file_path(), "<test>");
        let t2 = lexer.next_token();
        assert_eq!(t2.source(), "b");
        // After 'a', we have '\n'. line becomes 2, col becomes 1.
        // Then we have '\t'. col is calculated as:
        // self.col = (self.col / ts) * ts + ts;
        // ts = 8. (1 / 8)*8 + 8 = 8.
        // 'b' starts at column 8.
        assert_eq!(t2.loc, Loc::new("<test>", 2, 8));
    }

    #[test]
    fn test_multi_char_operators() {
        let source = "-> == := <= >= != && || :: ...";
        let mut lex = Lexer::new("<test>", source);
        assert_eq!(lex.next_token().kind, TokenKind::Arrow);
        assert_eq!(lex.next_token().kind, TokenKind::Eq);
        assert_eq!(lex.next_token().kind, TokenKind::ColonAssign);
        assert_eq!(lex.next_token().kind, TokenKind::LtEq);
        assert_eq!(lex.next_token().kind, TokenKind::GtEq);
        assert_eq!(lex.next_token().kind, TokenKind::NotEq);
        assert_eq!(lex.next_token().kind, TokenKind::DoubleAmpersand);
        assert_eq!(lex.next_token().kind, TokenKind::DoublePipe);
        assert_eq!(lex.next_token().kind, TokenKind::DoubleColon);
        assert_eq!(lex.next_token().kind, TokenKind::Ellipsis);
    }

    #[test]
    fn test_single_and_compound_operators() {
        let source = ", ; : \\ = < > ! + ++ += - -= . * *= / /= % %= $ & ^ | ( ) [ ] { } @ ?";
        let mut lex = Lexer::new("<test>", source);
        assert_eq!(lex.next_token().kind, TokenKind::Comma);
        assert_eq!(lex.next_token().kind, TokenKind::SemiColon);
        assert_eq!(lex.next_token().kind, TokenKind::Colon);
        assert_eq!(lex.next_token().kind, TokenKind::BackSlash);
        assert_eq!(lex.next_token().kind, TokenKind::Assign);
        assert_eq!(lex.next_token().kind, TokenKind::Lt);
        assert_eq!(lex.next_token().kind, TokenKind::Gt);
        assert_eq!(lex.next_token().kind, TokenKind::Bang);
        assert_eq!(lex.next_token().kind, TokenKind::Plus);
        assert_eq!(lex.next_token().kind, TokenKind::Concat); // ++
        assert_eq!(lex.next_token().kind, TokenKind::PlusEq); // +=
        assert_eq!(lex.next_token().kind, TokenKind::Minus);
        assert_eq!(lex.next_token().kind, TokenKind::MinusEq); // -=
        assert_eq!(lex.next_token().kind, TokenKind::Dot);
        assert_eq!(lex.next_token().kind, TokenKind::Asterisk);
        assert_eq!(lex.next_token().kind, TokenKind::AsteriskEq); // *=
        assert_eq!(lex.next_token().kind, TokenKind::Slash);
        assert_eq!(lex.next_token().kind, TokenKind::SlashEq); // /=
        assert_eq!(lex.next_token().kind, TokenKind::Mod);
        assert_eq!(lex.next_token().kind, TokenKind::ModEq); // %=
        assert_eq!(lex.next_token().kind, TokenKind::Dollar);
        assert_eq!(lex.next_token().kind, TokenKind::Ampersand);
        assert_eq!(lex.next_token().kind, TokenKind::Caret);
        assert_eq!(lex.next_token().kind, TokenKind::Pipe);
        assert_eq!(lex.next_token().kind, TokenKind::OpenParen);
        assert_eq!(lex.next_token().kind, TokenKind::CloseParen);
        assert_eq!(lex.next_token().kind, TokenKind::OpenBracket);
        assert_eq!(lex.next_token().kind, TokenKind::CloseBracket);
        assert_eq!(lex.next_token().kind, TokenKind::OpenCurly);
        assert_eq!(lex.next_token().kind, TokenKind::CloseCurly);
        assert_eq!(lex.next_token().kind, TokenKind::At);
        assert_eq!(lex.next_token().kind, TokenKind::Question);
    }

    #[test]
    fn test_directives() {
        let mut lex = Lexer::new("<test>", "#define ABC");
        let tok = lex.next_token();
        assert_eq!(tok.kind, TokenKind::Directive);
        assert_eq!(tok.source(), "#define");

        let mut lex2 = Lexer::new("<test>", "#!/bin/bash\n#include");
        let tok2 = lex2.next_token();
        assert_eq!(tok2.kind, TokenKind::Directive);
        assert_eq!(tok2.source(), "#include");

        let mut lex3 = Lexer::new("<test>", " #!");
        let tok3 = lex3.next_token();
        assert_eq!(tok3.kind, TokenKind::Directive);
        assert_eq!(tok3.source(), "#");
    }

    #[test]
    fn test_numeric_bases() {
        let source = "123 0b101 0o755 0xFF 1.23";
        let mut lex = Lexer::new("<test>", source);

        let t1 = lex.next_token();
        assert_eq!(t1.kind, TokenKind::Number(NumberBase::D));
        assert_eq!(t1.source(), "123");

        let t2 = lex.next_token();
        assert_eq!(t2.kind, TokenKind::Number(NumberBase::B));
        assert_eq!(t2.source(), "101");

        let t3 = lex.next_token();
        assert_eq!(t3.kind, TokenKind::Number(NumberBase::O));
        assert_eq!(t3.source(), "755");

        let t4 = lex.next_token();
        assert_eq!(t4.kind, TokenKind::Number(NumberBase::X));
        assert_eq!(t4.source(), "FF");

        let t5 = lex.next_token();
        assert_eq!(t5.kind, TokenKind::RealNumber);
        assert_eq!(t5.source(), "1.23");
    }

    #[test]
    fn test_invalid_numbers() {
        let mut lex = Lexer::from_source("0x 0b102 0o89 0xGG 9xFF 123abc");

        let t1 = lex.next_token();
        assert_eq!(t1.kind, TokenKind::InvalidNumber);
        assert_eq!(t1.source(), "0x");

        let t2 = lex.next_token();
        assert_eq!(t2.kind, TokenKind::InvalidNumber);
        assert_eq!(t2.source(), "0b102");

        let t3 = lex.next_token();
        assert_eq!(t3.kind, TokenKind::InvalidNumber);
        assert_eq!(t3.source(), "0o89");

        let t4 = lex.next_token();
        assert_eq!(t4.kind, TokenKind::InvalidNumber);
        assert_eq!(t4.source(), "0xGG");

        let t5 = lex.next_token();
        assert_eq!(t5.kind, TokenKind::InvalidNumber);
        assert_eq!(t5.source(), "9xFF");

        let t6 = lex.next_token();
        assert_eq!(t6.kind, TokenKind::InvalidNumber);
        assert_eq!(t6.source(), "123abc");
    }

    #[test]
    fn test_number_base_conversions() {
        assert_eq!(NumberBase::B.radix(), 2);
        assert_eq!(NumberBase::O.radix(), 8);
        assert_eq!(NumberBase::D.radix(), 10);
        assert_eq!(NumberBase::X.radix(), 16);

        assert_eq!(NumberBase::try_from(2), Ok(NumberBase::B));
        assert_eq!(NumberBase::try_from(8), Ok(NumberBase::O));
        assert_eq!(NumberBase::try_from(10), Ok(NumberBase::D));
        assert_eq!(NumberBase::try_from(16), Ok(NumberBase::X));
        assert_eq!(NumberBase::try_from(3), Err(NumberBaseError::UnknownBase));

        assert_eq!(u32::from(NumberBase::B), 2);
        assert_eq!(u32::from(NumberBase::O), 8);
        assert_eq!(u32::from(NumberBase::D), 10);
        assert_eq!(u32::from(NumberBase::X), 16);
    }

    #[test]
    fn test_string_literals() {
        let mut lex = Lexer::new("<test>", "\"hello\"");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::StringLiteral);
        assert_eq!(t.source(), "\"hello\"");
        assert_eq!(t.unescape(), "hello");

        let mut lex = Lexer::new("<test>", "\"hello\\n\\t\\x41\\u{1F525}world\"");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::StringLiteral);
        assert_eq!(t.unescape(), "hello\n\tA🔥world");

        let mut lex = Lexer::new("<test>", "\"hello\\x\"");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::InvalidEscapeSequence);
        assert_eq!(t.source(), "\"hello\\x");

        let mut lex = Lexer::new("<test>", "\"hello");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::UnterminatedStringLiteral);
        assert_eq!(t.source(), "\"hello");
    }

    #[test]
    fn test_token_helpers_and_display() {
        let loc = Loc::new("<test>", 5, 10);
        let token = Token::new(TokenKind::Identifier, loc, "foo".into());
        assert!(!token.is_eof());
        assert_eq!(format!("{}", loc), "<test>:5:10");
        assert_eq!(format!("{}", token), "foo");
        assert_eq!(token.unescape(), "foo");

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
        assert!(TokenKind::ColonAssign.is_assign_kind());
        assert!(TokenKind::Assign.is_assign_kind());
        assert!(TokenKind::PlusEq.is_assign_kind());
        assert!(TokenKind::MinusEq.is_assign_kind());
        assert!(TokenKind::AsteriskEq.is_assign_kind());
        assert!(TokenKind::SlashEq.is_assign_kind());
        assert!(TokenKind::ModEq.is_assign_kind());
        assert!(!TokenKind::Eq.is_assign_kind());
        assert!(!TokenKind::Plus.is_assign_kind());
        assert!(!TokenKind::Identifier.is_assign_kind());
    }

    #[test]
    fn test_unexpected_character() {
        let mut lex = Lexer::new("<test>", "~");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::UnexpectedCharacter);
        assert_eq!(t.source(), "~");
    }

    #[test]
    fn test_savepoint() {
        let mut lex = Lexer::new("<test>", "arr[1]");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::Identifier);
        assert_eq!(t.source(), "arr");
        let savepoint = lex.save();
        let t = lex.next_token();
        if t.kind == TokenKind::OpenParen {
            assert!(false, "test_savepoint");
        } else {
            lex.restore(savepoint);
        }
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::OpenBracket);
        assert_eq!(t.source(), "[");

        // Test savepoint interaction with peek()
        let mut lex2 = Lexer::from_source("a b c");
        let sp = lex2.save();
        assert_eq!(lex2.next_token().source(), "a");
        assert_eq!(lex2.peek().source(), "b");
        lex2.restore(sp);
        assert_eq!(lex2.next_token().source(), "a");

        // Test save() after peek()
        let mut lex3 = Lexer::from_source("x y");
        assert_eq!(lex3.peek().source(), "x");
        let sp3 = lex3.save();
        assert_eq!(lex3.next_token().source(), "x");
        assert_eq!(lex3.next_token().source(), "y");
        lex3.restore(sp3);
        assert_eq!(lex3.next_token().source(), "x");
    }

    #[test]
    fn test_iterator_and_edge_cases() {
        // Test Iterator implementation
        let lex = Lexer::from_source("foo = 10");
        let kinds: Vec<TokenKind> = lex.map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier,
                TokenKind::Assign,
                TokenKind::Number(NumberBase::D),
            ]
        );

        // Test comment and shebang at EOF without newline
        let mut lex_comment = Lexer::from_source("abc // trailing comment without newline");
        assert_eq!(lex_comment.next_token().source(), "abc");
        assert_eq!(lex_comment.next_token().kind, TokenKind::EOF);

        let mut lex_shebang = Lexer::from_source("#!/usr/bin/env bash");
        assert_eq!(lex_shebang.next_token().kind, TokenKind::EOF);

        // Test two dots followed by identifier (`..x`) is two Dots and Identifier, not Ellipsis
        let mut lex_dots = Lexer::from_source("..x ...");
        assert_eq!(lex_dots.next_token().kind, TokenKind::Dot);
        assert_eq!(lex_dots.next_token().kind, TokenKind::Dot);
        assert_eq!(lex_dots.next_token().source(), "x");
        assert_eq!(lex_dots.next_token().kind, TokenKind::Ellipsis);
    }

    #[test]
    fn test_character_literals() {
        let source = "'a' '🔥' '\\n' '\\t' '\\r' '\\\\' '\\'' '\\0' '\\x41' '\\u{1F525}'";
        let mut lex = Lexer::new("<test>", source);

        let t1 = lex.next_token();
        assert_eq!(t1.kind, TokenKind::CharacterLiteral);
        assert_eq!(t1.source(), "'a'");
        assert_eq!(t1.unescape(), "a");

        let t2 = lex.next_token();
        assert_eq!(t2.kind, TokenKind::CharacterLiteral);
        assert_eq!(t2.source(), "'🔥'");
        assert_eq!(t2.unescape(), "🔥");

        let t3 = lex.next_token();
        assert_eq!(t3.kind, TokenKind::CharacterLiteral);
        assert_eq!(t3.source(), "'\\n'");
        assert_eq!(t3.unescape(), "\n");

        let t4 = lex.next_token();
        assert_eq!(t4.kind, TokenKind::CharacterLiteral);
        assert_eq!(t4.source(), "'\\t'");
        assert_eq!(t4.unescape(), "\t");

        let t5 = lex.next_token();
        assert_eq!(t5.kind, TokenKind::CharacterLiteral);
        assert_eq!(t5.source(), "'\\r'");
        assert_eq!(t5.unescape(), "\r");

        let t6 = lex.next_token();
        assert_eq!(t6.kind, TokenKind::CharacterLiteral);
        assert_eq!(t6.source(), "'\\\\'");
        assert_eq!(t6.unescape(), "\\");

        let t7 = lex.next_token();
        assert_eq!(t7.kind, TokenKind::CharacterLiteral);
        assert_eq!(t7.source(), "'\\''");
        assert_eq!(t7.unescape(), "'");

        let t8 = lex.next_token();
        assert_eq!(t8.kind, TokenKind::CharacterLiteral);
        assert_eq!(t8.source(), "'\\0'");
        assert_eq!(t8.unescape(), "\0");

        let t9 = lex.next_token();
        assert_eq!(t9.kind, TokenKind::CharacterLiteral);
        assert_eq!(t9.source(), "'\\x41'");
        assert_eq!(t9.unescape(), "A");

        let t10 = lex.next_token();
        assert_eq!(t10.kind, TokenKind::CharacterLiteral);
        assert_eq!(t10.source(), "'\\u{1F525}'");
        assert_eq!(t10.unescape(), "🔥");
    }

    #[test]
    fn test_character_literal_errors() {
        let mut lex = Lexer::new("<test>", "''");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::EmptyCharacterLiteral);

        let mut lex = Lexer::new("<test>", "'a");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::UnterminatedCharacterLiteral);

        let mut lex = Lexer::new("<test>", "'\\z'");
        let t = lex.next_token();
        assert_eq!(t.kind, TokenKind::InvalidEscapeSequence);
    }

    #[test]
    fn test_custom_rules() {
        fn custom_at_mention<'src>(lex: &mut Lexer<'src>) -> Option<Token<'src>> {
            let save = lex.save();
            let tok = lex.next_token();
            if tok.kind == TokenKind::At {
                let next = lex.next_token();
                if next.kind == TokenKind::Identifier {
                    return Some(Token::new(
                        TokenKind::CustomTag("mention"),
                        tok.loc,
                        format!("@{}", next.source()).into(),
                    ));
                }
            }
            lex.restore(save);
            None
        }

        fn custom_tilde_operator<'src>(lex: &mut Lexer<'src>) -> Option<Token<'src>> {
            let loc = lex.curr_loc();
            if lex.curr_ends_with_char('~') {
                return Some(Token::new(TokenKind::Custom(42), loc, "~".into()));
            }
            None
        }

        let mut lex = Lexer::new("<test>", "@user ~")
            .with_pre_rule(custom_at_mention)
            .with_post_rule(custom_tilde_operator);

        let t1 = lex.next_token();
        assert_eq!(t1.kind, TokenKind::CustomTag("mention"));
        assert_eq!(t1.source(), "@user");

        let t2 = lex.next_token();
        assert_eq!(t2.kind, TokenKind::Custom(42));
        assert_eq!(t2.source(), "~");
    }

    #[test]
    fn test_lexer_helper_methods() {
        let mut lex = Lexer::new("<test>", "abc 123 🔥");
        assert_eq!(lex.curr_loc(), Loc::new("<test>", 1, 1));
        assert!(!lex.curr_ends_with_char('a'));
        assert!(!lex.curr_ends_with_str("abc"));

        lex.advance_by_n(3);
        assert_eq!(lex.curr_loc(), Loc::new("<test>", 1, 4));
        assert!(lex.curr_ends_with_char('c'));
        assert!(lex.curr_ends_with_str("abc"));

        let tok1 = lex.next_token(); // lexer skips space, returns "123"
        assert_eq!(tok1.source(), "123");
        assert!(lex.curr_ends_with_str("123"));

        let tok2 = lex.next_token(); // lexer skips space, returns "🔥"
        assert_eq!(tok2.source(), "🔥");
        assert!(lex.curr_ends_with_char('🔥'));
        assert!(lex.curr_ends_with_str("🔥"));
    }

    #[test]
    fn test_str_token_source_eq() {
        assert_eq!(TokenSource::from("test1"), "test1");
        assert_eq!(TokenSource::from("test2"), "test2".to_string());
        assert_eq!(TokenSource::from("test3").as_str(), "test3");
    }

    #[cfg(not(feature = "zero-copy"))]
    #[test]
    fn test_string_interning_pointer_equality() {
        let source = "my_var my_var";
        let mut lex = Lexer::new(file!(), source);
        let t1 = lex.next_token();
        let t2 = lex.next_token();
        assert_eq!(t1.source(), "my_var");
        assert_eq!(t2.source(), "my_var");

        let p1 = t1.source.0;
        let p2 = t2.source.0;
        assert!(std::ptr::eq(p1, p2));
    }

    #[cfg(feature = "zero-copy")]
    #[test]
    fn test_zero_copy_source_slicing() {
        let source = "my_var my_var";
        let mut lex = Lexer::new(file!(), source);
        let t1 = lex.next_token();
        let t2 = lex.next_token();
        assert_eq!(t1.source(), "my_var");
        assert_eq!(t2.source(), "my_var");

        assert_eq!(t1.source.0.as_ptr(), source[0..6].as_ptr());
        assert_eq!(t2.source.0.as_ptr(), source[7..13].as_ptr());
    }
}
