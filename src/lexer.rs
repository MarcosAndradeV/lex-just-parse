#![allow(unused)]
use std::fmt;

pub struct Lexer<'src> {
    source: &'src str,
    data: Vec<char>,
    pos: usize,
    byte_pos: usize,
    loc: Loc,
    peeked: Option<Token>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            data: source.chars().collect(),
            loc: Loc::new(1, 1),
            pos: 0,
            byte_pos: 0,
            peeked: None,
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(peek) = self.peeked.take() {
            peek
        } else {
            self.next_token()
        }
    }

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
                '+' if self.read_char() == '+' => {
                    self.advance();
                    Token::new(
                        TokenKind::Concat,
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
                ch if ch.is_alphabetic() || ch == '_' => return self.lex_identfier(begin_byte),
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
                '+' => Token::new(
                    TokenKind::Plus,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '-' => Token::new(
                    TokenKind::Minus,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '.' => Token::new(
                    TokenKind::Dot,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '*' => Token::new(
                    TokenKind::Asterisk,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '/' => Token::new(
                    TokenKind::Slash,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '%' => Token::new(
                    TokenKind::Mod,
                    loc,
                    self.source[begin_byte..self.byte_pos].into(),
                ),
                '$' => Token::new(
                    TokenKind::Dollar,
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

    fn lex_identfier(&mut self, begin_byte: usize) -> Token {
        let loc = self.loc;
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
        match ident {
            "fn" => kind = TokenKind::FnKeyword,
            "do" => kind = TokenKind::DoKeyword,
            "end" => kind = TokenKind::EndKeyword,
            "type" => kind = TokenKind::TypeKeyword,
            "if" => kind = TokenKind::IfKeyword,
            "then" => kind = TokenKind::ThenKeyword,
            "else" => kind = TokenKind::ElseKeyword,
            "int" => kind = TokenKind::IntType,
            "string" => kind = TokenKind::StringType,
            "bool" => kind = TokenKind::BoolType,
            _ => {}
        }
        Token::new(kind, loc, ident.into())
    }

    // fn lex_number(&mut self, begin_byte: usize) -> Token {
    //     let loc = self.loc;
    //     let kind = TokenKind::IntegerNumber;

    //     while let '0'..='9' = self.read_char() {
    //         self.advance();
    //     }

    //     Token::new(kind, loc, self.source[begin_byte..self.byte_pos].into())
    // }

    fn lex_number(&mut self, begin_byte: usize) -> Token {
        let loc = self.loc;
        let end; // = begin_byte;
        let mut base = 10;

        // Check for base prefix (0x, 0b, 0o)
        // if self.read_char() == '0' {
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
        // }

        // Read digits according to base
        loop {
            let c = self.read_char();
            let valid = match base {
                2 => matches!(c, '0' | '1'),
                8 => matches!(c, '0'..='7'),
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

        // Parse suffix (letters/numbers after digits)
        let mut suffix = String::new();
        loop {
            let c = self.read_char();
            if c.is_ascii_alphanumeric() {
                suffix.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let num_str = &self.source[begin_byte..end]
            .trim_start_matches("0x")
            .trim_start_matches("0X")
            .trim_start_matches("0b")
            .trim_start_matches("0B")
            .trim_start_matches("0o")
            .trim_start_matches("0O");
        let kind = match (base, suffix.as_str()) {
            (2 | 8 | 10 | 16, "" | "i32") => TokenKind::Int(NumberBase::from(base)),
            (2 | 8 | 10 | 16, "i64") => TokenKind::Int64(NumberBase::from(base)),
            (2 | 8 | 10 | 16, "u32") => TokenKind::UInt(NumberBase::from(base)),
            (2 | 8 | 10 | 16, "u64") => TokenKind::UInt64(NumberBase::from(base)),
            _ => TokenKind::InvalidNumber,
        };

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
    // source: &'static str,
    pub source: String,
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
    pub fn source(&self) -> &str {
        // unsafe { transmute::<&'static str, &str>(self.source) }
        &self.source
    }

    pub fn new(kind: TokenKind, loc: Loc, source: String) -> Self {
        Self {
            kind,
            loc,
            // source: unsafe { transmute::<&str, &'static str>(source) },
            source,
        }
    }

    pub fn is_eof(&self) -> bool {
        matches!(self.kind, TokenKind::EOF)
    }

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
    FnKeyword,
    DoKeyword,
    EndKeyword,
    TypeKeyword,
    IfKeyword,
    ThenKeyword,
    ElseKeyword,

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
    InvalidNumber,

    Int64(NumberBase),
    UInt(NumberBase),
    UInt64(NumberBase),
    Int(NumberBase),

    IntType,
    StringType,
    BoolType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumberBase {
    B,
    O,
    D,
    X,
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
    pub fn is_int_num(&self) -> bool {
        matches!(
            self,
            Self::Int(_) | Self::Int64(_) | Self::UInt(_) | Self::UInt64(_)
        )
    }

    pub fn is_assign_kind(&self) -> bool {
        matches!(self, Self::Assign)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Loc {
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
}
