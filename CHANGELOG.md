# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.0.0] - 2026-10-27

### Added
- Added `zero-copy` Cargo feature and `'src` lifetime parameter to `Token<'src>`, `TokenSource<'src>`, and `LexerSavePoint<'src>` to allow borrowing token slices directly from the input source without interning.
- Added `Lexer::from_source(source)` convenience constructor with default `"<anonymous>"` file path.
- Implemented `Iterator<Item = Token<'src>>` for `Lexer<'src>` (yields tokens until `TokenKind::EOF`).
- Added `Copy` derive to `Token<'src>`, `TokenSource<'src>`, and `LexerSavePoint<'src>`, plus `Debug`, `PartialEq`, and `Eq` on `LexerSavePoint<'src>`.
- Added `NumberBaseError` and `TryFrom<u32>` for `NumberBase`.
- Added `\t`, `\xHH`, and `\u{...}` escape support in string literals and `token_string_unescape`.
- Emitted `TokenKind::InvalidNumber` for malformed numeric literals (e.g., `0x` without digits, invalid base digits like `0b102`, or invalid trailing alphanumeric suffixes like `123abc`).

### Changed
- **Breaking:** Added `'src` lifetime parameter to `Token<'src>`, `TokenSource<'src>`, `LexerSavePoint<'src>`, and `RuleFn` (`for<'a, 'src> fn(&'a mut Lexer<'src>) -> Option<Token<'src>>`).
- **Breaking:** Renamed `Lexer::next()` to `Lexer::next_token()`.
- **Breaking:** `Lexer::new` now requires a `file_path` as its first parameter (`Lexer::new(file_path, source)`).
- **Breaking:** Removed optional `interning` Cargo feature; string interning is now built-in and always enabled across `TokenSource` (`&'static str`).
- **Breaking:** Renamed `TokenKind` equality and assignment variants to C/Rust conventions: `:=` is `TokenKind::ColonAssign`, `=` is `TokenKind::Assign`, and `==` is `TokenKind::Eq`.
- **Breaking:** Replaced panicking `From<u32>` on `NumberBase` with `TryFrom<u32>`.
- **Breaking:** Changed `Loc::file_path()` return type from `&&'static str` to `&'static str`.
- Removed eager `Vec<char>` allocation in `Lexer` in favor of direct UTF-8 `&str` slicing.
- Updated `Loc` to track the source `file_path`, formatted as `<file_path>:<line>:<col>`.

### Fixed
- Fixed token `Loc` off-by-one column calculation by capturing `Loc` prior to advancing the first character of a token.
- Fixed `Lexer::save()` and `Lexer::restore()` desynchronization with `peek()` by preserving `peeked` in `LexerSavePoint`.
- Fixed `Token::unescape()` panicking with `todo!()` on non-literal tokens; it now returns the raw token source string.
- Fixed infinite loop when a `//` line comment or `#!` shebang appears at EOF without a trailing newline.
- Fixed `...` (`TokenKind::Ellipsis`) lookahead bug that checked the second character twice instead of checking the second and third characters.
- Fixed `lex_number` double-advance bug on `0x`/`0b`/`0o` prefixes and restricted base prefixes to leading `0`.
- Fixed `many`, `many1`, and `sep_by` parser combinators to automatically save and restore lexer state on failed iterations (including rolling back trailing separators in `sep_by`).

## [1.3.1] - 2026-08-13

### Added
- Added `Lexer::advance_by_n(n)` helper method to advance lexer position by `n` characters.
- Added `Lexer::curr_loc()` helper method to query the current location.
- Added `Lexer::curr_ends_with_char(c)` and `Lexer::curr_ends_with_str(s)` helper methods to inspect token boundaries.

## [1.3.0] - 2026-08-07

### Added
- Added character literal lexing (`'a'`, `'\n'`, `'🔥'`, `'\xHH'`, `'\u{XXXX}'`) with `TokenKind::CharacterLiteral`.
- Added `TokenKind::EmptyCharacterLiteral` and `TokenKind::UnterminatedCharacterLiteral`.
- Added support for `TokenKind::CharacterLiteral` in `Token::unescape()`.
- Added custom token rule hooks `Lexer::with_pre_rule()`, `Lexer::add_pre_rule()`, `Lexer::with_post_rule()`, and `Lexer::add_post_rule()`.
- Added `TokenKind::Custom(u32)` and `TokenKind::CustomTag(&'static str)` for tagging custom user tokens.

## [1.2.1] - 2026-07-26

### Added
- impl PartialEq<str> for TokenSource

## [1.2.0] - 2026-07-26

### Added
- impl PartialEq<&str> for TokenSource
- impl PartialEq<String> for TokenSource
- TokenSource::as_str()

## [1.1.0] - 2026-07-21

### Added
- Lexer::save(), Lexer::restore(), LexerSavePoint{} and test_savepoint()

## [1.0.0] - 2026-07-15

### Added
- Committed to semantic versioning going forward.
- Added support for lexing `@` (`TokenKind::At`) and `?` (`TokenKind::Question`) characters.
