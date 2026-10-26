# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- **Breaking:** Renamed `Lexer::next()` to `Lexer::next_token()`.
- **Breaking:** `Lexer::new` now requires a `file_path` as its first parameter (`Lexer::new(file_path, source)`).
- **Breaking:** Removed optional `interning` Cargo feature; string interning is now built-in and always enabled across `TokenSource` (`&'static str`).
- Updated `Loc` to track the source `file_path`, formatted as `<file_path>:<line>:<col>`.
- Updated README and Rustdoc references to reflect the new API and features.

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
