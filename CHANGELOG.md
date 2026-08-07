# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
