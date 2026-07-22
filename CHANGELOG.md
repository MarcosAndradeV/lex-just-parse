# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2026-07-21

### Added
- Lexer::save(), Lexer::restore(), LexerSavePoint{} and test_savepoint()

## [1.0.0] - 2026-07-15

### Added
- Committed to semantic versioning going forward.
- Added support for lexing `@` (`TokenKind::At`) and `?` (`TokenKind::Question`) characters.
