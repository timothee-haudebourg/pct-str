# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `IriReserved`
- `Bytes` representation for `PctStr` with `bytes` function.
- `TryFrom<T>` implementations for `PctStr` and `PctString`

### Changed
- Introduced dependency on `utf8-decode`
- Make URIReserved output stricter on what will be encoded

## [1.1.0] - 2020-09-10
### Added
- A `CHANGELOG.md` file.
- `PctString::into_string`
- `PartialEq<&str>` implementation for `PctString`.

### Changed
- Fix failing examples.
