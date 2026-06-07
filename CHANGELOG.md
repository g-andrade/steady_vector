# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0/).

## [Unreleased]

### Added

- support for Erlang/OTP 24 through 29
- API documentation rendered with ExDoc, through EEP-48 doc attributes

### Changed

- test suite migrated from inline EUnit to Common Test
- development tooling modernised (erlfmt, rebar3_hank, rebar3_lint, ExDoc)
- continuous integration moved to GitHub Actions

### Removed

- Travis CI and CircleCI configurations
- support for Erlang/OTP older than 24

## [1.0.1] - 2019-01-19

### Added

- support for newer Erlang/OTP releases (20.2 through 21.2)

### Changed

- Hex package metadata now links to the GitLab mirror

## [1.0.0] - 2017-12-04

### Added

- initial release: a persistent vector optimized for tail growth and shrinkage,
  ported from Elixir's PersistentVector

[Unreleased]: https://github.com/g-andrade/steady_vector/compare/1.0.1...HEAD
[1.0.1]: https://github.com/g-andrade/steady_vector/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/g-andrade/steady_vector/releases/tag/1.0.0
