# editor-types-macros

[![Build Status](https://github.com/ulyssa/modalkit/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/modalkit/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/editor-types-macros.svg?logo=apache)](https://crates.io/crates/editor-types-macros)
[![#modalkit:0x.badd.cafe](https://img.shields.io/badge/matrix-%23modalkit:0x.badd.cafe-blue)](https://matrix.to/#/#modalkit:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/editor-types-macros.svg?logo=rust)](https://crates.io/crates/editor-types-macros)
[![Docs Status](https://docs.rs/editor-types-macros/badge.svg)](https://docs.rs/editor-types-macros/latest/editor_types_macros)

## About

This Rust crate contains the procedural macros used by and re-exported from the
[editor-types] crate. Consumers can use this crate's macros to convert the
command syntax used by [editor-types] into `Action` values at compile time.

## Usage

This crate can be used by adding `editor-types-macros` to your dependencies in your
project's `Cargo.toml`.

```toml
[dependencies]
editor-types-macros = "0.0.2"
```

## License

`editor-types-macros` is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/modalkit/blob/master/LICENSE
[editor-types]: https://docs.rs/editor-types/latest/editor_types/
