# editor-types-parser

[![Build Status](https://github.com/ulyssa/modalkit/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/modalkit/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/editor-types-parser.svg?logo=apache)](https://crates.io/crates/editor-types-parser)
[![#modalkit:0x.badd.cafe](https://img.shields.io/badge/matrix-%23modalkit:0x.badd.cafe-blue)](https://matrix.to/#/#modalkit:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/editor-types-parser.svg?logo=rust)](https://crates.io/crates/editor-types-parser)
[![Docs Status](https://docs.rs/editor-types-parser/badge.svg)](https://docs.rs/editor-types-parser/latest/editor_types_parser)

## About

This is a Rust crate for parsing the command language used in the [editor-types]
crate. Library consumers can then use its parser output and the `ActionParser`
trait to drive the conversion of the parser tree into some kind of output.

## Usage

This crate can be used by adding `editor-types-parser` to your dependencies in your
project's `Cargo.toml`.

```toml
[dependencies]
editor-types-parser = "0.0.1"
```

## License

`editor-types-parser` is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/modalkit/blob/master/LICENSE
[editor-types]: https://docs.rs/editor-types/latest/editor_types/
