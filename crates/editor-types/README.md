# editor-types

[![Build Status](https://github.com/ulyssa/modalkit/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/modalkit/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/modalkit.svg?logo=apache)](https://crates.io/crates/modalkit)
[![#modalkit:0x.badd.cafe](https://img.shields.io/badge/matrix-%23modalkit:0x.badd.cafe-blue)](https://matrix.to/#/#modalkit:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/editor-types.svg?logo=rust)](https://crates.io/crates/editor-types)
[![Docs Status](https://docs.rs/modalkit/badge.svg)](https://docs.rs/crate/modalkit/)

## About

This is a Rust crate for describing defunctionalized editor actions.
Library consumers can then use them to drive an actual implementation
by switching on the different enum variants.

For examples of how you can use this crate, see [modalkit], [modalkit-ratatui],
[scansion], and [iamb].

## Usage

This crate can be used by adding `editor-types` to your dependencies in your
project's `Cargo.toml`.

```toml
[dependencies]
editor-types = "0.0.1"
```

## License

`editor-types` is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/modalkit/blob/master/LICENSE
[modalkit]: https://docs.rs/modalkit/latest/modalkit/
[modalkit-ratatui]: https://docs.rs/modalkit-ratatui/latest/modalkit_ratatui/
[scansion]: https://docs.rs/scansion/latest/scansion/
[iamb]: https://github.com/ulyssa/iamb

