# modalkit

[![Build Status](https://github.com/ulyssa/modalkit/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/modalkit/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/modalkit.svg?logo=apache)](https://crates.io/crates/modalkit)
[![#modalkit:0x.badd.cafe](https://img.shields.io/badge/matrix-%23modalkit:0x.badd.cafe-blue)](https://matrix.to/#/#modalkit:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/modalkit.svg?logo=rust)](https://crates.io/crates/modalkit)
[![Docs Status](https://docs.rs/modalkit/badge.svg)](https://docs.rs/crate/modalkit/)

## About

This is a Rust library for building modal editing applications, and provides
default keybindings for Vim and Emacs that you can drop into your application.

For examples of how you can use this crate, see [modalkit-ratatui], [scansion],
and [iamb].

## Usage

This crate can be used by adding `modalkit` to your dependencies in your
project's `Cargo.toml`.

```toml
[dependencies]
modalkit = "0.0.21"
```

## License

`modalkit` is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/modalkit/blob/master/LICENSE
[modalkit-ratatui]: https://docs.rs/modalkit-ratatui/latest/modalkit_ratatui/
[scansion]: https://docs.rs/scansion/latest/scansion/
[iamb]: https://github.com/ulyssa/iamb
