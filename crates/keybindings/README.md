# keybindings

[![Build Status](https://github.com/ulyssa/modalkit/actions/workflows/ci.yml/badge.svg)](https://github.com/ulyssa/modalkit/actions?query=workflow%3ACI+)
[![License: Apache 2.0](https://img.shields.io/crates/l/keybindings.svg?logo=apache)](https://crates.io/crates/keybindings)
[![#modalkit:0x.badd.cafe](https://img.shields.io/badge/matrix-%23modalkit:0x.badd.cafe-blue)](https://matrix.to/#/#modalkit:0x.badd.cafe)
[![Latest Version](https://img.shields.io/crates/v/keybindings.svg?logo=rust)](https://crates.io/crates/keybindings)
[![Docs Status](https://docs.rs/keybindings/badge.svg)](https://docs.rs/crate/keybindings/)

## About

This crate provides environment-agnostic interfaces for tracking and processing
keybindings in different modes, and can be used to build applications with
modal input.

See [modalkit] for an example of using this crate to implement Vim and Emacs
keybindings.

## Usage

This crate can be used by adding `keybindings` to your dependencies in your
project's `Cargo.toml`.

```toml
[dependencies]
keybindings = "0.0.2"
```

## License

`keybindings` is released under the [Apache License, Version 2.0].

[Apache License, Version 2.0]: https://github.com/ulyssa/modalkit/blob/master/LICENSE
[modalkit]: https://docs.rs/modalkit/latest/modalkit/
