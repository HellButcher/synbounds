# `synbounds`

[![Latest Version](https://img.shields.io/crates/v/synbounds.svg)](https://crates.io/crates/synbounds)
[![Documentation](https://docs.rs/synbounds/badge.svg)](https://docs.rs/synbounds)
[![Build Status](https://github.com/HellButcher/synbounds/actions/workflows/rust.yml/badge.svg)](https://github.com/HellButcher/synbounds/actions)

`synbounds` is a Rust crate that provides utilities for writing `proc-macro` crates that need to manipulate or analyze Rust type bounds.

It leverages the `syn` crate to parse and work with Rust syntax trees, making it easier to handle complex type bounds in procedural macros.

## Background

When writing a `proc-macro` for a specific library, I came across the problem, that I needed to know
which generic bounds where actually used on a specific part of the AST (Functions in `impl` blocks
to be precise).

While `syn` provides the necessary structures to parse and represent these bounds, it does not
provide utilities to easily extract or manipulate them. So I first wrote some helper functions for
my specific use case.
Later I came across the `synstructure` crate, which provided some similar functionality, but was
more focused on deriving implementations for enums and structs. But this inspired me to generalize
and refactor my helper functions into a standalone crate that could be reused in other
`proc-macro` projects.

## License

[license]: #license

This project is licensed under either of

* MIT license ([LICENSE-MIT] or <http://opensource.org/licenses/MIT>)
* Apache License, Version 2.0, ([LICENSE-APACHE] or <http://www.apache.org/licenses/LICENSE-2.0>)

at your option

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.

[LICENSE-MIT]: LICENSE-MIT
[LICENSE-APACHE]: LICENSE-APACHE
