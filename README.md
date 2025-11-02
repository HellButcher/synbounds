# `synbounds`

[![Latest Version](https://img.shields.io/crates/v/synbounds.svg)](https://crates.io/crates/synbounds)
[![Documentation](https://docs.rs/synbounds/badge.svg)](https://docs.rs/synbounds)
[![Build Status](https://github.com/HellButcher/synbounds/actions/workflows/rust.yml/badge.svg)](https://github.com/HellButcher/synbounds/actions)

`synbounds` is a Rust crate that provides utilities for writing `proc-macro` crates that need to manipulate or analyze Rust type bounds.

It leverages the `syn` crate to parse and work with Rust syntax trees, making it easier to handle complex type bounds in procedural macros.

## Features

- **Track Generic Usage**: Automatically determine which generic parameters (types, lifetimes, consts) are actually used in your code
- **Extract Minimal Bounds**: Generate minimal `where` clauses containing only the predicates for used generics
- **Lifetime Substitution**: Utilities for replacing lifetimes in syntax trees
- **Zero Overhead**: All analysis happens at compile-time via `proc-macro`

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
synbounds = "0.1"
```

### Example: Tracking Used Generics

```rust
use syn::{parse_quote, DeriveInput};
use syn::visit::Visit;
use synbounds::BoundGenerics;

// Parse a struct definition
let input: DeriveInput = parse_quote! {
    struct MyStruct<'a, T, U> {
        field: &'a T,
    }
};

// Track which generics are used in the fields
let mut bounds = BoundGenerics::new(&input.generics);
if let syn::Data::Struct(data) = &input.data {
    bounds.visit_fields(&data.fields);
}

// Only T and 'a are used, U is not
let bound_generics = bounds.to_bound_generics();
// bound_generics is now: <'a, T>
```

### Example: Filtering Where Clauses

```rust
use syn::{parse_quote, DeriveInput};
use syn::visit::Visit;
use synbounds::BoundGenerics;

let input: DeriveInput = parse_quote! {
    struct MyStruct<T, U>
    where
        T: Clone,
        U: Default,
    {
        field: T,
    }
};

let mut bounds = BoundGenerics::new(&input.generics);
if let syn::Data::Struct(data) = &input.data {
    bounds.visit_fields(&data.fields);
}

// Only get where predicates for used generics (T)
let where_predicates: Vec<_> = bounds.bound_where_predicates().collect();
// where_predicates contains only: T: Clone
```

### Example: Analyzing Impl Blocks

```rust
use syn::{parse_quote, ItemImpl};
use syn::visit::Visit;
use synbounds::BoundGenerics;

let item: ItemImpl = parse_quote! {
    impl<T, U, V> MyStruct<T> {
        fn process(&self, value: U) -> String {
            unimplemented!()
        }
    }
};

// Track which generics are used
let mut bounds = BoundGenerics::new(&item.generics);
bounds.visit_type(&item.self_ty);
for impl_item in &item.items {
    if let syn::ImplItem::Fn(method) = impl_item {
        bounds.visit_signature(&method.sig);
    }
}

// T and U are used, V is not
let bound_generics = bounds.to_bound_generics();
```

### Example: Lifetime Substitution

```rust
use syn::{parse_quote, Type};
use syn::visit_mut::VisitMut;
use synbounds::substitute_with_static_lifetime;

let mut ty: Type = parse_quote! { &'a String };
let mut visitor = substitute_with_static_lifetime::<()>();
visitor.visit_type_mut(&mut ty);
// ty is now: &'static String
```

## Features

- `proc-macro` (default): Enable proc-macro support
- `full`: Enable full `syn` feature for additional syntax support  
- `substitute` (default): Enable lifetime substitution utilities

## Use Cases

This crate is particularly useful when:

- Writing derive macros that need to determine which generic parameters are actually used
- Generating trait implementations with minimal bounds
- Analyzing AST nodes to extract relevant generic constraints
- Creating procedural macros that manipulate lifetime parameters

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
