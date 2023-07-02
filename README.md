# Percent-Encoded Strings for Rust

[![Build](https://img.shields.io/github/actions/workflow/status/timothee-haudebourg/pct-str/ci.yml?branch=main&style=flat-square)](https://github.com/timothee-haudebourg/pct-str/actions)
[![Crate informations](https://img.shields.io/crates/v/pct-str.svg?style=flat-square)](https://crates.io/crates/pct-str)
[![License](https://img.shields.io/crates/l/pct-str.svg?style=flat-square)](https://github.com/timothee-haudebourg/pct-str#license)
[![Documentation](https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square)](https://docs.rs/pct-str)

<!-- cargo-rdme start -->

This crate provides two types, [`PctStr`] and [`PctString`], similar to [`str`] and [`String`],
representing percent-encoded strings used in URL, URI, IRI, etc.
You can use them to encode, decode and compare percent-encoded strings.

## Basic usage

You can parse/decode percent-encoded strings by building a [`PctStr`] slice over a [`str`] slice.

```rust
use pct_str::PctStr;

let pct_str = PctStr::new("Hello%20World%21").unwrap();
assert_eq!(pct_str, "Hello World!");

let decoded_string: String = pct_str.decode();
assert_eq!(decoded_string, "Hello World!")
```

To create new percent-encoded strings, use the [`PctString`] to copy or encode new strings.

```rust
use pct_str::{PctString, URIReserved};

// Copy the given percent-encoded string.
let pct_string = PctString::new("Hello%20World%21").unwrap();

// Encode the given regular string.
let pct_string = PctString::encode("Hello World!".chars(), URIReserved);

assert_eq!(pct_string.as_str(), "Hello%20World%21");
```

You can choose which character will be percent-encoded by the `encode` function
by implementing the [`Encoder`] trait.

```rust
use pct_str::{URIReserved, PctString};

struct CustomEncoder;

impl pct_str::Encoder for CustomEncoder {
  fn encode(&self, c: char) -> bool {
    URIReserved.encode(c) || c.is_uppercase()
  }
}

let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
assert_eq!(pct_string.as_str(), "%48ello%20%57orld%21")
```

<!-- cargo-rdme end -->

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
