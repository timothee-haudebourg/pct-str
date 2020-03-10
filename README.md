# Percent-Encoded Strings

<table><tr>
	<td><a href="https://docs.rs/pct-str">Documentation</a></td>
	<td><a href="https://crates.io/crates/pct-str">Crate informations</a></td>
	<td><a href="https://github.com/timothee-haudebourg/pct-str">Repository</a></td>
</tr></table>

This crate provides two types, `PctStr` and `PctString`, similar to `str` and `String`,
representing percent-encoded strings used in URL, URI, IRI, etc.
You can use them to encode, decode and compare percent-encoded strings.

## Basic usage

You can parse/decode percent-encoded strings by building `PctStr` slice over a `str` slice.

```rust
use pct_str::PctStr;

let pct_str = PctStr::new("Hello%20World%21")?;

assert!(pct_str == "Hello World!");

let decoded_string: String = pct_str.decode();
println!("{}", decoded_string); // => Hello World!
```

To create new percent-encoded strings, use the `PctString` to copy or encode new strings.

```rust
use pct_str::PctString;

// Copy the given percent-encoded string.
let pct_string = PctString::new("Hello%20World%21")?;

// Encode the given regular string.
let pct_string = PctString::encode("Hello World!".chars(), URIReserved);

println!("{}", pct_string.as_str()); // => Hello World%21
```

You can choose which character will be percent-encoded by the `encode` function
by implementing the `Encoder` trait.

```rust
struct CustomEncoder;

impl pct_str::Encoder for CustomEncoder {
	fn encode(&self, c: char) -> bool {
		URIReserved.encode(c) || c.is_uppercase()
	}
}

let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
println!("{}", pct_string.as_str()); // => %48ello %57orld%21
```

## License

Licensed under either of

 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.
