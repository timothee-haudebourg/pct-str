extern crate pct_str;

use pct_str::{InvalidPctString, PctStr};

fn main() -> Result<(), InvalidPctString<&'static str>> {
	// [`PctStr`] is the equivalent of [`str`] for percent-encoded strings.
	let buffer = "Hello%20World%21";
	// It is just a reference to `buffer`.
	// It can fail if `buffer` is not a valid percent-encoded string.
	let pct_str = PctStr::new(buffer)?;

	// You can compare percent-encoded strings with a regular string.
	assert!(pct_str == "Hello World!"); // => true

	// The underlying string is unchanged.
	assert!(pct_str.as_str() == "Hello%20World%21"); // => true

	// Just as a regular string, you can iterate over the
	// encoded characters of `pct_str` with [`PctStr::chars`].
	for c in pct_str.chars() {
		print!("{}", c);
	}
	// => Hello World!

	println!("");

	// You can decode the string and every remove percent-encoded characters
	// with the [`PctStr::decode`] method.
	let decoded_string: String = pct_str.decode();
	println!("{}", decoded_string);
	// => Hello World!

	Ok(())
}
