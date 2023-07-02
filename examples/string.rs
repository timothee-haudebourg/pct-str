extern crate pct_str;

use pct_str::{InvalidPctString, PctStr, PctString};

fn main() -> Result<(), InvalidPctString<String>> {
	// [`PctString`] is the equivalent of [`String`] for
	// percent-encoded strings.
	// The data is owned by `pct_string`.
	let pct_string = PctString::from_string("Hello%20World%21".to_string())?;

	// You can compare percent-encoded strings with a regular string.
	assert!(pct_string == "Hello World!");

	// The underlying string is percent-encoded.
	assert!(pct_string.as_str() == "Hello%20World%21");

	// You can get a reference to the string as a [`PctStr`].
	assert!(
		pct_string.as_pct_str()
			== PctStr::new("Hello%20World%21").map_err(InvalidPctString::into_owned)?
	);

	// Just as a regular string, you can iterate over the
	// encoded characters of `pct_str` with [`PctString::chars`].
	for c in pct_string.chars() {
		println!("{}", c);
	}

	// You can decode the string and every remove percent-encoded characters
	// with the [`PctStr::decode`] method.
	let decoded_string: String = pct_string.decode();
	println!("{}", decoded_string);

	Ok(())
}
