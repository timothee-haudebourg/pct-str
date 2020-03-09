use std::hash;
use std::fmt;

/// Error raised when a given input string is not percent-encoded as expected.
#[derive(Debug)]
pub struct InvalidEncoding;

/// Result of a function performing a percent-encoding check.
pub type Result<T> = std::result::Result<T, InvalidEncoding>;

/// Checks if a string is a correct percent-encoded string.
pub fn is_pct_encoded(str: &str) -> bool {
	let mut chars = str.chars();
	loop {
		match chars.next() {
			Some('%') => {
				match chars.next() {
					Some(c) if c.is_digit(16) => {
						match chars.next() {
							Some(c) if c.is_digit(16) => {
								break
							},
							_ => return false
						}
					},
					_ => return false
				}
			},
			Some(_) => (),
			None => break
		}
	}

	true
}

pub struct Chars<'a> {
	inner: std::str::Chars<'a>
}

impl<'a> Iterator for Chars<'a> {
	type Item = char;

	fn next(&mut self) -> Option<char> {
		match self.inner.next() {
			Some('%') => {
				let a = self.inner.next().unwrap().to_digit(16).unwrap();
				let b = self.inner.next().unwrap().to_digit(16).unwrap();
				let codepoint = (a << 4 | b) as u32;
				Some(unsafe { std::char::from_u32_unchecked(codepoint) })
			},
			Some(c) => Some(c),
			None => None
		}
	}
}

impl<'a> std::iter::FusedIterator for Chars<'a> { }

/// Percent-Encoded string
pub struct PctStr<'a> {
	data: &'a str
}

impl<'a> PctStr<'a> {
	pub fn new<S: AsRef<str> + ?Sized>(str: &'a S) -> Result<PctStr<'a>> {
		if is_pct_encoded(str.as_ref()) {
			Ok(PctStr {
				data: str.as_ref()
			})
		} else {
			Err(InvalidEncoding)
		}
	}

	/// Length of the string, in bytes.
	///
	/// Note that two percent-encoded strings with different lengths may
	/// represent the same string.
	#[inline]
	pub fn len(&self) -> usize {
		self.data.len()
	}

	/// Get the underlying Percent-Encoded string.
	#[inline]
	pub fn as_str(&self) -> &str {
		self.data
	}

	#[inline]
	pub fn chars(&self) -> Chars {
		Chars {
			inner: self.data.chars()
		}
	}

	pub fn decode(&self) -> String {
		let mut decoded = String::with_capacity(self.len());
		for c in self.chars() {
			decoded.push(c)
		}

		decoded
	}
}

impl<'a> PartialEq for PctStr<'a> {
	#[inline]
	fn eq(&self, other: &PctStr<'a>) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl<'a> Eq for PctStr<'a> { }

impl<'a> PartialEq<&'a str> for PctStr<'a> {
	#[inline]
	fn eq(&self, other: &&'a str) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl<'a> PartialEq<PctString> for PctStr<'a> {
	#[inline]
	fn eq(&self, other: &PctString) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl<'a> hash::Hash for PctStr<'a> {
	#[inline]
	fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
		for c in self.chars() {
			c.hash(hasher)
		}
	}
}

impl<'a> fmt::Display for PctStr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(self.data, f)
	}
}

impl<'a> fmt::Debug for PctStr<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self.data, f)
	}
}

pub trait Encoder {
	/// Decide if the given character must be encoded.
	///
	/// Note that the character `%` is always encoded even if this method returns `false` on it.
	/// Only characters with codepoint below `0x100` are encoded.
	fn encode(&self, c: char) -> bool;
}

pub struct PctString {
	data: String
}

pub unsafe fn to_hex_digit(b: u32) -> char {
	if b < 10 {
		std::char::from_u32_unchecked(b + 0x30)
	} else {
		std::char::from_u32_unchecked(b + 0x37)
	}
}

impl PctString {
	pub fn new<S: AsRef<str> + ?Sized>(str: &S) -> Result<PctString> {
		if is_pct_encoded(str.as_ref()) {
			Ok(PctString {
				data: str.as_ref().to_string()
			})
		} else {
			Err(InvalidEncoding)
		}
	}

	/// Encode a string.
	pub fn encode<I: Iterator<Item = char>, E: Encoder>(src: I, encoder: E) -> PctString {
		let mut encoded = String::new();
		for c in src {
			let codepoint = c as u32;
			if c == '%' || (codepoint <= 0xff && encoder.encode(c)) {
				let a = (codepoint >> 4) & 0xf;
				let b = codepoint & 0xf;
				encoded.push('%');
				unsafe {
					encoded.push(to_hex_digit(a));
					encoded.push(to_hex_digit(b));
				}
			} else {
				encoded.push(c);
			}
		}

		PctString {
			data: encoded
		}
	}

	/// Get the underlying Percent-Encoded string.
	#[inline]
	pub fn as_str(&self) -> &str {
		self.data.as_ref()
	}

	#[inline]
	pub fn as_pct_str(&self) -> PctStr {
		PctStr {
			data: self.data.as_ref()
		}
	}

	#[inline]
	pub fn decode(&self) -> String {
		self.as_pct_str().decode()
	}

	#[inline]
	pub fn chars(&self) -> Chars {
		Chars {
			inner: self.data.chars()
		}
	}
}

impl PartialEq for PctString {
	#[inline]
	fn eq(&self, other: &PctString) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl Eq for PctString { }

impl<'a> PartialEq<PctStr<'a>> for PctString {
	#[inline]
	fn eq(&self, other: &PctStr<'a>) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl<'a> PartialEq<&'a str> for PctString {
	#[inline]
	fn eq(&self, other: &&'a str) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => ()
			}
		}

		true
	}
}

impl hash::Hash for PctString {
	#[inline]
	fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
		for c in self.chars() {
			c.hash(hasher)
		}
	}
}

impl fmt::Display for PctString {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(self.as_str(), f)
	}
}

impl fmt::Debug for PctString {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self.as_str(), f)
	}
}

pub struct URLReserved;

impl Encoder for URLReserved {
	fn encode(&self, c: char) -> bool {
		match c {
			'!' | '#' | '$' | '%' | '&' | '\'' |
			'(' | ')' | '*' | '+' | ',' | '/' |
			':' | ';' | '=' | '?' | '@' | '[' | ']' => true,
			_ => false
		}
	}
}
