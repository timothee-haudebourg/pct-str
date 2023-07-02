//! This crate provides two types, [`PctStr`] and [`PctString`], similar to [`str`] and [`String`],
//! representing percent-encoded strings used in URL, URI, IRI, etc.
//! You can use them to encode, decode and compare percent-encoded strings.
//!
//! # Basic usage
//!
//! You can parse/decode percent-encoded strings by building a [`PctStr`] slice over a [`str`] slice.
//!
//! ```
//! use pct_str::PctStr;
//!
//! let pct_str = PctStr::new("Hello%20World%21").unwrap();
//! assert_eq!(pct_str, "Hello World!");
//!
//! let decoded_string: String = pct_str.decode();
//! assert_eq!(decoded_string, "Hello World!")
//! ```
//!
//! To create new percent-encoded strings, use the [`PctString`] to copy or encode new strings.
//!
//! ```
//! use pct_str::{PctString, URIReserved};
//!
//! // Copy the given percent-encoded string.
//! let pct_string = PctString::new("Hello%20World%21").unwrap();
//!
//! // Encode the given regular string.
//! let pct_string = PctString::encode("Hello World!".chars(), URIReserved);
//!
//! assert_eq!(pct_string.as_str(), "Hello%20World%21");
//! ```
//!
//! You can choose which character will be percent-encoded by the `encode` function
//! by implementing the [`Encoder`] trait.
//!
//! ```
//! use pct_str::{URIReserved, PctString};
//!
//! struct CustomEncoder;
//!
//! impl pct_str::Encoder for CustomEncoder {
//!   fn encode(&self, c: char) -> bool {
//!     URIReserved.encode(c) || c.is_uppercase()
//!   }
//! }
//!
//! let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
//! assert_eq!(pct_string.as_str(), "%48ello%20%57orld%21")
//! ```

use std::borrow::Borrow;
use std::hash;
use std::{
	cmp::{Ord, Ordering, PartialOrd},
	fmt::Display,
};
use std::{convert::TryFrom, fmt, io, str::FromStr};

/// Encoding error.
///
/// Raised when a given input string is not percent-encoded as expected.
#[derive(Debug, Clone, thiserror::Error)]
#[error("invalid percent-encoded string")]
pub struct InvalidPctString<T>(pub T);

impl<T> InvalidPctString<T> {
	pub fn map<U>(self, f: impl FnOnce(T) -> U) -> InvalidPctString<U> {
		InvalidPctString(f(self.0))
	}
}

impl<'a, T: ?Sized + ToOwned> InvalidPctString<&'a T> {
	pub fn into_owned(self) -> InvalidPctString<T::Owned> {
		self.map(T::to_owned)
	}
}

#[inline(always)]
fn to_digit(b: u8) -> Result<u8, ByteError> {
	match b {
		// ASCII 0..=9
		0x30..=0x39 => Ok(b - 0x30),
		// ASCII A..=F
		0x41..=0x46 => Ok(b - 0x37),
		// ASCII a..=f
		0x61..=0x66 => Ok(b - 0x57),
		_ => Err(ByteError::InvalidByte(b)),
	}
}

/// Bytes iterator.
///
/// Iterates over the encoded bytes of a percent-encoded string.
pub struct Bytes<'a>(std::slice::Iter<'a, u8>);

#[derive(Debug, Clone)]
enum ByteError {
	InvalidByte(u8),
	IncompleteEncoding,
}

impl From<ByteError> for io::Error {
	fn from(e: ByteError) -> Self {
		io::Error::new(io::ErrorKind::InvalidData, e.to_string())
	}
}

impl Display for ByteError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ByteError::InvalidByte(b) => write!(f, "Invalid UTF-8 byte: {:#x}", b),
			ByteError::IncompleteEncoding => f.write_str("Incomplete percent-encoding segment"),
		}
	}
}

impl std::error::Error for ByteError {}

impl<'a> Iterator for Bytes<'a> {
	type Item = u8;

	fn next(&mut self) -> Option<u8> {
		if let Some(next) = self.0.next().copied() {
			match next {
				b'%' => {
					let a = self.0.next().copied().unwrap();
					let a = to_digit(a).unwrap();
					let b = self.0.next().copied().unwrap();
					let b = to_digit(b).unwrap();
					let byte = a << 4 | b;
					Some(byte)
				}
				_ => Some(next),
			}
		} else {
			None
		}
	}
}

impl<'a> std::iter::FusedIterator for Bytes<'a> {}

/// Untrusted bytes iterator.
///
/// Iterates over the encoded bytes of a percent-encoded string.
struct UntrustedBytes<B>(B);

impl<B> UntrustedBytes<B> {
	fn new(bytes: B) -> Self {
		Self(bytes)
	}
}

impl<B: Iterator<Item = u8>> UntrustedBytes<B> {
	fn try_next(&mut self, next: u8) -> io::Result<u8> {
		match next {
			b'%' => {
				let a = self.0.next().ok_or(ByteError::IncompleteEncoding)?;
				let a = to_digit(a)?;
				let b = self.0.next().ok_or(ByteError::IncompleteEncoding)?;
				let b = to_digit(b)?;
				let byte = a << 4 | b;
				Ok(byte)
			}
			_ => Ok(next),
		}
	}
}

impl<B: Iterator<Item = u8>> Iterator for UntrustedBytes<B> {
	type Item = io::Result<u8>;

	fn next(&mut self) -> Option<io::Result<u8>> {
		self.0.next().map(|b| self.try_next(b))
	}
}

impl<B: Iterator<Item = u8>> std::iter::FusedIterator for UntrustedBytes<B> {}

/// Characters iterator.
///
/// Iterates over the encoded characters of a percent-encoded string.
pub struct Chars<'a> {
	inner: utf8_decode::Decoder<Bytes<'a>>,
}

impl<'a> Chars<'a> {
	fn new(bytes: Bytes<'a>) -> Self {
		Self {
			inner: utf8_decode::Decoder::new(bytes),
		}
	}
}

impl<'a> Iterator for Chars<'a> {
	type Item = char;

	fn next(&mut self) -> Option<char> {
		// Safe as PctStr guarantees a valid byte sequence
		self.inner.next().map(|x| x.unwrap())
	}
}

impl<'a> std::iter::FusedIterator for Chars<'a> {}

/// Percent-Encoded string slice.
///
/// This is the equivalent of [`str`] for percent-encoded strings.
/// This is an *unsized* type, meaning that it must always be used behind a
/// pointer like `&` or [`Box`]. For an owned version of this type,
/// see [`PctString`].
///
/// # Examples
///
/// ```
/// use pct_str::PctStr;
///
/// let buffer = "Hello%20World%21";
/// let pct_str = PctStr::new(buffer).unwrap();
///
/// // You can compare percent-encoded strings with a regular string.
/// assert!(pct_str == "Hello World!");
///
/// // The underlying string is unchanged.
/// assert!(pct_str.as_str() == "Hello%20World%21");
///
/// // Just as a regular string, you can iterate over the
/// // encoded characters of `pct_str` with [`PctStr::chars`].
/// for c in pct_str.chars() {
///   print!("{}", c);
/// }
///
/// // You can decode the string and every remove percent-encoded characters
/// // with the [`PctStr::decode`] method.
/// let decoded_string: String = pct_str.decode();
/// println!("{}", decoded_string);
/// ```
pub struct PctStr([u8]);

impl PctStr {
	/// Create a new percent-encoded string slice.
	///
	/// The input slice is checked for correct percent-encoding.
	/// If the test fails, a [`InvalidEncoding`] error is returned.
	pub fn new<S: AsRef<[u8]> + ?Sized>(input: &S) -> Result<&PctStr, InvalidPctString<&S>> {
		let input_bytes = input.as_ref();
		if Self::validate(input_bytes.iter().copied()) {
			Ok(unsafe { Self::new_unchecked(input_bytes) })
		} else {
			Err(InvalidPctString(input))
		}
	}

	/// Create a new percent-encoded string slice without checking for correct encoding.
	///
	/// This is an unsafe function. The resulting string slice will have an undefined behaviour
	/// if the input slice is not percent-encoded.
	///
	/// # Safety
	///
	/// The input `str` must be a valid percent-encoded string.
	pub unsafe fn new_unchecked<S: AsRef<[u8]> + ?Sized>(input: &S) -> &PctStr {
		std::mem::transmute(input.as_ref())
	}

	/// Checks that the given iterator produces a valid percent-encoded string.
	pub fn validate(input: impl Iterator<Item = u8>) -> bool {
		let chars = UntrustedBytes::new(input);
		utf8_decode::UnsafeDecoder::new(chars).all(|r| r.is_ok())
	}

	/// Length of the decoded string (character count).
	///
	/// Computed in linear time.
	/// This is different from the byte length, which can be retrieved using
	/// `value.as_bytes().len()`.
	#[inline]
	pub fn len(&self) -> usize {
		self.chars().count()
	}

	/// Checks if the string is empty.
	#[inline]
	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	/// Returns the underlying percent-encoding bytes.
	#[inline]
	pub fn as_bytes(&self) -> &[u8] {
		&self.0
	}

	/// Get the underlying percent-encoded string slice.
	#[inline]
	pub fn as_str(&self) -> &str {
		unsafe {
			// SAFETY: the data has be validated, and all percent-encoded
			//         strings are valid UTF-8 strings.
			core::str::from_utf8_unchecked(&self.0)
		}
	}

	/// Iterate over the encoded characters of the string.
	#[inline]
	pub fn chars(&self) -> Chars {
		Chars::new(self.bytes())
	}

	/// Iterate over the encoded bytes of the string.
	#[inline]
	pub fn bytes(&self) -> Bytes {
		Bytes(self.0.iter())
	}

	/// Decoding.
	///
	/// Return the string with the percent-encoded characters decoded.
	pub fn decode(&self) -> String {
		let mut decoded = String::with_capacity(self.len());
		for c in self.chars() {
			decoded.push(c)
		}

		decoded
	}
}

impl PartialEq for PctStr {
	#[inline]
	fn eq(&self, other: &PctStr) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => (),
			}
		}

		true
	}
}

impl Eq for PctStr {}

impl PartialEq<str> for PctStr {
	#[inline]
	fn eq(&self, other: &str) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => (),
			}
		}

		true
	}
}

impl PartialEq<PctString> for PctStr {
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
				_ => (),
			}
		}

		true
	}
}

impl PartialOrd for PctStr {
	fn partial_cmp(&self, other: &PctStr) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for PctStr {
	fn cmp(&self, other: &PctStr) -> Ordering {
		let mut self_chars = self.chars();
		let mut other_chars = other.chars();

		loop {
			match (self_chars.next(), other_chars.next()) {
				(None, None) => return Ordering::Equal,
				(None, Some(_)) => return Ordering::Less,
				(Some(_), None) => return Ordering::Greater,
				(Some(a), Some(b)) => match a.cmp(&b) {
					Ordering::Less => return Ordering::Less,
					Ordering::Greater => return Ordering::Greater,
					Ordering::Equal => (),
				},
			}
		}
	}
}

impl PartialOrd<PctString> for PctStr {
	fn partial_cmp(&self, other: &PctString) -> Option<Ordering> {
		self.partial_cmp(other.as_pct_str())
	}
}

impl hash::Hash for PctStr {
	#[inline]
	fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
		for c in self.chars() {
			c.hash(hasher)
		}
	}
}

impl fmt::Display for PctStr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Display::fmt(self.as_str(), f)
	}
}

impl fmt::Debug for PctStr {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		fmt::Debug::fmt(self.as_str(), f)
	}
}

impl ToOwned for PctStr {
	type Owned = PctString;

	fn to_owned(&self) -> Self::Owned {
		unsafe { PctString::new_unchecked(self.0.to_owned()) }
	}
}

impl Borrow<str> for PctStr {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}

impl AsRef<str> for PctStr {
	fn as_ref(&self) -> &str {
		self.as_str()
	}
}

impl Borrow<[u8]> for PctStr {
	fn borrow(&self) -> &[u8] {
		self.as_bytes()
	}
}

impl AsRef<[u8]> for PctStr {
	fn as_ref(&self) -> &[u8] {
		self.as_bytes()
	}
}

/// Encoding predicate.
///
/// Instances of this trait are used along with the [`encode`](`PctString::encode`) function
/// to decide which character must be percent-encoded.
///
/// This crate provides a simple implementation of the trait, [`URIReserved`]
/// encoding characters reserved in the URI syntax.
///
/// # Example
///
/// ```
/// use pct_str::{PctString, URIReserved};
///
/// let pct_string = PctString::encode("Hello World!".chars(), URIReserved);
/// println!("{}", pct_string.as_str()); // => Hello World%21
/// ```
///
/// Custom encoder implementation:
///
/// ```
/// use pct_str::{PctString, URIReserved};
///
/// struct CustomEncoder;
///
/// impl pct_str::Encoder for CustomEncoder {
///   fn encode(&self, c: char) -> bool {
///     URIReserved.encode(c) || c.is_uppercase()
///   }
/// }
///
/// let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
/// println!("{}", pct_string.as_str()); // => %48ello %57orld%21
/// ```
pub trait Encoder {
	/// Decide if the given character must be encoded.
	///
	/// Note that the character `%` is always encoded even if this method returns `false` on it.
	fn encode(&self, c: char) -> bool;
}

impl<F: Fn(char) -> bool> Encoder for F {
	fn encode(&self, c: char) -> bool {
		self(c)
	}
}

/// Owned, mutable percent-encoded string.
///
/// This is the equivalent of [`String`] for percent-encoded strings.
/// It implements [`Deref`](`std::ops::Deref`) to [`PctStr`] meaning that all methods on [`PctStr`] slices are
/// available on `PctString` values as well.
pub struct PctString(Vec<u8>);

impl PctString {
	/// Create a new owned percent-encoded string.
	///
	/// The input string is checked for correct percent-encoding.
	/// If the test fails, a [`InvalidPctString`] error is returned.
	pub fn new<B: Into<Vec<u8>>>(bytes: B) -> Result<Self, InvalidPctString<Vec<u8>>> {
		let bytes = bytes.into();
		if PctStr::validate(bytes.iter().copied()) {
			Ok(Self(bytes))
		} else {
			Err(InvalidPctString(bytes))
		}
	}

	pub fn from_string(string: String) -> Result<Self, InvalidPctString<String>> {
		Self::new(string).map_err(|e| {
			e.map(|bytes| unsafe {
				// SAFETY: the bytes come from the UTF-8 encoded input `string`.
				String::from_utf8_unchecked(bytes)
			})
		})
	}

	/// Creates a new owned percent-encoded string without validation.
	///
	/// # Safety
	///
	/// The input string must be correctly percent-encoded.
	pub unsafe fn new_unchecked<B: Into<Vec<u8>>>(bytes: B) -> Self {
		Self(bytes.into())
	}

	/// Encode a string into a percent-encoded string.
	///
	/// This function takes an [`Encoder`] instance to decide which character of the string must
	/// be encoded.
	///
	/// Note that the character `%` will always be encoded regardless of the provided [`Encoder`].
	///
	/// # Example
	///
	/// ```
	/// use pct_str::{PctString, URIReserved};
	///
	/// let pct_string = PctString::encode("Hello World!".chars(), URIReserved);
	/// println!("{}", pct_string.as_str()); // => Hello World%21
	/// ```
	pub fn encode<E: Encoder>(src: impl Iterator<Item = char>, encoder: E) -> PctString {
		use std::fmt::Write;

		let mut buf = String::with_capacity(4);
		let mut encoded = String::new();
		for c in src {
			if encoder.encode(c) || c == '%' {
				buf.clear();
				buf.push(c);
				for byte in buf.bytes() {
					write!(encoded, "%{:02X}", byte).unwrap();
				}
			} else {
				encoded.push(c);
			}
		}

		PctString(encoded.into_bytes())
	}

	/// Return this string as a borrowed percent-encoded string slice.
	#[inline]
	pub fn as_pct_str(&self) -> &PctStr {
		unsafe {
			// SAFETY: the bytes have been validated.
			PctStr::new_unchecked(&self.0)
		}
	}

	/// Return the internal string of the [`PctString`], consuming it
	#[inline]
	pub fn into_string(self) -> String {
		unsafe {
			// SAFETY: the bytes have been validated, and a percent-encoded
			//         string is a valid UTF-8 string.
			String::from_utf8_unchecked(self.0)
		}
	}

	#[inline]
	pub fn into_bytes(self) -> Vec<u8> {
		self.0
	}
}

impl std::ops::Deref for PctString {
	type Target = PctStr;

	#[inline]
	fn deref(&self) -> &PctStr {
		self.as_pct_str()
	}
}

impl Borrow<PctStr> for PctString {
	fn borrow(&self) -> &PctStr {
		self.as_pct_str()
	}
}

impl AsRef<PctStr> for PctString {
	fn as_ref(&self) -> &PctStr {
		self.as_pct_str()
	}
}

impl Borrow<str> for PctString {
	fn borrow(&self) -> &str {
		self.as_str()
	}
}

impl AsRef<str> for PctString {
	fn as_ref(&self) -> &str {
		self.as_str()
	}
}

impl Borrow<[u8]> for PctString {
	fn borrow(&self) -> &[u8] {
		self.as_bytes()
	}
}

impl AsRef<[u8]> for PctString {
	fn as_ref(&self) -> &[u8] {
		self.as_bytes()
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
				_ => (),
			}
		}

		true
	}
}

impl Eq for PctString {}

impl PartialEq<PctStr> for PctString {
	#[inline]
	fn eq(&self, other: &PctStr) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => (),
			}
		}

		true
	}
}

impl PartialEq<&str> for PctString {
	#[inline]
	fn eq(&self, other: &&str) -> bool {
		let mut a = self.chars();
		let mut b = other.chars();

		loop {
			match (a.next(), b.next()) {
				(Some(a), Some(b)) if a != b => return false,
				(Some(_), None) => return false,
				(None, Some(_)) => return false,
				(None, None) => break,
				_ => (),
			}
		}

		true
	}
}

impl PartialEq<str> for PctString {
	#[inline]
	fn eq(&self, other: &str) -> bool {
		self.eq(&other)
	}
}

impl PartialOrd for PctString {
	fn partial_cmp(&self, other: &PctString) -> Option<Ordering> {
		self.as_pct_str().partial_cmp(other.as_pct_str())
	}
}

impl PartialOrd<PctStr> for PctString {
	fn partial_cmp(&self, other: &PctStr) -> Option<Ordering> {
		self.as_pct_str().partial_cmp(other)
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

impl FromStr for PctString {
	type Err = InvalidPctString<String>;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Self::from_string(s.to_string())
	}
}

impl TryFrom<String> for PctString {
	type Error = InvalidPctString<String>;

	fn try_from(value: String) -> Result<Self, Self::Error> {
		Self::from_string(value)
	}
}

impl<'a> TryFrom<&'a str> for PctString {
	type Error = InvalidPctString<String>;

	fn try_from(value: &'a str) -> Result<Self, Self::Error> {
		Self::from_string(value.to_owned())
	}
}

impl<'a> TryFrom<&'a str> for &'a PctStr {
	type Error = InvalidPctString<&'a str>;

	fn try_from(value: &'a str) -> Result<Self, Self::Error> {
		PctStr::new(value)
	}
}

/// URI-reserved characters encoder.
///
/// This [`Encoder`] encodes characters that are reserved in the syntax of URI according to
/// [RFC 3986](https://tools.ietf.org/html/rfc3986).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct URIReserved;

impl Encoder for URIReserved {
	fn encode(&self, c: char) -> bool {
		if !c.is_ascii_graphic() {
			return true;
		}

		matches!(
			c,
			'!' | '#'
				| '$' | '%' | '&'
				| '\'' | '(' | ')'
				| '*' | '+' | ','
				| '/' | ':' | ';'
				| '=' | '?' | '@'
				| '[' | ']'
		)
	}
}

/// IRI-reserved characters encoder.
///
/// This [`Encoder`] encodes characters that are reserved in the syntax of IRI according to
/// [RFC 3987](https://tools.ietf.org/html/rfc3987).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IriReserved {
	Segment,
	SegmentNoColons,
	Fragment,
	Query,
}

impl Encoder for IriReserved {
	fn encode(&self, c: char) -> bool {
		// iunreserved
		if c.is_ascii_alphanumeric() {
			return false;
		}

		match c {
			// ipchar
			'@' => return false,
			// iunreserved
			'-' | '.' | '_' | '~' => return false,
			// sub-delims
			'!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' => return false,
			'/' | '?' => return *self != IriReserved::Query && *self != IriReserved::Fragment,
			':' => return *self == IriReserved::SegmentNoColons,
			_ => { /* fall through */ }
		}

		match c as u32 {
			// ucschar
			0xA0..=0xD7FF
			| 0xF900..=0xFDCF
			| 0xFDF0..=0xFFEF
			| 0x10000..=0x1FFFD
			| 0x20000..=0x2FFFD
			| 0x30000..=0x3FFFD
			| 0x40000..=0x4FFFD
			| 0x50000..=0x5FFFD
			| 0x60000..=0x6FFFD
			| 0x70000..=0x7FFFD
			| 0x80000..=0x8FFFD
			| 0x90000..=0x9FFFD
			| 0xA0000..=0xAFFFD
			| 0xB0000..=0xBFFFD
			| 0xC0000..=0xCFFFD
			| 0xD0000..=0xDFFFD
			| 0xE1000..=0xEFFFD => false,
			// iprivate
			0xE000..=0xF8FF | 0xF0000..=0xFFFFD | 0x100000..=0x10FFFD => {
				*self != IriReserved::Query
			}
			_ => true,
		}
	}
}

#[cfg(test)]
mod tests {
	use std::convert::TryInto;

	use super::*;

	#[test]
	fn iri_encode_cyrillic() {
		let encoder = IriReserved::Segment;
		let pct_string = PctString::encode("традиционное польское блюдо".chars(), encoder);
		assert_eq!(&pct_string, &"традиционное польское блюдо");
		assert_eq!(&pct_string.as_str(), &"традиционное%20польское%20блюдо");
	}

	#[test]
	fn iri_encode_segment() {
		let encoder = IriReserved::Segment;
		let pct_string = PctString::encode(
			"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}".chars(),
			encoder,
		);

		assert_eq!(
			&pct_string,
			&"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}"
		);
		assert_eq!(
			&pct_string.as_str(),
			&"%3Ftest=традиционное%20польское%20блюдо&cjk=真正&private=%F4%8F%BF%BD"
		);
	}

	#[test]
	fn iri_encode_segment_nocolon() {
		let encoder = IriReserved::SegmentNoColons;
		let pct_string = PctString::encode(
			"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}".chars(),
			encoder,
		);
		assert_eq!(
			&pct_string,
			&"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}"
		);
		assert_eq!(
			&pct_string.as_str(),
			&"%3Ftest=традиционное%20польское%20блюдо&cjk=真正&private=%F4%8F%BF%BD"
		);
	}

	#[test]
	fn iri_encode_fragment() {
		let encoder = IriReserved::Fragment;
		let pct_string = PctString::encode(
			"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}".chars(),
			encoder,
		);
		assert_eq!(
			&pct_string,
			&"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}"
		);
		assert_eq!(
			&pct_string.as_str(),
			&"?test=традиционное%20польское%20блюдо&cjk=真正&private=%F4%8F%BF%BD"
		);
	}

	#[test]
	fn iri_encode_query() {
		let encoder = IriReserved::Query;
		let pct_string = PctString::encode(
			"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}".chars(),
			encoder,
		);
		assert_eq!(
			&pct_string,
			&"?test=традиционное польское блюдо&cjk=真正&private=\u{10FFFD}"
		);
		assert_eq!(
			&pct_string.as_str(),
			&"?test=традиционное%20польское%20блюдо&cjk=真正&private=\u{10FFFD}"
		);
	}

	#[test]
	fn uri_encode_cyrillic() {
		let encoder = URIReserved;
		let pct_string = PctString::encode("традиционное польское блюдо\0".chars(), encoder);
		assert_eq!(&pct_string, &"традиционное польское блюдо\0");
		assert_eq!(&pct_string.as_str(), &"%D1%82%D1%80%D0%B0%D0%B4%D0%B8%D1%86%D0%B8%D0%BE%D0%BD%D0%BD%D0%BE%D0%B5%20%D0%BF%D0%BE%D0%BB%D1%8C%D1%81%D0%BA%D0%BE%D0%B5%20%D0%B1%D0%BB%D1%8E%D0%B4%D0%BE%00");
	}

	#[test]
	fn pct_encoding_invalid() {
		let s = "%FF%FE%20%4F";
		assert!(PctStr::new(s).is_err());
		let s = "%36%A";
		assert!(PctStr::new(s).is_err());
		let s = "%%32";
		assert!(PctStr::new(s).is_err());
		let s = "%%32";
		assert!(PctStr::new(s).is_err());
	}

	#[test]
	fn pct_encoding_valid() {
		let s = "%00%5C%F4%8F%BF%BD%69";
		assert!(PctStr::new(s).is_ok());
		let s = "No percent.";
		assert!(PctStr::new(s).is_ok());
		let s = "%e2%82%acwat";
		assert!(PctStr::new(s).is_ok());
	}

	#[test]
	fn try_from() {
		let s = "%00%5C%F4%8F%BF%BD%69";
		let _pcs = PctString::try_from(s).unwrap();
		let _pcs: &PctStr = s.try_into().unwrap();
	}

	#[test]
	fn encode_percent_always() {
		struct NoopEncoder;
		impl Encoder for NoopEncoder {
			fn encode(&self, _: char) -> bool {
				false
			}
		}
		let s = "%";
		let c = PctString::encode(s.chars(), NoopEncoder);
		assert_eq!(c.as_str(), "%25");
	}
}
