//! Percent-encoded strings manipulation.
//!
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
//!
//! assert!(pct_str == "Hello World!");
//!
//! let decoded_string: String = pct_str.decode();
//! println!("{}", decoded_string); // => Hello World!
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
//! println!("{}", pct_string.as_str()); // => Hello World%21
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
//! 	fn encode(&self, c: char) -> bool {
//! 		URIReserved.encode(c) || c.is_uppercase()
//! 	}
//! }
//!
//! let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
//! println!("{}", pct_string.as_str()); // => %48ello %57orld%21
//! ```

use std::cmp::{Ord, Ordering, PartialOrd};
use std::fmt;
use std::hash;

/// Encoding error.
///
/// Raised when a given input string is not percent-encoded as expected.
#[derive(Debug)]
pub struct InvalidEncoding;

/// Result of a function performing a percent-encoding check.
pub type Result<T> = std::result::Result<T, InvalidEncoding>;

/// Checks if a string is a correct percent-encoded string.
pub fn is_pct_encoded(str: &str) -> bool {
    let mut chars = str.chars();
    loop {
        match chars.next() {
            Some('%') => match chars.next() {
                Some(c) if c.is_digit(16) => match chars.next() {
                    Some(c) if c.is_digit(16) => break,
                    _ => return false,
                },
                _ => return false,
            },
            Some(_) => (),
            None => break,
        }
    }

    true
}

/// Characters iterator.
///
/// Iterates over the encoded characters of a percent-encoded string.
pub struct Chars<'a> {
    inner: std::str::Chars<'a>,
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
            }
            Some(c) => Some(c),
            None => None,
        }
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
/// 	print!("{}", c);
/// }
///
/// // You can decode the string and every remove percent-encoded characters
/// // with the [`PctStr::decode`] method.
/// let decoded_string: String = pct_str.decode();
/// println!("{}", decoded_string);
/// ```
pub struct PctStr {
    data: str,
}

impl PctStr {
    /// Create a new percent-encoded string slice.
    ///
    /// The input slice is checked for correct percent-encoding.
    /// If the test fails, a [`InvalidEncoding`] error is returned.
    pub fn new<S: AsRef<str> + ?Sized>(str: &S) -> Result<&PctStr> {
        if is_pct_encoded(str.as_ref()) {
            Ok(unsafe { PctStr::new_unchecked(str) })
        } else {
            Err(InvalidEncoding)
        }
    }

    /// Create a new percent-encoded string slice without checking for correct encoding.
    ///
    /// This is an unsafe function. The resulting string slice will have an undefined behaviour
    /// if the input slice is not percent-encoded.
    pub unsafe fn new_unchecked<S: AsRef<str> + ?Sized>(str: &S) -> &PctStr {
        &*(str.as_ref() as *const str as *const PctStr)
    }

    /// Length of the string slice, in bytes.
    ///
    /// Note that two percent-encoded strings with different lengths may
    /// represent the same string.
    #[inline]
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Get the underlying percent-encoded string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.data
    }

    /// Iterate over the encoded characters of the string.
    #[inline]
    pub fn chars(&self) -> Chars {
        Chars {
            inner: self.data.chars(),
        }
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
        fmt::Display::fmt(&self.data, f)
    }
}

impl fmt::Debug for PctStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.data, f)
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
/// 	fn encode(&self, c: char) -> bool {
/// 		URIReserved.encode(c) || c.is_uppercase()
/// 	}
/// }
///
/// let pct_string = PctString::encode("Hello World!".chars(), CustomEncoder);
/// println!("{}", pct_string.as_str()); // => %48ello %57orld%21
/// ```
pub trait Encoder {
    /// Decide if the given character must be encoded.
    ///
    /// Note that the character `%` is always encoded even if this method returns `false` on it.
    /// Only characters with codepoint below `0x100` are encoded.
    fn encode(&self, c: char) -> bool;
}

/// Owned, mutable percent-encoded string.
///
/// This is the equivalent of [`String`] for percent-encoded strings.
/// It implements [`Deref`](`std::ops::Deref`) to [`PctStr`] meaning that all methods on [`PctStr`] slices are
/// available on `PctString` values as well.
pub struct PctString {
    data: String,
}

unsafe fn to_hex_digit(b: u32) -> char {
    if b < 10 {
        std::char::from_u32_unchecked(b + 0x30)
    } else {
        std::char::from_u32_unchecked(b + 0x37)
    }
}

impl PctString {
    /// Create a new owned percent-encoded string.
    ///
    /// The input slice is checked for correct percent-encoding and copied.
    /// If the test fails, a [`InvalidEncoding`] error is returned.
    pub fn new<S: AsRef<str> + ?Sized>(str: &S) -> Result<PctString> {
        if is_pct_encoded(str.as_ref()) {
            Ok(PctString {
                data: str.as_ref().to_string(),
            })
        } else {
            Err(InvalidEncoding)
        }
    }

    /// Encode a string into a percent-encoded string.
    ///
    /// This function takes an [`Encoder`] instance to decide which character of the string must
    /// be encoded.
    ///
    /// # Example
    ///
    /// ```
    /// use pct_str::{PctString, URIReserved};
    ///
    /// let pct_string = PctString::encode("Hello World!".chars(), URIReserved);
    /// println!("{}", pct_string.as_str()); // => Hello World%21
    /// ```
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

        PctString { data: encoded }
    }

    /// Return this string as a borrowed percent-encoded string slice.
    #[inline]
    pub fn as_pct_str(&self) -> &PctStr {
        unsafe { PctStr::new_unchecked(&self.data) }
    }

    /// Return the internal string of the [`PctString`], consuming it
    #[inline]
    pub fn into_string(self) -> String {
        self.data
    }
}

impl std::ops::Deref for PctString {
    type Target = PctStr;

    #[inline]
    fn deref(&self) -> &PctStr {
        self.as_pct_str()
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

/// URI-reserved characters encoder.
///
/// This [`Encoder`] encodes characters that are reserved in the syntax of URI according to
/// [RFC 3986](https://tools.ietf.org/html/rfc3986).
pub struct URIReserved;

impl Encoder for URIReserved {
    fn encode(&self, c: char) -> bool {
        match c {
            '!' | '#' | '$' | '%' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | '/' | ':' | ';'
            | '=' | '?' | '@' | '[' | ']' => true,
            _ => false,
        }
    }
}
