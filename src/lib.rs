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

use std::fmt;
use std::hash;
use std::{
    cmp::{Ord, Ordering, PartialOrd},
    fmt::Display,
};

/// Encoding error.
///
/// Raised when a given input string is not percent-encoded as expected.
#[derive(Debug, Clone)]
pub struct InvalidEncoding;

impl Display for InvalidEncoding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("invalid encoding")
    }
}

impl std::error::Error for InvalidEncoding {}

/// Result of a function performing a percent-encoding check.
pub type Result<T> = std::result::Result<T, InvalidEncoding>;

/// Bytes iterator.
///
/// Iterates over the encoded bytes of a percent-encoded string.
pub struct Bytes<'a> {
    inner: std::str::Bytes<'a>,
}

#[derive(Debug, Clone)]
pub enum ByteError {
    InvalidByte(u8),
    IncompleteEncoding,
}

impl From<ByteError> for std::io::Error {
    fn from(e: ByteError) -> Self {
        std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())
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

#[inline(always)]
fn to_digit(b: u8) -> std::result::Result<u8, ByteError> {
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

impl<'a> Bytes<'a> {
    fn try_next(&mut self, next: u8) -> std::result::Result<u8, ByteError> {
        match next {
            b'%' => {
                let a = self
                    .inner
                    .next()
                    .ok_or_else(|| ByteError::IncompleteEncoding)?;
                let a = to_digit(a)?;
                let b = self
                    .inner
                    .next()
                    .ok_or_else(|| ByteError::IncompleteEncoding)?;
                let b = to_digit(b)?;
                let byte = (a << 4 | b) as u8;
                Ok(byte)
            }
            _ => Ok(next),
        }
    }
}

impl<'a> Iterator for Bytes<'a> {
    type Item = std::result::Result<u8, ByteError>;

    fn next(&mut self) -> Option<std::result::Result<u8, ByteError>> {
        if let Some(b) = self.inner.next() {
            Some(self.try_next(b))
        } else {
            None
        }
    }
}

impl<'a> std::iter::FusedIterator for Bytes<'a> {}

/// Characters iterator.
///
/// Iterates over the encoded characters of a percent-encoded string.
pub struct Chars<'a> {
    inner: std::str::Chars<'a>,
    buf: Vec<u8>,
    next: Option<char>,
}

impl<'a> Chars<'a> {
    fn new(chars: std::str::Chars<'a>) -> Self {
        Self {
            inner: chars,
            buf: Vec::with_capacity(4),
            next: None,
        }
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        if let Some(value) = self.next.take() {
            return Some(value);
        }

        fn process_buf(buf: &mut Vec<u8>) -> char {
            // This is safe because PctStr guarantees percent-encoded parts are valid UTF-8.
            let result = std::str::from_utf8(&buf).unwrap().chars().next().unwrap();
            buf.clear();
            result
        };

        match self.inner.next() {
            Some('%') => {
                let a = self.inner.next().unwrap().to_digit(16).unwrap();
                let b = self.inner.next().unwrap().to_digit(16).unwrap();
                let byte = (a << 4 | b) as u8;
                self.buf.push(byte);

                if self.buf.len() == 4 {
                    return Some(process_buf(&mut self.buf));
                }

                self.next()
            }
            Some(c) if !self.buf.is_empty() => {
                self.next = Some(c);
                Some(process_buf(&mut self.buf))
            }
            None if !self.buf.is_empty() => Some(process_buf(&mut self.buf)),
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
        let chars = Bytes {
            inner: str.as_ref().bytes(),
        };
        let decoder = utf8_decode::UnsafeDecoder::new(chars.map(|x| x.map_err(|e| e.into())));
        for c in decoder {
            c.map_err(|_| InvalidEncoding)?;
        }
        Ok(unsafe { Self::new_unchecked(str.as_ref()) })
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
        Chars::new(self.data.chars())
    }

    /// Iterate over the encoded bytes of the string.
    #[inline]
    pub fn bytes(&self) -> Bytes {
        Bytes {
            inner: self.data.bytes(),
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

impl PctString {
    /// Create a new owned percent-encoded string.
    ///
    /// The input slice is checked for correct percent-encoding and copied.
    /// If the test fails, a [`InvalidEncoding`] error is returned.
    pub fn new<S: AsRef<str> + ?Sized>(str: &S) -> Result<PctString> {
        Ok(Self {
            data: PctStr::new(str)?.decode(),
        })
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
        use std::fmt::Write;

        let mut buf = String::with_capacity(4);
        let mut encoded = String::new();
        for c in src {
            if encoder.encode(c) {
                buf.clear();
                buf.push(c);
                for byte in buf.bytes() {
                    write!(encoded, "%{:02X}", byte).unwrap();
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IriSegmentKind {
    Segment,
    SegmentNoColons,
    Fragment,
    Query,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct IriReserved(IriSegmentKind);

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
            '/' | '?' => {
                return self.0 != IriSegmentKind::Query && self.0 != IriSegmentKind::Fragment
            }
            ':' => return self.0 == IriSegmentKind::SegmentNoColons,
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
                self.0 != IriSegmentKind::Query
            }
            _ => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iri_encode_cyrillic() {
        let encoder = IriReserved(IriSegmentKind::Segment);
        let pct_string = PctString::encode("традиционное польское блюдо".chars(), encoder);
        assert_eq!(&pct_string, &"традиционное польское блюдо");
        assert_eq!(&pct_string.as_str(), &"традиционное%20польское%20блюдо");
    }

    #[test]
    fn iri_encode_segment() {
        let encoder = IriReserved(IriSegmentKind::Segment);
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
        let encoder = IriReserved(IriSegmentKind::SegmentNoColons);
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
        let encoder = IriReserved(IriSegmentKind::Fragment);
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
        let encoder = IriReserved(IriSegmentKind::Query);
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
}
