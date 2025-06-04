//! [`Input`] types used by `stylang`.

use std::{fmt::Debug, iter::Enumerate, str::Bytes};

use parserc::{AsBytes, AsStr, Input, span::WithSpan};

/// `Input` type alias for `stylang` parser combinator.
pub trait LangInput: Input<Item = u8> + AsBytes + AsStr + Clone + WithSpan + Debug {}

/// Input type used by stylang compiler.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TokenStream<'a> {
    pub offset: usize,
    pub value: &'a str,
}

impl<'a> From<&'a str> for TokenStream<'a> {
    fn from(value: &'a str) -> Self {
        TokenStream { offset: 0, value }
    }
}

impl<'a> From<(usize, &'a str)> for TokenStream<'a> {
    fn from(value: (usize, &'a str)) -> Self {
        TokenStream {
            offset: value.0,
            value: value.1,
        }
    }
}

impl<'a> Input for TokenStream<'a> {
    type Item = u8;

    type Iter = Bytes<'a>;

    type IterIndices = Enumerate<Self::Iter>;

    fn len(&self) -> usize {
        self.value.len()
    }

    fn split_to(&mut self, at: usize) -> Self {
        let (first, last) = self.value.split_at(at);

        self.value = last;
        let offset = self.offset;
        self.offset += at;

        TokenStream {
            offset,
            value: first,
        }
    }

    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = self.value.split_at(at);

        self.value = first;

        TokenStream {
            offset: self.offset + at,
            value: last,
        }
    }

    fn iter(&self) -> Self::Iter {
        self.value.bytes()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.iter().enumerate()
    }
}

impl<'a> AsBytes for TokenStream<'a> {
    fn as_bytes(&self) -> &[u8] {
        self.value.as_bytes()
    }
}

impl<'a> AsStr for TokenStream<'a> {
    fn as_str(&self) -> &str {
        self.value
    }
}

impl<'a> WithSpan for TokenStream<'a> {
    #[inline(always)]
    fn start(&self) -> usize {
        self.offset
    }
}

impl<'a> LangInput for TokenStream<'a> {}
