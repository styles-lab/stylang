use std::{iter::Enumerate, str::Bytes};

use parserc::{AsBytes, AsStr, Input, span::WithSpan};

/// `Input` type used by stylang compiler.
pub trait TokenStream: Input<Item = u8> + AsBytes + AsStr + Clone + WithSpan {}

/// Input type used by stylang compiler.
#[derive(Debug, PartialEq, Clone)]
pub struct Source<'a> {
    pub offset: usize,
    pub value: &'a str,
}

impl<'a> From<&'a str> for Source<'a> {
    fn from(value: &'a str) -> Self {
        Source { offset: 0, value }
    }
}

impl<'a> From<(usize, &'a str)> for Source<'a> {
    fn from(value: (usize, &'a str)) -> Self {
        Source {
            offset: value.0,
            value: value.1,
        }
    }
}

impl<'a> Input for Source<'a> {
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

        Source {
            offset,
            value: first,
        }
    }

    fn split_off(&mut self, at: usize) -> Self {
        let (first, last) = self.value.split_at(at);

        self.value = first;

        Source {
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

impl<'a> AsBytes for Source<'a> {
    fn as_bytes(&self) -> &[u8] {
        self.value.as_bytes()
    }
}

impl<'a> AsStr for Source<'a> {
    fn as_str(&self) -> &str {
        self.value
    }
}

impl<'a> WithSpan for Source<'a> {
    #[inline(always)]
    fn start(&self) -> usize {
        self.offset
    }
}

impl<'a> TokenStream for Source<'a> {}
