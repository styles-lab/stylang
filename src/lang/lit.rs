//! lit exprs.

use parserc::Parse;

use super::{ParseError, StylangInput};

/// literial color expr.
pub enum LitColor<I> {
    /// be like: `rgb(255,255,255)`
    Rgb(I, I, I),
    /// be like: `rgb(10%,10%,10%)`
    RgbP(I, I, I),
    /// be like: `#fff` or `#f0f0f0`
    Hex(I),
}

impl<I> Parse<I> for LitColor<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(_input: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}
