//! lit exprs.

use parserc::Parse;

use super::{Delimiter, ParseError, StylangInput};

/// An ascii hex-digit characters sequence: [0-9a-f]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct HexDigits<I>(pub I);

/// An ascii digit characters sequence: [0-9]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Digits<I>(pub I);

/// An ascii digit characters sequence + `%`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct DigitsPercent<I> {
    /// integer: [0-9]+
    pub digits: Digits<I>,
    /// punct `%`
    pub percent: I,
}

/// literial color expr.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum LitColor<I> {
    /// be like: `rgb(255,255,255)`
    Rgb {
        /// keyword `rgb`
        keyword: I,
        /// delimiter `(...)`
        delimiter: Delimiter<I>,
        /// read component integer: 0~255
        red: Digits<I>,
        /// ,
        comma1: I,
        /// green component integer: 0~255
        green: Digits<I>,
        /// ,
        comma2: I,
        /// blue component integer: 0~255
        blue: Digits<I>,
    },
    /// be like: `rgb(10%,10%,10%)`
    RgbPercent {
        /// keyword `rgb`
        keyword: I,
        /// delimiter `(...)`
        delimiter: Delimiter<I>,
        /// read component integer: 0~100
        red: DigitsPercent<I>,
        /// ,
        comma1: I,
        /// green component integer: 0~100
        green: DigitsPercent<I>,
        /// ,
        comma2: I,
        /// blue component integer: 0~100
        blue: DigitsPercent<I>,
    },
    /// be like: `#fff` or `#f0f0f0`
    Hex {
        /// hex color prefix `#`
        prefix: I,
        /// sequence of color hex digits.
        digits: HexDigits<I>,
    },
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
