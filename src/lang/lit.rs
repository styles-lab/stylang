use parserc::{Parse, Parser, ParserExt};

use super::{LitColor, LitHexNum, LitNum, LitStr, ParseError, StylangInput};

/// Variant for literial expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Lit<I> {
    Num(LitNum<I>),
    Hex(LitHexNum<I>),
    Str(LitStr<I>),
    Color(LitColor<I>),
}

impl<I> Parse<I> for Lit<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        LitNum::into_parser()
            .map(|v| Lit::Num(v))
            .or(LitHexNum::into_parser().map(|v| Lit::Hex(v)))
            .or(LitStr::into_parser().map(|v| Lit::Str(v)))
            .or(LitColor::into_parser().map(|v| Lit::Color(v)))
            .parse(input)
    }
}
