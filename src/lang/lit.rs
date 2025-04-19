use parserc::derive_parse;

use super::{KeywordNone, LitColor, LitHexNum, LitNum, LitStr, ParseError, StylangInput};

/// A literial value.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Lit<I>
where
    I: StylangInput,
{
    /// A literial `number` value.
    Num(LitNum<I>),
    /// A literial `hex number` value.
    Hex(LitHexNum<I>),
    /// A literial `string` value.
    String(LitStr<I>),
    /// A literial `color` value.
    Color(LitColor<I>),
    /// A literial `none` value.
    None(KeywordNone<I>),
}
