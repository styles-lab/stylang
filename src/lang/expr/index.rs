use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{DotDot, DotDotEq, LeftBracket, RightBracket},
};

use super::Expr;

/// Limit types of a range, inclusive or exclusive.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum RangeLimits<I>
where
    I: LangInput,
{
    /// ..=
    Closed(DotDotEq<I>),
    /// ..
    HalfOpen(DotDot<I>),
}

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Range<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// optional start operand.
    pub start: Option<Expr<I>>,
    /// limit types.
    pub limits: RangeLimits<I>,
    /// optional end operand.
    pub end: Option<Expr<I>>,
}

/// The right part of indexing expression
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Index<I>
where
    I: LangInput,
{
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_index() {}
}
