use parserc::derive_parse;

use super::{DotDot, DotDotEq, Expr, ParseError, StylangInput};

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRange<I>
where
    I: StylangInput,
{
    pub start: Option<Box<Expr<I>>>,
    pub limits: RangeLimits<I>,
    pub end: Option<Box<Expr<I>>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum RangeLimits<I>
where
    I: StylangInput,
{
    Closed(DotDotEq<I>),
    HalfOpen(DotDot<I>),
}
