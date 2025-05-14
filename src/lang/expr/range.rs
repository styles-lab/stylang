use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{DotDot, DotDotEq},
};

use super::Expr;

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRange<I>
where
    I: LangInput,
{
    pub meta_list: MetaList<I>,
    /// optional range start expr.
    pub start: Option<Box<Expr<I>>>,
    /// Range limits token.
    pub limits: RangeLimits<I>,
    /// optional range end expr.
    pub end: Option<Box<Expr<I>>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum RangeLimits<I>
where
    I: LangInput,
{
    HalfOpen(DotDot<I>),
    Closed(DotDotEq<I>),
}
