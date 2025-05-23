use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    tokens::{LeftBracket, RightBracket},
};

use super::Expr;

/// The right part of indexing expression
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) struct Index<I>
where
    I: LangInput,
{
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

/// A square bracketed indexing expression: vector[2].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIndex<I>
where
    I: LangInput,
{
    pub base: Box<Expr<I>>,
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_index() {}
}
