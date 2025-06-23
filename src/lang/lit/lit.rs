use parserc::{lang::LangInput, syntax::Syntax};

use crate::lang::{errors::LangError, token::TokenNone};

use super::*;

/// A literial value.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Lit<I>
where
    I: LangInput,
{
    /// A literial bool value `true` or `false`
    Bool(LitBool<I>),
    /// literial `none` value.
    None(TokenNone<I>),
    /// A literial `hex number` value.
    Hex(LitHexNum<I>),
    /// A literial `string` value.
    String(LitStr<I>),
    /// A literial `color` value.
    Color(LitColor<I>),
    /// A literial `number` value.
    Num(LitNum<I>),
}
