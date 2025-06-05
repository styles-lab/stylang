use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    lit::{LitBool, LitColor, LitHexNum, LitNum, LitStr},
    token::TokenNone,
};

/// A literial value.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
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
