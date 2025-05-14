//! lit expr parsers for `stylang`

mod num;
pub use num::*;
mod string;
pub use string::*;
mod color;
pub use color::*;

use parserc::derive_parse;

use super::{
    errors::LangError,
    inputs::LangInput,
    tokens::{False, KeywordNone, True},
};

/// A literial bool value.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum LitBool<I>
where
    I: LangInput,
{
    True(True<I>),
    False(False<I>),
}

/// A literial value.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Lit<I>
where
    I: LangInput,
{
    /// A literial `hex number` value.
    Hex(LitHexNum<I>),
    /// A literial `string` value.
    String(LitStr<I>),
    /// A literial `color` value.
    Color(LitColor<I>),
    /// A literial `none` value.
    None(KeywordNone<I>),
    /// A literial `number` value.
    Num(LitNum<I>),
    /// A literial bool value `true` or `false`
    Bool(LitBool<I>),
}
