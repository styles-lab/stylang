use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    token::{TokenFalse, TokenTrue},
};

/// A literial bool value.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum LitBool<I>
where
    I: LangInput,
{
    True(TokenTrue<I>),
    False(TokenFalse<I>),
}
