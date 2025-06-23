use parserc::{lang::LangInput, syntax::Syntax};

use crate::lang::{errors::LangError, token::*};

/// A literial bool value.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum LitBool<I>
where
    I: LangInput,
{
    True(TokenTrue<I>),
    False(TokenFalse<I>),
}
