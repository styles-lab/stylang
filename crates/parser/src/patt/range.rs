use parserc::{lang::LangInput, syntax::Syntax};

use crate::{errors::LangError, lit::Lit, token::*};

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PattRange<I>
where
    I: LangInput,
{
    /// optional start operand.
    pub start: Option<Box<Lit<I>>>,
    /// limit types.
    pub limits: RangeLimits<I>,
    /// optional end operand.
    pub end: Option<Box<Lit<I>>>,
}

/// The dots in a tuple or slice pattern: [0, 1, ..].
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PattRest<I>(pub TokenDotDot<I>)
where
    I: LangInput;
