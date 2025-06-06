use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    lit::Lit,
    token::{RangeLimits, SepDotDot},
};

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
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
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattRest<I>(pub SepDotDot<I>)
where
    I: LangInput;
