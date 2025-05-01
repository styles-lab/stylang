use parserc::derive_parse;

use super::{Digits, Dot, Expr, Ident, ParseError, StylangInput};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Member<I>
where
    I: StylangInput,
{
    Named(Ident<I>),
    Unnamed(Digits<I>),
}

/// Access of a named struct field (obj.k) or unnamed tuple struct field (obj.0).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprField<I>
where
    I: StylangInput,
{
    /// base expr.
    pub base: Box<Expr<I>>,
    /// dot token `.`
    pub dot_token: Dot<I>,
    /// member expr.
    pub member: Member<I>,
}
