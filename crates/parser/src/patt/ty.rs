use parserc::{lang::LangInput, syntax::Syntax};

use crate::{
    errors::LangError,
    token::{Ident, S, TokenColon},
    ty::Type,
};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PattType<I>
where
    I: LangInput,
{
    /// parameter name.
    pub ident: Ident<I>,
    /// seperate token: `:`
    pub sep: (Option<S<I>>, TokenColon<I>, Option<S<I>>),
    /// type declaration clause.
    pub ty: Type<I>,
}
