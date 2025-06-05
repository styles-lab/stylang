use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    token::{Ident, SepColon},
    ty::Type,
};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PattType<I>
where
    I: LangInput,
{
    /// parameter name.
    pub ident: Ident<I>,
    /// seperate token: `:`
    pub sep: SepColon<I>,
    /// type declaration clause.
    pub ty: Type<I>,
}
