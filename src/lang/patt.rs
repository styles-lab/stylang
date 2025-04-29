use parserc::derive_parse;

use super::{
    Colon, Ident, MetaList, ParseError, S, StylangInput,
    ty::{Type, TypePath},
};

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct PattType<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// named variable.
    pub name: Ident<I>,
    /// token `:`
    pub colon_token: (Option<S<I>>, Colon<I>, Option<S<I>>),
    /// type declaration.
    pub ty: Box<Type<I>>,
}

/// A type ascription pattern: text::Fill.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct PattPath<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// type declaration.
    pub path: TypePath<I>,
}

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Patt<I>
where
    I: StylangInput,
{
    Type(PattType<I>),
    Path(PattPath<I>),
}
