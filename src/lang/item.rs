use parserc::derive_parse;

use super::{
    Colon, Ident, KeywordClass, MetaList, ParseError, S, StylangInput, Visibility, ty::Type,
};

/// named field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct NamedField<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<Visibility<I>>,
    pub s1: Option<S<I>>,
    /// field name.
    pub name: Ident<I>,
    pub s2: Option<S<I>>,
    /// colon token: `:`
    pub colon: Colon<I>,
    pub s3: Option<S<I>>,
    /// field type.
    pub ty: Type<I>,
}

/// uname field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct UnameField<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<Visibility<I>>,
    pub s1: S<I>,
    /// field type.
    pub ty: Type<I>,
}

/// Field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Field<I>
where
    I: StylangInput,
{
    Named(NamedField<I>),
    Uname(UnameField<I>),
}

/// Class item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ItemClass<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<Visibility<I>>,
    pub s1: Option<S<I>>,
    /// keyword token: `class`
    pub class_token: KeywordClass<I>,
    pub s2: S<I>,
}
