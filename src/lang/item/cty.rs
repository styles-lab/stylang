use parserc::{Punctuated, derive_parse};

use crate::lang::{
    errors::LangError,
    input::LangInput,
    item::Visibility,
    meta::MetaList,
    patt::PattType,
    token::{Brace, Ident, KeywordClass, KeywordData, KeywordEnum, Paren, SepComma},
    ty::Type,
};

/// Name field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct NameField<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// field type ascription pattern.
    pub patt: PattType<I>,
}

/// Uname field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UnameField<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// field type.
    pub ty: Type<I>,
}

/// Name field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Fields<I>
where
    I: LangInput,
{
    Name(Brace<I, Punctuated<NameField<I>, SepComma<I>>>),
    Uname(Paren<I, Punctuated<UnameField<I>, SepComma<I>>>),
}

/// Item `class`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemClass<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `class[s]+`
    pub keyword: KeywordClass<I>,
    /// class name.
    pub ident: Ident<I>,
    /// field list.
    pub fields: Fields<I>,
}

/// Item `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemData<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `data[s]+`
    pub keyword: KeywordData<I>,
    /// class name.
    pub ident: Ident<I>,
    /// field list.
    pub fields: Fields<I>,
}

/// Field for [`ItemEnum`]
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Variant<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// variant name.
    pub ident: Ident<I>,
    /// optional field list clause.
    pub fields: Option<Fields<I>>,
}

/// Item `enum`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemEnum<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `enum[s]+`
    pub keyword: KeywordEnum<I>,
    /// class name.
    pub ident: Ident<I>,
    /// variant list.
    pub variants: Brace<I, Punctuated<Variant<I>, SepComma<I>>>,
}
