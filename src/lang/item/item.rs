use parserc::{Parse, Parser, ParserExt, Punctuated, derive_parse};

use crate::lang::{
    errors::{ItemKind, LangError},
    input::LangInput,
    item::{ItemFn, Visibility},
    meta::MetaList,
    patt::PattType,
    token::{
        Brace, Ident, KeywordClass, KeywordData, KeywordEnum, KeywordMod, KeywordUse, Paren,
        SepColonColon, SepComma, SepSemiColon, TokenStar,
    },
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

/// Item `mod`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemMod<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `mod[s]+`
    pub keyword: KeywordMod<I>,
    /// mod name.
    pub ident: Ident<I>,
    /// end token: `;`
    pub semi_colon: SepSemiColon<I>,
}

/// Path used by `ItemUse`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UsePath<I>
where
    I: LangInput,
{
    /// first element of use path.
    pub first: Ident<I>,
    pub rest: Vec<(SepColonColon<I>, UseSegment<I>)>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UseSegment<I>
where
    I: LangInput,
{
    Ident(Ident<I>),
    Glob(TokenStar<I>),
    Group(Brace<I, Punctuated<UsePath<I>, SepComma<I>>>),
}

/// Item `use`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemUse<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `use[s]+`
    pub keyword: KeywordUse<I>,
    /// path clause.
    pub path: UsePath<I>,
    /// end token: `;`
    pub semi_colon: SepSemiColon<I>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item<I>
where
    I: LangInput,
{
    Class(ItemClass<I>),
    Data(ItemData<I>),
    Enum(ItemEnum<I>),
    Fn(ItemFn<I>),
    Mod(ItemMod<I>),
    Use(ItemUse<I>),
    MetaList(MetaList<I>),
}

impl<I> Parse<I> for Item<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        ItemClass::into_parser()
            .map(|v| Self::Class(v))
            .map_err(|input: I, err| LangError::from((err, ItemKind::Class(input.span()))))
            .or(ItemData::into_parser()
                .map(|v| Self::Data(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Data(input.span())))))
            .or(ItemEnum::into_parser()
                .map(|v| Self::Enum(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Enum(input.span())))))
            .or(ItemFn::into_parser()
                .map(|v| Self::Fn(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Fn(input.span())))))
            .or(ItemMod::into_parser()
                .map(|v| Self::Mod(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Mod(input.span())))))
            .or(ItemUse::into_parser()
                .map(|v| Self::Use(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Use(input.span())))))
            .or(MetaList::into_parser().map(|v| Self::MetaList(v)))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Delimiter, Parse};

    use super::*;
    use crate::lang::{input::TokenStream, token::*};

    #[test]
    fn use_path() {
        assert_eq!(
            Item::parse(TokenStream::from("use a::b::{c,d::*};")),
            Ok((
                Item::Use(ItemUse {
                    meta_list: vec![],
                    vs: None,
                    keyword: KeywordUse(
                        TokenStream {
                            offset: 0,
                            value: "use"
                        },
                        S(TokenStream {
                            offset: 3,
                            value: " "
                        })
                    ),
                    path: UsePath {
                        first: Ident(TokenStream {
                            offset: 4,
                            value: "a"
                        }),
                        rest: vec![
                            (
                                SepColonColon(
                                    None,
                                    TokenStream {
                                        offset: 5,
                                        value: "::"
                                    },
                                    None
                                ),
                                UseSegment::Ident(Ident(TokenStream {
                                    offset: 7,
                                    value: "b"
                                }))
                            ),
                            (
                                SepColonColon(
                                    None,
                                    TokenStream {
                                        offset: 8,
                                        value: "::"
                                    },
                                    None
                                ),
                                UseSegment::Group(Delimiter {
                                    delimiter_start: SepLeftBrace(
                                        None,
                                        TokenStream {
                                            offset: 10,
                                            value: "{"
                                        },
                                        None
                                    ),
                                    body: Punctuated {
                                        pairs: vec![(
                                            UsePath {
                                                first: Ident(TokenStream {
                                                    offset: 11,
                                                    value: "c"
                                                }),
                                                rest: vec![]
                                            },
                                            SepComma(
                                                None,
                                                TokenStream {
                                                    offset: 12,
                                                    value: ","
                                                },
                                                None
                                            )
                                        )],
                                        tail: Some(Box::new(UsePath {
                                            first: Ident(TokenStream {
                                                offset: 13,
                                                value: "d"
                                            }),
                                            rest: vec![(
                                                SepColonColon(
                                                    None,
                                                    TokenStream {
                                                        offset: 14,
                                                        value: "::"
                                                    },
                                                    None
                                                ),
                                                UseSegment::Glob(TokenStar(TokenStream {
                                                    offset: 16,
                                                    value: "*"
                                                }))
                                            )]
                                        }))
                                    },
                                    delimiter_end: SepRightBrace(
                                        None,
                                        TokenStream {
                                            offset: 17,
                                            value: "}"
                                        },
                                        None
                                    )
                                })
                            )
                        ]
                    },
                    semi_colon: SepSemiColon(
                        None,
                        TokenStream {
                            offset: 18,
                            value: ";"
                        },
                        None
                    )
                }),
                TokenStream {
                    offset: 19,
                    value: ""
                }
            ))
        );
    }
}
