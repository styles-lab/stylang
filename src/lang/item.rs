use parserc::derive_parse;

use super::{
    Colon, Comma, Ident, KeywordClass, KeywordData, KeywordEnum, LeftCurlyBracket, LeftParenthesis,
    MetaList, ParseError, Punctuated, RightCurlyBracket, RightParenthesis, S, SemiColon,
    StylangInput, Visibility, ty::Type,
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
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// field name.
    pub name: Ident<I>,
    /// colon token: `:`
    pub colon: (Option<S<I>>, Colon<I>, Option<S<I>>),
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
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// field type.
    pub ty: Type<I>,
}

/// Field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Fields<I>
where
    I: StylangInput,
{
    Named {
        delimiter_start: LeftCurlyBracket<I>,
        fields: Punctuated<NamedField<I>, Comma<I>>,
        delimiter_end: RightCurlyBracket<I>,
    },
    Uname {
        delimiter_start: LeftParenthesis<I>,
        fields: Punctuated<UnameField<I>, Comma<I>>,
        delimiter_end: RightParenthesis<I>,
    },
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
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `class`
    pub keyword: (KeywordClass<I>, S<I>),
    /// class name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// field list
    pub fields: Fields<I>,
    /// optional semi-colon: `;`
    pub semi_colon: Option<SemiColon<I>>,
}

/// Data item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ItemData<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `data`
    pub keyword: (KeywordData<I>, S<I>),
    /// data name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// field list
    pub fields: Fields<I>,
    /// optional semi-colon: `;`
    pub semi_colon: Option<SemiColon<I>>,
}

/// Enum variant field.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct Variant<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// variant name.
    pub name: (Ident<I>, Option<S<I>>),
    /// variant fields
    pub fields: Option<Fields<I>>,
}

/// Enum item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ItemEnum<I>
where
    I: StylangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `enum`
    pub keyword: (KeywordEnum<I>, S<I>),
    /// data name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// delimiter `{`
    pub delimiter_start: LeftCurlyBracket<I>,
    ///  variant list.
    pub variants: Punctuated<Variant<I>, Comma<I>>,
    /// delimiter `}`
    pub delimiter_end: RightCurlyBracket<I>,
}

/// stylang item variant.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Item<I>
where
    I: StylangInput,
{
    Class(ItemClass<I>),
    Data(ItemData<I>),
    Enum(ItemEnum<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        ArrowRight, At, Attr, Comment, I32, KeywordColor, KeywordFn, KeywordString, KeywordView,
        Meta, OutlineDoc, TokenStream,
        ty::{TypeFn, TypeReturn},
    };

    use super::*;

    #[test]
    fn test_class() {
        let input = TokenStream::from(
            r#"
            /// Fill properties.
            class Fill {
                foreground_color: color,
                background_color: color,
            }
            "#,
        );

        assert_eq!(
            ItemClass::parse(input),
            Ok((
                ItemClass {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((16, " Fill properties."))
                    ))),]),
                    vs: None,
                    keyword: (
                        KeywordClass(TokenStream::from((46, "class"))),
                        S(TokenStream::from((51, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((52, "Fill"))),
                        Some(S(TokenStream::from((56, " "))))
                    ),
                    fields: Fields::Named {
                        delimiter_start: LeftCurlyBracket(TokenStream::from((57, "{"))),
                        fields: Punctuated {
                            items: vec![
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![]),
                                        vs: None,
                                        name: Ident(TokenStream::from((75, "foreground_color"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((91, ":"),),),
                                            Some(S(TokenStream::from((92, " "))))
                                        ),
                                        ty: Type::Color(KeywordColor(TokenStream::from((
                                            93, "color"
                                        ))))
                                    },
                                    Comma(TokenStream::from((98, ",")))
                                ),
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![]),
                                        vs: None,
                                        name: Ident(TokenStream::from((116, "background_color"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((132, ":"),),),
                                            Some(S(TokenStream::from((133, " "))))
                                        ),
                                        ty: Type::Color(KeywordColor(TokenStream::from((
                                            134, "color"
                                        ))))
                                    },
                                    Comma(TokenStream::from((139, ",")))
                                )
                            ],
                            last: None
                        },
                        delimiter_end: RightCurlyBracket(TokenStream::from((153, "}")))
                    },
                    semi_colon: None
                },
                TokenStream::from((154, "\n            "))
            ))
        );
    }

    #[test]
    fn test_data() {
        let input = TokenStream::from(
            r#"
            /// All fields of a `state` are default required.
            data Label {
                /// The fields is a value object passed by value.
                @state text: string,
                /// optional content builder function.
                @option content: fn() -> view,
            }
            "#,
        );

        assert_eq!(
            ItemData::parse(input),
            Ok((
                ItemData {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((16, " All fields of a `state` are default required."))
                    ))),]),
                    vs: None,
                    keyword: (
                        KeywordData(TokenStream::from((75, "data"))),
                        S(TokenStream::from((79, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((80, "Label"))),
                        Some(S(TokenStream::from((85, " "))))
                    ),
                    fields: Fields::Named {
                        delimiter_start: LeftCurlyBracket(TokenStream::from((86, "{"))),
                        fields: Punctuated {
                            items: vec![
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![
                                            Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                                                TokenStream::from((
                                                    107,
                                                    " The fields is a value object passed by value."
                                                ))
                                            ))),
                                            Meta::Attr(Attr {
                                                keyword: At(TokenStream::from((170, "@"))),
                                                ident: Ident(TokenStream::from((171, "state"))),
                                                s1: Some(S(TokenStream::from((176, " ")))),
                                                params: None
                                            })
                                        ]),
                                        vs: None,
                                        name: Ident(TokenStream::from((177, "text"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((181, ":"),),),
                                            Some(S(TokenStream::from((182, " "))))
                                        ),
                                        ty: Type::String(KeywordString(TokenStream::from((
                                            183, "string"
                                        ))))
                                    },
                                    Comma(TokenStream::from((189, ",")))
                                ),
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![
                                            Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                                                TokenStream::from((
                                                    210,
                                                    " optional content builder function."
                                                ))
                                            ))),
                                            Meta::Attr(Attr {
                                                keyword: At(TokenStream::from((262, "@"))),
                                                ident: Ident(TokenStream::from((263, "option"))),
                                                s1: Some(S(TokenStream::from((269, " ")))),
                                                params: None
                                            })
                                        ]),
                                        vs: None,
                                        name: Ident(TokenStream::from((270, "content"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((277, ":"),),),
                                            Some(S(TokenStream::from((278, " "))))
                                        ),
                                        ty: Type::Fn(TypeFn {
                                            keyword_fn: KeywordFn(TokenStream::from((279, "fn"))),
                                            delimiter_start: LeftParenthesis(TokenStream::from((
                                                281, "("
                                            ))),
                                            inputs: Punctuated {
                                                items: vec![],
                                                last: None
                                            },
                                            delimiter_end: RightParenthesis(TokenStream::from((
                                                282, ")"
                                            ))),
                                            output: Some(TypeReturn {
                                                arrow_right: (
                                                    Some(S(TokenStream::from((283, " ")))),
                                                    ArrowRight(TokenStream::from((284, "->"))),
                                                    Some(S(TokenStream::from((286, " "))))
                                                ),
                                                ty: Box::new(Type::View(KeywordView(
                                                    TokenStream::from((287, "view"))
                                                )))
                                            })
                                        })
                                    },
                                    Comma(TokenStream::from((291, ",")))
                                )
                            ],
                            last: None
                        },
                        delimiter_end: RightCurlyBracket(TokenStream::from((305, "}"))),
                    },
                    semi_colon: None,
                },
                TokenStream::from((306, "\n            "))
            ))
        );
    }

    #[test]
    fn test_enum() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(
                r#"
                /// This property describes decorations that are added to the text of an element.
                enum TextDecoration {
                    Underline,
                    Overline,
                    LineThrough,
                    Blink,
                }
                "#
            )),
            Ok((
                ItemEnum {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((
                            20,
                            " This property describes decorations that are added to the text of an element."
                        ))
                    )))]),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((115, "enum"))),
                        S(TokenStream::from((119, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((120, "TextDecoration"))),
                        Some(S(TokenStream::from((134, " "))))
                    ),
                    delimiter_start: LeftCurlyBracket(TokenStream::from((135, "{"))),
                    variants: Punctuated {
                        items: vec![
                            (
                                Variant {
                                    meta_list: MetaList(vec![]),
                                    name: (Ident(TokenStream::from((157, "Underline"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((166, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList(vec![]),
                                    name: (Ident(TokenStream::from((188, "Overline"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((196, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList(vec![]),
                                    name: (Ident(TokenStream::from((218, "LineThrough"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((229, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList(vec![]),
                                    name: (Ident(TokenStream::from((251, "Blink"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((256, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightCurlyBracket(TokenStream::from((274, "}")))
                },
                TokenStream::from((275, "\n                "))
            ))
        );
    }

    #[test]
    fn test_enum2() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(r#"enum A { V(i32) }"#)),
            Ok((
                ItemEnum {
                    meta_list: MetaList(vec![]),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((0, "enum"))),
                        S(TokenStream::from((4, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((5, "A"))),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    delimiter_start: LeftCurlyBracket(TokenStream::from((7, "{"))),
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            meta_list: MetaList(vec![]),
                            name: (Ident(TokenStream::from((9, "V"))), None),
                            fields: Some(Fields::Uname {
                                delimiter_start: LeftParenthesis(TokenStream::from((10, "("))),
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(UnameField {
                                        meta_list: MetaList(vec![]),
                                        vs: None,
                                        ty: Type::I32(I32(TokenStream::from((11, "i32"))))
                                    })),
                                },
                                delimiter_end: RightParenthesis(TokenStream::from((14, ")")))
                            })
                        }))
                    },
                    delimiter_end: RightCurlyBracket(TokenStream::from((16, "}")))
                },
                TokenStream::from((17, ""))
            ))
        );
    }

    #[test]
    fn test_enum3() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(r#"enum A { V {v:i32} }"#)),
            Ok((
                ItemEnum {
                    meta_list: MetaList(vec![]),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((0, "enum"))),
                        S(TokenStream::from((4, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((5, "A"))),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    delimiter_start: LeftCurlyBracket(TokenStream::from((7, "{"))),
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            meta_list: MetaList(vec![]),
                            name: (
                                Ident(TokenStream::from((9, "V"))),
                                Some(S(TokenStream::from((10, " "))))
                            ),
                            fields: Some(Fields::Named {
                                delimiter_start: LeftCurlyBracket(TokenStream::from((11, "{"))),
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(NamedField {
                                        meta_list: MetaList(vec![]),
                                        vs: None,
                                        name: Ident(TokenStream::from((12, "v"))),
                                        colon: (None, Colon(TokenStream::from((13, ":"))), None),
                                        ty: Type::I32(I32(TokenStream::from((14, "i32"))))
                                    })),
                                },
                                delimiter_end: RightCurlyBracket(TokenStream::from((17, "}")))
                            })
                        }))
                    },
                    delimiter_end: RightCurlyBracket(TokenStream::from((19, "}")))
                },
                TokenStream::from((20, ""))
            ))
        );
    }
}
