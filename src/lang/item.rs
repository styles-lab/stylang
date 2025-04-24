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
    pub name: Ident<I>,
    /// variant fields
    pub fields: Fields<I>,
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
    pub keyword: KeywordEnum<I>,
    pub s1: Option<S<I>>,
    pub delimiter_start: LeftCurlyBracket<I>,
    pub s2: Option<S<I>>,
    ///  variant list.
    pub variants: Punctuated<Variant<I>, Comma<I>>,
    pub s3: Option<S<I>>,
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
        ArrowRight, At, Attr, Comment, KeywordColor, KeywordFn, KeywordString, KeywordView, Meta,
        OutlineDoc, TokenStream,
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

    // #[test]
    // fn test_enum() {
    //     assert_eq!(
    //         ItemEnum::parse(TokenStream::from(
    //             r#"
    //             /// This property describes decorations that are added to the text of an element.
    //             enum TextDecoration {
    //                 Underline,
    //                 Overline,
    //                 LineThrough,
    //                 Blink,
    //             }
    //             "#
    //         )),
    //         Ok((
    //             ItemEnum {
    //                 attr_comment_list: vec![AttrOrComment::Comment(Comment(TokenStream::from((
    //                     20,
    //                     " This property describes decorations that are added to the text of an element."
    //                 )))),],
    //                 keyword: TokenStream::from((115, "enum")),
    //                 ident: Ident(TokenStream::from((120, "TextDecoration"))),
    //                 delimiter: Delimiter {
    //                     start: TokenStream::from((135, "{")),
    //                     end: TokenStream::from((274, "}"))
    //                 },
    //                 variants: Punctuated {
    //                     items: vec![
    //                         (
    //                             Variant {
    //                                 attr_comment_list: vec![],
    //                                 ident: Ident(TokenStream::from((157, "Underline"))),
    //                                 fields: None
    //                             },
    //                             TokenStream::from((166, ","))
    //                         ),
    //                         (
    //                             Variant {
    //                                 attr_comment_list: vec![],
    //                                 ident: Ident(TokenStream::from((188, "Overline"))),
    //                                 fields: None
    //                             },
    //                             TokenStream::from((196, ","))
    //                         ),
    //                         (
    //                             Variant {
    //                                 attr_comment_list: vec![],
    //                                 ident: Ident(TokenStream::from((218, "LineThrough"))),
    //                                 fields: None
    //                             },
    //                             TokenStream::from((229, ","))
    //                         ),
    //                         (
    //                             Variant {
    //                                 attr_comment_list: vec![],
    //                                 ident: Ident(TokenStream::from((251, "Blink"))),
    //                                 fields: None
    //                             },
    //                             TokenStream::from((256, ","))
    //                         )
    //                     ],
    //                     last: None
    //                 }
    //             },
    //             TokenStream::from((275, "\n                "))
    //         ))
    //     );
    // }

    // #[test]
    // fn test_enum2() {
    //     assert_eq!(
    //         ItemEnum::parse(TokenStream::from(r#"enum A { V(i32) }"#)),
    //         Ok((
    //             ItemEnum {
    //                 attr_comment_list: vec![],
    //                 keyword: TokenStream::from((0, "enum")),
    //                 ident: Ident(TokenStream::from((5, "A"))),
    //                 delimiter: Delimiter {
    //                     start: TokenStream::from((7, "{")),
    //                     end: TokenStream::from((16, "}")),
    //                 },
    //                 variants: Punctuated {
    //                     items: vec![],
    //                     last: Some(Box::new(Variant {
    //                         attr_comment_list: vec![],
    //                         ident: Ident(TokenStream::from((9, "V"))),
    //                         fields: Some(Fields::Unamed {
    //                             delimiter: Delimiter {
    //                                 start: TokenStream::from((10, "(")),
    //                                 end: TokenStream::from((14, ")")),
    //                             },
    //                             fields: Punctuated {
    //                                 items: vec![],
    //                                 last: Some(Box::new(UnameField {
    //                                     attr_comment_list: vec![],
    //                                     ty: Type::Primary(TokenStream::from((11, "i32")))
    //                                 }))
    //                             }
    //                         })
    //                     }))
    //                 }
    //             },
    //             TokenStream::from((17, ""))
    //         ))
    //     );
    // }

    // #[test]
    // fn test_enum3() {
    //     assert_eq!(
    //         ItemEnum::parse(TokenStream::from(r#"enum A { V {v:i32} }"#)),
    //         Ok((
    //             ItemEnum {
    //                 attr_comment_list: vec![],
    //                 keyword: TokenStream::from((0, "enum")),
    //                 ident: Ident(TokenStream::from((5, "A"))),
    //                 delimiter: Delimiter {
    //                     start: TokenStream::from((7, "{")),
    //                     end: TokenStream::from((19, "}")),
    //                 },
    //                 variants: Punctuated {
    //                     items: vec![],
    //                     last: Some(Box::new(Variant {
    //                         attr_comment_list: vec![],
    //                         ident: Ident(TokenStream::from((9, "V"))),
    //                         fields: Some(Fields::Named {
    //                             delimiter: Delimiter {
    //                                 start: TokenStream::from((11, "{")),
    //                                 end: TokenStream::from((17, "}")),
    //                             },
    //                             fields: Punctuated {
    //                                 items: vec![],
    //                                 last: Some(Box::new(NamedField {
    //                                     attr_comment_list: vec![],
    //                                     ident: Ident(TokenStream::from((12, "v"))),
    //                                     colon: TokenStream::from((13, ":")),
    //                                     ty: Type::Primary(TokenStream::from((14, "i32")))
    //                                 }))
    //                             }
    //                         })
    //                     }))
    //                 }
    //             },
    //             TokenStream::from((20, ""))
    //         ))
    //     );
    // }
}
