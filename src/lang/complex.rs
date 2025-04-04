use parserc::{Kind, Parse, Parser, ParserExt, keyword};

use crate::lang::{delimited, parse_attr_comment_list, parse_punctuation_sep, skip_ws, ws};

use super::{AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, Token, Type};

/// Parsed position field declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct UnameField<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// required attribute type declaration.
    pub ty: Type<I>,
}

impl<I> Parse<I> for UnameField<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (ty, input) = Type::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                ty,
            },
            input,
        ))
    }
}

/// Parsed named field declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NamedField<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// required attribute ident.
    pub ident: Ident<I>,
    /// punct: `:`
    pub colon: I,
    /// required attribute type declaration.
    pub ty: Type<I>,
}

impl<I> Parse<I> for NamedField<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (ident, input) = Ident::parse(input)?;
        let (semi_colon, input) = parse_punctuation_sep(b':').parse(input)?;
        let (ty, input) = Type::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                ident,
                colon: semi_colon,
                ty,
            },
            input,
        ))
    }
}

/// Parsed class type declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Class<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// keyword: `class`.
    pub keyword: I,
    /// class ident.
    pub ident: Ident<I>,
    /// class body delimiter: `{...}`
    pub delimiter: Delimiter<I>,
    /// attr declartion list.
    pub named_fields: Punctuated<I, NamedField<I>, b','>,
}

impl<I> Parse<I> for Class<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (keyword, input) = keyword("class")
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Keyword("class"), input.span()))
            .parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, named_fields), input) =
            delimited("{", Punctuated::into_parser(), "}").parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                keyword,
                ident,
                delimiter,
                named_fields,
            },
            input,
        ))
    }
}

/// Parsed data type declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Data<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// keyword: `data`.
    pub keyword: I,
    /// data ident.
    pub ident: Ident<I>,
    /// data body delimiter: `{...}`
    pub delimiter: Delimiter<I>,
    /// attr declartion list.
    pub named_fields: Punctuated<I, NamedField<I>, b','>,
}

impl<I> Parse<I> for Data<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (keyword, input) = keyword("data")
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Keyword("data"), input.span()))
            .parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, named_fields), input) =
            delimited("{", Punctuated::into_parser(), "}").parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                keyword,
                ident,
                delimiter,
                named_fields,
            },
            input,
        ))
    }
}

/// Parsed enum field body.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Fields<I> {
    Unamed {
        /// delimiter `(...)`
        delimiter: Delimiter<I>,
        fields: Punctuated<I, UnameField<I>, b','>,
    },

    Named {
        /// delimiter `{...}`
        delimiter: Delimiter<I>,
        fields: Punctuated<I, NamedField<I>, b','>,
    },
}

impl<I> Parse<I> for Fields<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        delimited("(", Punctuated::into_parser(), ")")
            .map(|(delimiter, fields)| Self::Unamed { delimiter, fields })
            .or(delimited("{", Punctuated::into_parser(), "}")
                .map(|(delimiter, fields)| Self::Named { delimiter, fields }))
            .parse(input)
    }
}

/// Parsed enum field.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Variant<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// field name.
    pub ident: Ident<I>,
    /// variant fields: `(...)` or `{...}`
    pub fields: Option<Fields<I>>,
}

impl<I> Parse<I> for Variant<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (fields, input) = Fields::into_parser().ok().parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                ident,
                fields,
            },
            input,
        ))
    }
}

/// Parsed data type declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Enum<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// keyword: `enum`.
    pub keyword: I,
    /// the ident of the enum.
    pub ident: Ident<I>,
    /// body delimiter: `{...}`
    pub delimiter: Delimiter<I>,
    /// list.
    pub variants: Punctuated<I, Variant<I>, b','>,
}

impl<I> Parse<I> for Enum<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (keyword, input) = keyword("enum").parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, variants), input) =
            delimited("{", Punctuated::into_parser(), "}").parse(input)?;

        Ok((
            Self {
                delimiter,
                attr_comment_list,
                keyword,
                ident,
                variants,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Comment, Data, Delimiter, Fields, Ident, NamedField, Punctuated,
        TokenStream, Type, TypeFn, TypeReturn, UnameField, Variant,
    };

    use super::{Class, Enum};

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
            Class::parse(input),
            Ok((
                Class {
                    attr_comment_list: vec![AttrOrComment::Comment(Comment(TokenStream::from((
                        16,
                        " Fill properties."
                    )))),],
                    keyword: TokenStream::from((46, "class")),
                    ident: Ident(TokenStream::from((52, "Fill"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((57, "{")),
                        end: TokenStream::from((153, "}")),
                    },
                    named_fields: Punctuated {
                        items: vec![
                            (
                                NamedField {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((75, "foreground_color"))),
                                    colon: TokenStream::from((91, ":")),
                                    ty: Type::Primary(TokenStream::from((93, "color")))
                                },
                                TokenStream::from((98, ","))
                            ),
                            (
                                NamedField {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((116, "background_color"))),
                                    colon: TokenStream::from((132, ":")),
                                    ty: Type::Primary(TokenStream::from((134, "color")))
                                },
                                TokenStream::from((139, ","))
                            )
                        ],
                        last: None
                    }
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
            Data::parse(input),
            Ok((
                Data {
                    attr_comment_list: vec![AttrOrComment::Comment(Comment(TokenStream::from((
                        16,
                        " All fields of a `state` are default required."
                    )))),],
                    keyword: TokenStream::from((75, "data")),
                    ident: Ident(TokenStream::from((80, "Label"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((86, "{")),
                        end: TokenStream::from((305, "}")),
                    },
                    named_fields: Punctuated {
                        items: vec![
                            (
                                NamedField {
                                    attr_comment_list: vec![
                                        AttrOrComment::Comment(Comment(TokenStream::from((
                                            107,
                                            " The fields is a value object passed by value."
                                        )))),
                                        AttrOrComment::Attr(Attr {
                                            keyword: TokenStream::from((170, "@")),
                                            ident: Ident(TokenStream::from((171, "state"))),
                                            body: None
                                        })
                                    ],
                                    ident: Ident(TokenStream::from((177, "text"))),
                                    colon: TokenStream::from((181, ":")),
                                    ty: Type::Primary(TokenStream::from((183, "string")))
                                },
                                TokenStream::from((189, ","))
                            ),
                            (
                                NamedField {
                                    attr_comment_list: vec![
                                        AttrOrComment::Comment(Comment(TokenStream::from((
                                            210,
                                            " optional content builder function."
                                        )))),
                                        AttrOrComment::Attr(Attr {
                                            keyword: TokenStream::from((262, "@")),
                                            ident: Ident(TokenStream::from((263, "option"))),
                                            body: None
                                        })
                                    ],
                                    ident: Ident(TokenStream::from((270, "content"))),
                                    colon: TokenStream::from((277, ":")),
                                    ty: Type::Fn(TypeFn {
                                        prefix: TokenStream::from((279, "fn")),
                                        delimiter: Delimiter {
                                            start: TokenStream::from((281, "(")),
                                            end: TokenStream::from((282, ")"))
                                        },
                                        inputs: Punctuated {
                                            items: vec![],
                                            last: None
                                        },
                                        output: Some(TypeReturn {
                                            prefix: TokenStream::from((284, "->")),
                                            ty: Box::new(Type::Primary(TokenStream::from((
                                                287, "view"
                                            ))))
                                        })
                                    })
                                },
                                TokenStream::from((291, ","))
                            )
                        ],
                        last: None
                    }
                },
                TokenStream::from((306, "\n            "))
            ))
        );
    }

    #[test]
    fn test_enum() {
        assert_eq!(
            Enum::parse(TokenStream::from(
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
                Enum {
                    attr_comment_list: vec![AttrOrComment::Comment(Comment(TokenStream::from((
                        20,
                        " This property describes decorations that are added to the text of an element."
                    )))),],
                    keyword: TokenStream::from((115, "enum")),
                    ident: Ident(TokenStream::from((120, "TextDecoration"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((135, "{")),
                        end: TokenStream::from((274, "}"))
                    },
                    variants: Punctuated {
                        items: vec![
                            (
                                Variant {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((157, "Underline"))),
                                    fields: None
                                },
                                TokenStream::from((166, ","))
                            ),
                            (
                                Variant {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((188, "Overline"))),
                                    fields: None
                                },
                                TokenStream::from((196, ","))
                            ),
                            (
                                Variant {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((218, "LineThrough"))),
                                    fields: None
                                },
                                TokenStream::from((229, ","))
                            ),
                            (
                                Variant {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((251, "Blink"))),
                                    fields: None
                                },
                                TokenStream::from((256, ","))
                            )
                        ],
                        last: None
                    }
                },
                TokenStream::from((275, "\n                "))
            ))
        );
    }

    #[test]
    fn test_enum2() {
        assert_eq!(
            Enum::parse(TokenStream::from(r#"enum A { V(i32) }"#)),
            Ok((
                Enum {
                    attr_comment_list: vec![],
                    keyword: TokenStream::from((0, "enum")),
                    ident: Ident(TokenStream::from((5, "A"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((7, "{")),
                        end: TokenStream::from((16, "}")),
                    },
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            attr_comment_list: vec![],
                            ident: Ident(TokenStream::from((9, "V"))),
                            fields: Some(Fields::Unamed {
                                delimiter: Delimiter {
                                    start: TokenStream::from((10, "(")),
                                    end: TokenStream::from((14, ")")),
                                },
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(UnameField {
                                        attr_comment_list: vec![],
                                        ty: Type::Primary(TokenStream::from((11, "i32")))
                                    }))
                                }
                            })
                        }))
                    }
                },
                TokenStream::from((17, ""))
            ))
        );
    }

    #[test]
    fn test_enum3() {
        assert_eq!(
            Enum::parse(TokenStream::from(r#"enum A { V {v:i32} }"#)),
            Ok((
                Enum {
                    attr_comment_list: vec![],
                    keyword: TokenStream::from((0, "enum")),
                    ident: Ident(TokenStream::from((5, "A"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((7, "{")),
                        end: TokenStream::from((19, "}")),
                    },
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            attr_comment_list: vec![],
                            ident: Ident(TokenStream::from((9, "V"))),
                            fields: Some(Fields::Named {
                                delimiter: Delimiter {
                                    start: TokenStream::from((11, "{")),
                                    end: TokenStream::from((17, "}")),
                                },
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(NamedField {
                                        attr_comment_list: vec![],
                                        ident: Ident(TokenStream::from((12, "v"))),
                                        colon: TokenStream::from((13, ":")),
                                        ty: Type::Primary(TokenStream::from((14, "i32")))
                                    }))
                                }
                            })
                        }))
                    }
                },
                TokenStream::from((20, ""))
            ))
        );
    }
}
