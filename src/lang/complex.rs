use parserc::{Kind, Parse, Parser, ParserExt, keyword};

use crate::lang::{delimited, parse_attr_comment_list, parse_punctuation_sep, skip_ws};

use super::{AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, Token, Type};

/// Parsed attribute declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct NamedField<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// required attribute ident.
    pub ident: Ident<I>,
    /// punct: `:`
    pub semi_colon: I,
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
                semi_colon,
                ty,
            },
            input,
        ))
    }
}

/// Parsed class type declaration.
#[derive(Debug, PartialEq, Clone)]
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

        let (_, input) = skip_ws(input)?;

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

        let (_, input) = skip_ws(input)?;

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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Comment, Data, Delimiter, Ident, NamedField, Punctuated, TokenStream,
        Type, TypeFn, TypeReturn,
    };

    use super::Class;

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
                        prefix: TokenStream::from((57, "{")),
                        suffix: TokenStream::from((153, "}")),
                    },
                    named_fields: Punctuated {
                        items: vec![
                            (
                                NamedField {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((75, "foreground_color"))),
                                    semi_colon: TokenStream::from((91, ":")),
                                    ty: Type::Primary(TokenStream::from((93, "color")))
                                },
                                TokenStream::from((98, ","))
                            ),
                            (
                                NamedField {
                                    attr_comment_list: vec![],
                                    ident: Ident(TokenStream::from((116, "background_color"))),
                                    semi_colon: TokenStream::from((132, ":")),
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
                        prefix: TokenStream::from((86, "{")),
                        suffix: TokenStream::from((305, "}")),
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
                                            prefix: TokenStream::from((170, "@")),
                                            ident: Ident(TokenStream::from((171, "state"))),
                                            body: None
                                        })
                                    ],
                                    ident: Ident(TokenStream::from((177, "text"))),
                                    semi_colon: TokenStream::from((181, ":")),
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
                                            prefix: TokenStream::from((262, "@")),
                                            ident: Ident(TokenStream::from((263, "option"))),
                                            body: None
                                        })
                                    ],
                                    ident: Ident(TokenStream::from((270, "content"))),
                                    semi_colon: TokenStream::from((277, ":")),
                                    ty: Type::Fn(TypeFn {
                                        prefix: TokenStream::from((279, "fn")),
                                        delimiter: Delimiter {
                                            prefix: TokenStream::from((281, "(")),
                                            suffix: TokenStream::from((282, ")"))
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
}
