use parserc::{Kind, Parse, Parser, ParserExt, keyword, next};

use crate::lang::{NamedField, UnameField, delimited, parse_attr_comment_list, ws};

use super::{
    AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, Token, Type, TypeReturn,
    skip_ws,
};

/// Function body block.
#[derive(Debug, PartialEq, Clone)]
pub enum FnArg<I> {
    Named {
        /// optional attribute/comment list.
        attr_comment_list: Vec<AttrOrComment<I>>,
        /// required attribute ident.
        ident: Ident<I>,
        /// punct: `:`
        colon: I,
        /// required attribute type declaration.
        ty: Type<I>,
    },
    Uname {
        /// optional attribute/comment list.
        attr_comment_list: Vec<AttrOrComment<I>>,
        /// required attribute type declaration.
        ty: Type<I>,
    },
}

impl<I> Parse<I> for FnArg<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        NamedField::into_parser()
            .map(|field: NamedField<I>| Self::Named {
                attr_comment_list: field.attr_comment_list,
                ident: field.ident,
                colon: field.colon,
                ty: field.ty,
            })
            .or(
                UnameField::into_parser().map(|field: UnameField<I>| Self::Uname {
                    attr_comment_list: field.attr_comment_list,
                    ty: field.ty,
                }),
            )
            .parse(input)
    }
}

/// Function body block.
#[derive(Debug, PartialEq, Clone)]
pub enum FnBlock<I> {
    SemiColon(I),
    Stats {
        /// delimiter for body: `{...}`
        delimiter: Delimiter<I>,
    },
}

impl<I> Parse<I> for FnBlock<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (semi_colon, input) = next(b';').parse(input)?;

        Ok((Self::SemiColon(semi_colon), input))
    }
}

/// Function declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct Fn<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// optional keyword `extern`
    pub extern_keyword: Option<I>,
    /// func keyword `fn`
    pub keyword: I,
    /// function ident name.
    pub ident: Ident<I>,
    /// parameter list delimiter: `(...)`
    pub delimiter: Delimiter<I>,
    /// function input arguments.
    pub inputs: Punctuated<I, FnArg<I>, b','>,
    /// optional returns argument type.
    pub return_ty: Option<TypeReturn<I>>,
    /// fn body block.
    pub block: FnBlock<I>,
}

impl<I> Parse<I> for Fn<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (extern_keyword, input) = keyword("extern").ok().parse(input)?;

        let input = if extern_keyword.is_some() {
            let (_, input) = ws(input)?;
            input
        } else {
            input
        };

        let (keyword, input) = keyword("fn")
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Keyword("fn"), input.span()))
            .parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, inputs), input) =
            delimited("(", Punctuated::into_parser(), ")").parse(input)?;

        let (return_ty, input) = TypeReturn::into_parser().ok().parse(input)?;

        let (block, input) = FnBlock::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                extern_keyword,
                keyword,
                ident,
                delimiter,
                inputs,
                return_ty,
                block,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Delimiter, FnArg, FnBlock, Ident, Punctuated, TokenStream, Type,
    };

    use super::Fn;

    #[test]
    fn parse_fn_without_returns() {
        assert_eq!(
            Fn::parse(TokenStream::from(
                "@platform extern fn label(Label, TextLayout, Fill);",
            )),
            Ok((
                Fn {
                    attr_comment_list: vec![AttrOrComment::Attr(Attr {
                        keyword: TokenStream::from((0, "@")),
                        ident: Ident(TokenStream::from((1, "platform"))),
                        body: None
                    })],
                    extern_keyword: Some(TokenStream::from((10, "extern"))),
                    keyword: TokenStream::from((17, "fn")),
                    ident: Ident(TokenStream::from((20, "label"))),
                    delimiter: Delimiter {
                        start: TokenStream::from((25, "(")),
                        end: TokenStream::from((49, ")")),
                    },
                    inputs: Punctuated {
                        items: vec![
                            (
                                FnArg::Uname {
                                    attr_comment_list: vec![],
                                    ty: Type::Ident(Ident(TokenStream::from((26, "Label"))))
                                },
                                TokenStream::from((31, ","))
                            ),
                            (
                                FnArg::Uname {
                                    attr_comment_list: vec![],
                                    ty: Type::Ident(Ident(TokenStream::from((33, "TextLayout"))))
                                },
                                TokenStream::from((43, ","))
                            )
                        ],
                        last: Some(Box::new(FnArg::Uname {
                            attr_comment_list: vec![],
                            ty: Type::Ident(Ident(TokenStream::from((45, "Fill"))))
                        }))
                    },
                    return_ty: None,
                    block: FnBlock::SemiColon(TokenStream::from((50, ";")))
                },
                TokenStream::from((51, ""))
            ))
        );
    }
}
