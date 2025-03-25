use parserc::{Kind, Parse, Parser, ParserExt, keyword, next};

use crate::lang::{ParseKind, parse_comments, skip_ws};

use super::{
    Comment, Delimiter, Ident, ParseError, Punctuated, TokenStream, Type, parse_return_type_arrow,
};

#[derive(Debug, PartialEq)]
pub enum Stat<I> {
    LitStr(I),
}

/// Fn returns stmt, be like: `-> i32`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ReturnType<I> {
    pub arrow_token: I,
    pub ty: Box<Type<I>>,
}

impl<I> Parse<I> for ReturnType<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (arrow_token, input) = parse_return_type_arrow(input)?;
        let (ty, input) = Type::parse(input)?;

        Ok((
            ReturnType {
                arrow_token,
                ty: Box::new(ty),
            },
            input,
        ))
    }
}

/// Parsed fn argument.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FnArg<I> {
    /// argument name.
    pub ident: Ident<I>,
    /// colon token: `:`
    pub colon_token: I,
    /// argument type,
    pub ty: Type<I>,
}

impl<I> Parse<I> for FnArg<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (ident, input) = Ident::into_parser().parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (colon_token, input) = next(b':')
            .map_err(|input: I, _: Kind| ParseError::Expect(ParseKind::Colon, input.span()))
            .fatal()
            .parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ty, input) = Type::into_parser()
            .map_err(|input: I, _| ParseError::Expect(ParseKind::FnArgType, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            FnArg {
                ident,
                colon_token,
                ty,
            },
            input,
        ))
    }
}

/// Parsed fn signature.
#[derive(Debug, PartialEq)]
pub struct Signature<I> {
    /// keyword `fn`
    pub keyword: I,
    /// reqired fn name.
    pub ident: Ident<I>,
    /// params delimeter: `(...)`
    pub delimiter: Delimiter<I>,
    /// params list.
    pub params: Punctuated<I, FnArg<I>, b','>,
    /// returns type.
    pub return_type: Option<ReturnType<I>>,
}

impl<I> Parse<I> for Signature<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        // let (comments, input) = parse_comments(input)?;

        let (kw, input) = keyword("fn").parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ident, input) = Ident::into_parser().fatal().parse(input)?;

        let (_, input) = skip_ws(input)?;

        let ((delimiter, params), input) = Delimiter::paren(input)?;

        let (return_type, input) = ReturnType::into_parser().ok().parse(input)?;

        Ok((
            Signature {
                keyword: kw,
                ident,
                delimiter,
                params,
                return_type,
            },
            input,
        ))
    }
}

/// Parsed extern fn declaration, be like: `extern fn xxx(...);`
#[derive(Debug, PartialEq)]
pub struct ItemExternFn<I> {
    /// optional comment list.
    pub comments: Vec<Comment<I>>,
    /// extern keyword: `extern`
    pub extern_token: I,
    /// The function signature.
    pub signature: Signature<I>,
    /// The end tag of extern fn, `;`
    pub semi_token: I,
}

impl<I> Parse<I> for ItemExternFn<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comments, input) = parse_comments(input)?;

        let (extern_token, input) = keyword("extern").parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (signature, input) = Signature::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (semi_token, input) = next(b';').parse(input)?;

        Ok((
            ItemExternFn {
                comments,
                extern_token,
                signature,
                semi_token,
            },
            input,
        ))
    }
}

/// Parsed fn block, be like: `fn xxx(...) -> xxx { ... }`
#[derive(Debug, PartialEq)]
pub struct ItemFn<I> {
    /// optional comment list.
    pub comments: Vec<Comment<I>>,
    /// The function signature.
    pub signature: Signature<I>,
    /// fn body block.
    pub body: Vec<Stat<I>>,
}

impl<I> Parse<I> for ItemFn<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comments, input) = parse_comments(input)?;

        let (signature, input) = Signature::parse(input)?;

        Ok((
            ItemFn {
                comments,
                signature,
                body: vec![],
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Delimiter, FnArg, Ident, ItemExternFn, Punctuated, ReturnType, Source, Type,
    };

    use super::Signature;

    #[test]
    fn test_signature() {
        assert_eq!(
            Signature::parse(Source::from("fn mainView()")),
            Ok((
                Signature {
                    keyword: Source::from((0, "fn")),
                    ident: Ident(Source::from((3, "mainView"))),
                    delimiter: Delimiter::Paren(Source::from((11, "(")), Source::from((12, ")"))),
                    params: Punctuated {
                        items: vec![],
                        last: None
                    },
                    return_type: None
                },
                Source::from((13, ""))
            ))
        );

        assert_eq!(
            Signature::parse(Source::from("fn _man-view ( ) -> view")),
            Ok((
                Signature {
                    keyword: Source::from((0, "fn")),
                    ident: Ident(Source::from((3, "_man-view"))),
                    delimiter: Delimiter::Paren(Source::from((13, "(")), Source::from((15, ")"))),
                    params: Punctuated {
                        items: vec![],
                        last: None
                    },
                    return_type: Some(ReturnType {
                        arrow_token: Source::from((17, "->")),
                        ty: Box::new(Type::View(Source::from((20, "view"))))
                    })
                },
                Source::from((24, ""))
            ))
        );
    }

    #[test]
    fn parse_extern_fn() {
        assert_eq!(
            ItemExternFn::parse(Source::from(
                "extern fn _man-view ( content: () -> view,) -> view ;"
            )),
            Ok((
                ItemExternFn {
                    comments: vec![],
                    extern_token: Source::from((0, "extern")),
                    signature: Signature {
                        keyword: Source::from((7, "fn")),
                        ident: Ident(Source::from((10, "_man-view"))),
                        delimiter: Delimiter::Paren(
                            Source::from((20, "(")),
                            Source::from((42, ")"))
                        ),
                        params: Punctuated {
                            items: vec![(
                                FnArg {
                                    ident: Ident(Source::from((22, "content"))),
                                    colon_token: Source::from((29, ":")),
                                    ty: Type::Fn {
                                        delimiter: Delimiter::Paren(
                                            Source::from((31, "(")),
                                            Source::from((32, ")"))
                                        ),
                                        params: Punctuated {
                                            items: vec![],
                                            last: None
                                        },
                                        return_stmt: Some(ReturnType {
                                            arrow_token: Source::from((34, "->")),
                                            ty: Box::new(Type::View(Source::from((37, "view"))))
                                        }),
                                    }
                                },
                                Source::from((41, ","))
                            )],
                            last: None
                        },
                        return_type: Some(ReturnType {
                            arrow_token: Source::from((44, "->")),
                            ty: Box::new(Type::View(Source::from((47, "view"))))
                        })
                    },
                    semi_token: Source::from((52, ";"))
                },
                Source::from((53, ""))
            ))
        );
    }
}
