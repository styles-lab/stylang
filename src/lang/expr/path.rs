use parserc::{Punctuated, derive_parse};

use crate::lang::{
    errors::LangError,
    expr::Expr,
    input::LangInput,
    lit::Lit,
    meta::MetaList,
    token::{Bracket, Digits, Ident, Paren, SepComma, SepDot},
    ty::TypePath,
};

/// Path `start element` parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum PathStart<I>
where
    I: LangInput,
{
    /// `std::text::Font`
    TypePath(TypePath<I>),
    /// 1,"hello",...
    Lit(Lit<I>),
}

/// `Path` call segement parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PathCall<I>(pub Paren<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// index parser for `Path` expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PathIndex<I>(pub Bracket<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// `Path` field index parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum PathField<I>
where
    I: LangInput,
{
    Name {
        /// `.`
        dot_sep: SepDot<I>,
        /// a,b,...
        name: Ident<I>,
    },
    Uname {
        /// `.`
        dot_sep: SepDot<I>,
        /// 1,10,..
        index: Digits<I>,
    },
}

/// The rest element parser for expr `Path`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum PathSegment<I>
where
    I: LangInput,
{
    Field(PathField<I>),
    Call(PathCall<I>),
    Index(PathIndex<I>),
}

/// A parser for expr `Path`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprPath<I>
where
    I: LangInput,
{
    /// The leading meta-data list.
    pub meta_list: MetaList<I>,
    /// The first element.
    pub first: PathStart<I>,
    /// The rest path segment list.
    pub rest: Vec<PathSegment<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Delimiter, Parse, Punctuated, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        expr::{Expr, ExprPath, ExprRange, PathIndex, PathSegment, PathStart},
        input::TokenStream,
        lit::{Lit, LitNum},
        token::*,
        ty::TypePath,
    };

    #[test]
    fn range() {
        assert_eq!(
            Expr::parse(TokenStream::from("a[..10]")),
            Ok((
                Expr::Path(ExprPath {
                    meta_list: vec![],
                    first: PathStart::TypePath(TypePath {
                        first: Ident(TokenStream {
                            offset: 0,
                            value: "a"
                        }),
                        rest: vec![]
                    }),
                    rest: vec![PathSegment::Index(PathIndex(Delimiter {
                        delimiter_start: SepLeftBracket(
                            None,
                            TokenStream {
                                offset: 1,
                                value: "["
                            },
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(Expr::Range(ExprRange {
                                start: None,
                                limits: RangeLimits::HalfOpen(SepDotDot(
                                    None,
                                    TokenStream {
                                        offset: 2,
                                        value: ".."
                                    },
                                    None
                                )),
                                end: Some(Box::new(Expr::Path(ExprPath {
                                    meta_list: vec![],
                                    first: PathStart::Lit(Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream {
                                            offset: 4,
                                            value: "10"
                                        })),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: None
                                    })),
                                    rest: vec![]
                                })))
                            })))
                        },
                        delimiter_end: SepRightBracket(
                            None,
                            TokenStream {
                                offset: 6,
                                value: "]"
                            },
                            None
                        )
                    }))]
                }),
                TokenStream {
                    offset: 7,
                    value: ""
                }
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("..=10")),
            Ok((
                Expr::Range(ExprRange {
                    start: None,
                    limits: RangeLimits::Closed(SepDotDotEq(
                        None,
                        TokenStream {
                            offset: 0,
                            value: "..="
                        },
                        None
                    )),
                    end: Some(Box::new(Expr::Path(ExprPath {
                        meta_list: vec![],
                        first: PathStart::Lit(Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 3,
                                value: "10"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })),
                        rest: vec![]
                    })))
                }),
                TokenStream {
                    offset: 5,
                    value: ""
                }
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("10..=")),
            Ok((
                Expr::Range(ExprRange {
                    start: Some(Box::new(Expr::Path(ExprPath {
                        meta_list: vec![],
                        first: PathStart::Lit(Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 0,
                                value: "10"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })),
                        rest: vec![]
                    }))),
                    limits: RangeLimits::Closed(SepDotDotEq(
                        None,
                        TokenStream {
                            offset: 2,
                            value: "..="
                        },
                        None
                    )),
                    end: None
                }),
                TokenStream {
                    offset: 5,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn invalid_range() {
        assert_eq!(
            Expr::parse(TokenStream::from("..")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::RightOperand,
                Span { offset: 2, len: 0 }
            )))
        );
    }
}
