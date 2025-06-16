use parserc::{
    inputs::lang::LangInput,
    parser::Parser,
    syntax::{Punctuated, Syntax},
};

use super::*;
use crate::lang::{
    errors::LangError,
    lit::Lit,
    meta::MetaList,
    token::{Bracket, Digits, Ident, Paren, S, SepComma, TokenDot},
    ty::TypePath,
};

/// Path `start element` parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
enum PathStart<I>
where
    I: LangInput,
{
    /// `ident {...}`
    Struct(ExprStruct<I>),
    /// `if {...} else ...`
    If(ExprIf<I>),
    /// `{...}`
    Block(ExprBlock<I>),
    /// `(...)`
    Tuple(ExprTuple<I>),
    /// `[10f32;10]`
    Repeat(ExprRepeat<I>),
    /// 1,"hello",...
    Lit(MetaList<I>, Lit<I>),
    /// `std::text::Font`
    TypePath(MetaList<I>, TypePath<I>),
    /// `a`
    Ident(MetaList<I>, Ident<I>),
}

impl<I> From<PathStart<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: PathStart<I>) -> Self {
        match value {
            PathStart::Struct(expr_struct) => Self::Struct(expr_struct),
            PathStart::If(expr_if) => Self::If(expr_if),
            PathStart::Block(expr_block) => Self::Block(expr_block),
            PathStart::Tuple(expr_tuple) => Self::Tuple(expr_tuple),
            PathStart::Repeat(expr_repeat) => Self::Repeat(expr_repeat),
            PathStart::Lit(metas, lit) => Self::Lit(metas, lit),
            PathStart::TypePath(metas, type_path) => Self::TypePath(metas, type_path),
            PathStart::Ident(metas, ident) => Self::Ident(metas, ident),
        }
    }
}

impl<I> Syntax<I, LangError> for PathStart<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        ExprStruct::into_parser()
            .map(|v| Self::Struct(v))
            .or(ExprIf::into_parser().map(|v| Self::If(v)))
            .or(ExprBlock::into_parser().map(|v| Self::Block(v)))
            .or(ExprTuple::into_parser().map(|v| Self::Tuple(v)))
            .or(ExprRepeat::into_parser().map(|v| Self::Repeat(v)))
            .or(<(MetaList<_>, Lit<_>)>::into_parser()
                .map(|(meta_list, v)| Self::Lit(meta_list, v)))
            .or(
                <(MetaList<_>, TypePath<_>)>::into_parser().map(|(meta_list, v)| {
                    if v.rest.is_empty() {
                        Self::Ident(meta_list, v.first)
                    } else {
                        Self::TypePath(meta_list, v)
                    }
                }),
            )
            .parse(input)
    }
}

/// `Path` call segement parser.
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive(Debug, PartialEq, Clone, Syntax)]
#[error(LangError)]
pub struct PathCall<I>(pub Paren<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// index parser for `Path` expr.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PathIndex<I>(pub Bracket<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// `Path` field index parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum PathField<I>
where
    I: LangInput,
{
    Name(Ident<I>),
    Uname(Digits<I>),
}

/// The rest element parser for expr `Path`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum PathSegment<I>
where
    I: LangInput,
{
    Field {
        dot_sep: (Option<S<I>>, TokenDot<I>, Option<S<I>>),
        field: PathField<I>,
    },
    Call(PathCall<I>),
    Index(PathIndex<I>),
}

/// A parser for expr `Path`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprPath<I>
where
    I: LangInput,
{
    /// The first element.
    pub first: Box<Expr<I>>,
    /// The rest path segment list.
    pub rest: Vec<PathSegment<I>>,
}

impl<I> Syntax<I, LangError> for ExprPath<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;

        let (first, input) = PathStart::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;

        let (rest, input) = input.parse()?;

        Ok((Self { first, rest }, input))
    }
}

impl<I> From<ExprPath<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: ExprPath<I>) -> Self {
        if value.rest.is_empty() {
            *value.first
        } else {
            Self::Path(value)
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        errors::ControlFlow,
        inputs::{Span, lang::TokenStream},
        syntax::Delimiter,
    };

    use crate::lang::{
        errors::SyntaxKind,
        lit::LitNum,
        token::{RangeLimits, TokenDotDot, TokenDotDotEq, TokenLeftBracket, TokenRightBracket},
    };

    use super::*;

    #[test]
    fn range() {
        assert_eq!(
            Expr::parse(TokenStream::from("a[..10]")),
            Ok((
                Expr::Path(ExprPath {
                    first: Box::new(Expr::Ident(
                        Default::default(),
                        Ident(TokenStream {
                            offset: 0,
                            value: "a"
                        })
                    )),

                    rest: vec![PathSegment::Index(PathIndex(Delimiter {
                        start: (
                            None,
                            TokenLeftBracket(TokenStream {
                                offset: 1,
                                value: "["
                            }),
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(Expr::Range(ExprRange {
                                start: None,
                                limits: RangeLimits::HalfOpen((
                                    None,
                                    TokenDotDot(TokenStream {
                                        offset: 2,
                                        value: ".."
                                    }),
                                    None
                                )),
                                end: Some(Box::new(Expr::Lit(
                                    Default::default(),
                                    Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream {
                                            offset: 4,
                                            value: "10"
                                        })),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: None
                                    })
                                ),))
                            })))
                        },
                        end: (
                            None,
                            TokenRightBracket(TokenStream {
                                offset: 6,
                                value: "]"
                            }),
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
                    limits: RangeLimits::Closed((
                        None,
                        TokenDotDotEq(TokenStream {
                            offset: 0,
                            value: "..="
                        }),
                        None
                    )),
                    end: Some(Box::new(Expr::Lit(
                        Default::default(),
                        Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 3,
                                value: "10"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ),))
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
                    start: Some(Box::new(Expr::Lit(
                        Default::default(),
                        Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 0,
                                value: "10"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ),)),
                    limits: RangeLimits::Closed((
                        None,
                        TokenDotDotEq(TokenStream {
                            offset: 2,
                            value: "..="
                        }),
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
                SyntaxKind::RightOperand,
                Span { offset: 2, len: 0 }
            )))
        );
    }
}
