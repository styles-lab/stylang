use parserc::{Parse, Parser, ParserExt, derive_parse, span::Span};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{DotDot, DotDotEq},
};

use super::{Expr, ExprLit};

/// inner start field parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum RangeStart<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum RangeLimits<I>
where
    I: LangInput,
{
    Closed(DotDotEq<I>),
    HalfOpen(DotDot<I>),
}

impl<I> From<RangeLimits<I>> for Span
where
    I: LangInput,
{
    fn from(value: RangeLimits<I>) -> Self {
        match value {
            RangeLimits::Closed(v) => v.0.span(),
            RangeLimits::HalfOpen(v) => v.0.span(),
        }
    }
}

impl<I> From<RangeStart<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: RangeStart<I>) -> Self {
        match value {
            RangeStart::Lit(v) => Self::Lit(v),
        }
    }
}

/// A range expression: 1..2, 1.., ..2, 1..=2, ..=2.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRange<I>
where
    I: LangInput,
{
    pub meta_list: MetaList<I>,
    /// optional range start expr.
    pub start: Option<Box<Expr<I>>>,
    /// Range limits token.
    pub limits: RangeLimits<I>,
    /// optional range end expr.
    pub end: Option<Box<Expr<I>>>,
}

impl<I> Parse<I> for ExprRange<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;

        let (start, input) = RangeStart::into_parser()
            .map(|v| Box::new(Expr::from(v)))
            .ok()
            .parse(input)?;
        let (limits, input) = RangeLimits::parse(input)?;
        let (end, input) = Expr::into_parser().boxed().ok().parse(input)?;

        // Invalid range expression, at least one of the start and end fields is non-null.
        if start.is_none() && end.is_none() {
            return Err(parserc::ControlFlow::Fatal(LangError::unexpect(
                TokenKind::RangeLimits,
                limits.into(),
            )));
        }

        Ok((
            Self {
                meta_list,
                start,
                limits,
                end,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        expr::{Expr, ExprLit, ExprRange, RangeLimits},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        meta::MetaList,
        tokens::{Digits, DotDot, DotDotEq},
    };

    #[test]
    fn test_range_half_open() {
        assert_eq!(
            Expr::parse(TokenStream::from("1..2")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from("1"))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    }))),
                    limits: RangeLimits::HalfOpen(DotDot(TokenStream::from((1, "..")))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((3, "2")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })))
                }),
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("1..")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from("1"))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    }))),
                    limits: RangeLimits::HalfOpen(DotDot(TokenStream::from((1, "..")))),
                    end: None
                }),
                TokenStream::from((3, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("..2")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: None,
                    limits: RangeLimits::HalfOpen(DotDot(TokenStream::from((0, "..")))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((2, "2")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })))
                }),
                TokenStream::from((3, ""))
            ))
        );
    }

    #[test]
    fn test_range_closed() {
        assert_eq!(
            Expr::parse(TokenStream::from("1..=2")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from("1"))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    }))),
                    limits: RangeLimits::Closed(DotDotEq(TokenStream::from((1, "..=")))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((4, "2")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })))
                }),
                TokenStream::from((5, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("1..=")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from("1"))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    }))),
                    limits: RangeLimits::Closed(DotDotEq(TokenStream::from((1, "..=")))),
                    end: None
                }),
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("..=2")),
            Ok((
                Expr::Range(ExprRange {
                    meta_list: MetaList(vec![]),
                    start: None,
                    limits: RangeLimits::Closed(DotDotEq(TokenStream::from((0, "..=")))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((3, "2")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })))
                }),
                TokenStream::from((4, ""))
            ))
        );
    }

    #[test]
    fn test_range_without_start_end() {
        assert_eq!(
            Expr::parse(TokenStream::from("..=")),
            Err(ControlFlow::Fatal(LangError::unexpect(
                TokenKind::RangeLimits,
                Span { offset: 0, len: 3 }
            )))
        );
        assert_eq!(
            Expr::parse(TokenStream::from("..")),
            Err(ControlFlow::Fatal(LangError::unexpect(
                TokenKind::RangeLimits,
                Span { offset: 0, len: 2 }
            )))
        );
    }
}
