use parserc::{Parse, Parser, ParserExt, PartialParse, derive_parse};

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    tokens::{DotDot, DotDotEq, S},
};

use super::{Expr, caudal::CaudalRecursion};

/// Limit types of a range, inclusive or exclusive.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum RangeLimits<I>
where
    I: LangInput,
{
    /// ..=
    Closed(DotDotEq<I>),
    /// ..
    HalfOpen(DotDot<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(super) struct RangeTail<I>
where
    I: LangInput,
{
    /// limit types.
    pub limits: RangeLimits<I>,
    /// optional end operand.
    pub end: CaudalRecursion<I>,
}

impl<I> Parse<I> for RangeTail<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (limits, input) = RangeLimits::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (end, input) = CaudalRecursion::into_parser().parse(input)?;

        Ok((Self { limits, end }, input))
    }
}

impl<I> From<RangeTail<I>> for ExprRange<I>
where
    I: LangInput,
{
    fn from(value: RangeTail<I>) -> Self {
        Self {
            start: None,
            limits: value.limits,
            end: Some(Box::new(value.end.into())),
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
    /// optional start operand.
    pub start: Option<Box<Expr<I>>>,
    /// limit types.
    pub limits: RangeLimits<I>,
    /// optional end operand.
    pub end: Option<Box<Expr<I>>>,
}

impl<I> PartialParse<I> for ExprRange<I>
where
    I: LangInput,
{
    type Error = LangError;
    type Parsed = CaudalRecursion<I>;

    fn parse(left: CaudalRecursion<I>, input: I) -> parserc::Result<Self, I, LangError> {
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (limits, input) = RangeLimits::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (end, input) = CaudalRecursion::into_parser().ok().parse(input)?;

        Ok((
            Self {
                start: Some(Box::new(left.into())),
                limits,
                end: end.map(|v| Box::new(v.into())),
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprLit, RangeLimits},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        meta::MetaList,
        tokens::{Digits, DotDotEq},
    };

    use super::ExprRange;

    #[test]
    fn test_range() {
        assert_eq!(
            Expr::parse(TokenStream::from("..=2")),
            Ok((
                Expr::Range(ExprRange {
                    start: None,
                    limits: RangeLimits::Closed(DotDotEq(TokenStream::from("..="))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList::default(),
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
            Expr::parse(TokenStream::from("1..=2")),
            Ok((
                Expr::Range(ExprRange {
                    start: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList::default(),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((0, "1")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    }))),
                    limits: RangeLimits::Closed(DotDotEq(TokenStream::from((1, "..=")))),
                    end: Some(Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList::default(),
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
    }
}
