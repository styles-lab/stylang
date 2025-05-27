use parserc::{Parse, Parser, ParserExt};

use crate::lang::{errors::LangError, expr::partial::Partial, inputs::LangInput};

use super::{
    ExprArray, ExprAssign, ExprBinary, ExprBlock, ExprBreak, ExprCall, ExprField, ExprFor, ExprIf,
    ExprIndex, ExprLet, ExprLit, ExprLoop, ExprMatch, ExprParen, ExprPath, ExprRange, ExprRepeat,
    ExprReturn, ExprTuple, ExprUnary, ExprXml, RangeTail, caudal::CaudalRecursion,
};

/// A `stylang` expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    Let(ExprLet<I>),
    If(ExprIf<I>),
    Xml(ExprXml<I>),
    Block(ExprBlock<I>),
    Array(ExprArray<I>),
    Unary(ExprUnary<I>),
    Paren(ExprParen<I>),
    Return(ExprReturn<I>),
    Break(ExprBreak<I>),
    For(ExprFor<I>),
    Loop(ExprLoop<I>),
    Repeat(ExprRepeat<I>),
    Binary(ExprBinary<I>),
    Assign(ExprAssign<I>),
    Range(ExprRange<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Call(ExprCall<I>),
    Index(ExprIndex<I>),
    Field(ExprField<I>),
    Tuple(ExprTuple<I>),
    Match(ExprMatch<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (expr, input) = ExprLet::into_parser()
            .map(|v| Self::Let(v))
            .or(ExprIf::into_parser().map(|v| Self::If(v)))
            .or(ExprXml::into_parser().map(|v| Self::Xml(v)))
            .or(ExprParen::into_parser().map(|v| Self::Paren(v)))
            .or(ExprTuple::into_parser().map(|v| Self::Tuple(v)))
            .or(ExprBlock::into_parser().map(|v| Self::Block(v)))
            .or(ExprArray::into_parser().map(|v| Self::Array(v)))
            .or(ExprUnary::into_parser().map(|v| Self::Unary(v)))
            .or(ExprReturn::into_parser().map(|v| Self::Return(v)))
            .or(ExprBreak::into_parser().map(|v| Self::Break(v)))
            .or(ExprFor::into_parser().map(|v| Self::For(v)))
            .or(ExprLoop::into_parser().map(|v| Self::Loop(v)))
            .or(ExprRepeat::into_parser().map(|v| Self::Repeat(v)))
            .or(RangeTail::into_parser().map(|v| Self::Range(v.into())))
            .or(ExprMatch::into_parser().map(|v| Self::Match(v.into())))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        let (cr, input) = CaudalRecursion::parse(input)?;

        let (expr, input) = Partial::from(cr.clone())
            .map(|v| Self::Binary(v))
            .or(Partial::from(cr.clone()).map(|v| Self::Assign(v)))
            .or(Partial::from(cr.clone()).map(|v| Self::Range(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        Ok((cr.into(), input))
    }
}
