use parserc::{Parse, Parser, ParserExt};

use crate::lang::{errors::LangError, expr::partial::Partial, inputs::LangInput};

use super::{
    ExprArray, ExprAssign, ExprBinary, ExprBlock, ExprBreak, ExprChain, ExprFor, ExprIf, ExprLet,
    ExprLoop, ExprParen, ExprRange, ExprRepeat, ExprReturn, ExprUnary, ExprXml, RangeWithoutStart,
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
    Chain(ExprChain<I>),
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
            .or(ExprBlock::into_parser().map(|v| Self::Block(v)))
            .or(ExprArray::into_parser().map(|v| Self::Array(v)))
            .or(ExprUnary::into_parser().map(|v| Self::Unary(v)))
            .or(ExprParen::into_parser().map(|v| Self::Paren(v)))
            .or(ExprReturn::into_parser().map(|v| Self::Return(v)))
            .or(ExprBreak::into_parser().map(|v| Self::Break(v)))
            .or(ExprFor::into_parser().map(|v| Self::For(v)))
            .or(ExprLoop::into_parser().map(|v| Self::Loop(v)))
            .or(ExprRepeat::into_parser().map(|v| Self::Repeat(v)))
            .or(RangeWithoutStart::into_parser().map(|v| Self::Range(v.into())))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        let (chain, input) = ExprChain::parse(input)?;

        let (expr, input) = Partial::from(chain.clone())
            .map(|v| Self::Binary(v))
            .or(Partial::from(chain.clone()).map(|v| Self::Assign(v)))
            .or(Partial::from(chain.clone()).map(|v| Self::Range(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        Ok((Self::Chain(chain), input))
    }
}
