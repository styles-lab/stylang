use parserc::{Parse, Parser, ParserExt};

use crate::lang::{errors::LangError, expr::partial::Partial, inputs::LangInput};

use super::{ExprAssign, ExprBinary, ExprBlock, ExprChain, ExprIf, ExprLet, ExprUnary, ExprXml};

/// A Rust expression.
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
    Unary(ExprUnary<I>),
    Binary(ExprBinary<I>),
    Assign(ExprAssign<I>),
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
            .or(ExprUnary::into_parser().map(|v| Self::Unary(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        let (chain, input) = ExprChain::parse(input)?;

        let (expr, input) = Partial::from(chain.clone())
            .map(|v| Self::Binary(v))
            .or(Partial::from(chain.clone()).map(|v| Self::Assign(v)))
            .ok()
            .parse(input)?;

        if let Some(expr) = expr {
            return Ok((expr, input));
        }

        return Ok((Self::Chain(chain), input));
    }
}
