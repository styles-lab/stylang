use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{errors::LangError, inputs::LangInput, lit::Lit, meta::MetaList};

use super::{XmlEnd, XmlStart};

/// A variable expr
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLit<I>
where
    I: LangInput,
{
    pub meta_list: MetaList<I>,
    pub lit: Lit<I>,
}

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    XmlStart(XmlStart<I>),
    XmlEnd(XmlEnd<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: LangInput,
{
    type Error = LangError;
    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        if let (Some(expr), input) = ExprLit::into_parser()
            .map(|v| Self::Lit(v))
            .or(XmlStart::into_parser().map(|v| Self::XmlStart(v)))
            .or(XmlEnd::into_parser().map(|v| Self::XmlEnd(v)))
            .ok()
            .parse(input.clone())?
        {
            return Ok((expr, input));
        }

        todo!()
    }
}
