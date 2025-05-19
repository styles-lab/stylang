use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput};

use super::{ExprBinary, ExprBlock, ExprChain, ExprIf, ExprLet, ExprUnary, ExprXml};

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Expr<I>
where
    I: LangInput,
{
    Let(ExprLet<I>),
    If(ExprIf<I>),
    Xml(ExprXml<I>),
    Block(ExprBlock<I>),
    Binary(ExprBinary<I>),
    Chain(ExprChain<I>),
    Unary(ExprUnary<I>),
}
