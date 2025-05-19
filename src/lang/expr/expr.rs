use parserc::derive_parse;

use crate::lang::{errors::LangError, expr::ExprField, inputs::LangInput};

use super::{
    Call, ExprBlock, ExprCall, ExprIf, ExprIndex, ExprLet, ExprLit, ExprPath, ExprUnary, ExprXml,
    Field, Index,
};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum TargetStart<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum TargetSegment<I>
where
    I: LangInput,
{
    Call(Call<I>),
    Index(Index<I>),
    Field(Field<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Target<I>
where
    I: LangInput,
{
    pub start: TargetStart<I>,
    pub segments: Vec<TargetSegment<I>>,
}

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
    Field(ExprField<I>),
    Call(ExprCall<I>),
    Index(ExprIndex<I>),
    Xml(ExprXml<I>),
    Block(ExprBlock<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
    Unary(ExprUnary<I>),
}
