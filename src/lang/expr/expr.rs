use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput, lit::Lit, meta::MetaList};

use super::{ExprField, ExprPath, ExprRange, XmlEnd, XmlStart};

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
#[derive_parse(error = LangError,input = I)]
pub enum Expr<I>
where
    I: LangInput,
{
    Field(ExprField<I>),
    XmlStart(XmlStart<I>),
    XmlEnd(XmlEnd<I>),
    Range(ExprRange<I>),
    Path(ExprPath<I>),
    Lit(ExprLit<I>),
}
