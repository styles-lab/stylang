use parserc::{Parse, derive_parse};

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{LeftBracket, RightBracket},
};

use super::Expr;

/// A square bracketed indexing expression: vector[2].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIndex<I>
where
    I: LangInput,
{
    /// Leading meta-data list.
    pub meta_list: MetaList<I>,
    /// The index target expr.
    pub target: Box<Expr<I>>,
    /// start bracket: `[`
    pub delimiter_start: LeftBracket<I>,
    /// The index expr.
    pub index: Box<Expr<I>>,
    /// end bracket: `]`
    pub delimiter_end: RightBracket<I>,
}
