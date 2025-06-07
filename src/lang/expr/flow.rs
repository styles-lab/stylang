use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    expr::{Expr, ExprBlock},
    input::LangInput,
    meta::MetaList,
    token::{KeywordElse, KeywordIf, KeywordLoop, KeywordWhile},
};

/// An if expression with an optional else block: if expr { ... } else { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `if`
    pub keyword: KeywordIf<I>,
    /// condition expr.
    #[fatal]
    pub cond: Box<Expr<I>>,
    /// then branch.
    #[fatal]
    pub then_branch: ExprBlock<I>,
    /// else branch.
    pub else_branch: Option<(KeywordElse<I>, Box<Expr<I>>)>,
}

/// Conditionless loop: loop { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub keyword: KeywordLoop<I>,
    /// A group of stmts as the body of the loop.
    #[fatal]
    pub body: ExprBlock<I>,
}

/// Condition loop: while $expr { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprWhile<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub keyword: KeywordWhile<I>,
    /// condition expr.
    #[fatal]
    pub cond: Box<Expr<I>>,
    /// A group of stmts as the body of the loop.
    #[fatal]
    pub body: ExprBlock<I>,
}
