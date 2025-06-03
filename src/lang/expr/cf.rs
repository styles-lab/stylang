use parserc::derive_parse;

use crate::lang::{
    errors::LangError, expr::Expr, inputs::LangInput, meta::MetaList, tokens::KeywordReturn,
};

/// A control flow expression `return`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprReturn<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword: `return`
    pub return_token: KeywordReturn<I>,
    /// optional return value expr.
    pub expr: Option<Box<Expr<I>>>,
}
