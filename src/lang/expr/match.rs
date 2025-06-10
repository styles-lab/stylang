use parserc::derive_parse;

use crate::lang::{
    errors::LangError, expr::Expr, input::LangInput, meta::MetaList, token::KeywordMatch,
};

// A `match` pattern expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprMatch<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `match`.
    pub keyword: KeywordMatch<I>,
    /// target expr.
    pub expr: Expr<I>,
}
