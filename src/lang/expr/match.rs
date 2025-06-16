use parserc::{inputs::lang::LangInput, syntax::Syntax};

use crate::lang::{errors::LangError, expr::Expr, meta::MetaList, token::KeywordMatch};

// A `match` pattern expr.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
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
