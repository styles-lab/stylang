use parserc::{
    lang::LangInput,
    syntax::{PartialSyntax, Syntax},
};

use crate::{
    errors::LangError,
    expr::Expr,
    meta::MetaList,
    patt::Patt,
    token::{KeywordLet, S, TokenEq},
};

/// expression `let xxx=xxx`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprLet<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `let`
    pub keyword: (KeywordLet<I>, S<I>),
    /// pattern meta-data list.
    pub patt_meta_list: MetaList<I>,
    /// assign to pattern.
    pub patt: Patt<I>,
    /// equal token `[S]*=[S]*`
    pub eq_token: (Option<S<I>>, TokenEq<I>, Option<S<I>>),
    /// assign expression.
    pub expr: Box<Expr<I>>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordLet<I>)> for ExprLet<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordLet<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;
        let (s, input) = input.ensure_parse()?;
        let keyword = (keyword, s);
        let (patt_meta_list, input) = input.ensure_parse()?;
        let (patt, input) = input.ensure_parse()?;
        let (eq_token, input) = input.ensure_parse()?;
        let (expr, input) = input.ensure_parse()?;

        Ok((
            Self {
                meta_list,
                keyword,
                patt_meta_list,
                patt,
                eq_token,
                expr,
            },
            input,
        ))
    }
}
