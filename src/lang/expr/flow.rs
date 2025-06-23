use parserc::{
    lang::LangInput,
    parser::Parser,
    syntax::{PartialSyntax, Syntax},
};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    meta::MetaList,
    stmt::Block,
    token::{KeywordLoop, S},
};

/// Unary ops.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// leading unary op
    pub keyword: (KeywordLoop<I>, Option<S<I>>),
    /// loop body block.
    pub body: Block<I>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordLoop<I>)> for ExprLoop<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordLoop<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;

        let (s, input) = input.parse()?;
        let keyword = (keyword, s);
        let (body, input) = Block::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        Ok((
            Self {
                meta_list,
                keyword,
                body,
            },
            input,
        ))
    }
}
