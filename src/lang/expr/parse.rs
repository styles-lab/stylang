use crate::lang::{errors::LangError, inputs::LangInput, meta::MetaList};

use super::Expr;

pub(super) trait PartialParse<I>: Sized
where
    I: LangInput,
{
    type LeadingToken;
    fn partial_parse(
        meta_list: MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, LangError>;
}
