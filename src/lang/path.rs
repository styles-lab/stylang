use parserc::derive_parse;

use super::{Ident, MetaList, ParseError, PathSep, S, StylangInput};

/// A path at which a named item is exported (e.g. std::collections::HashMap).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprPath<I>
where
    I: StylangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    pub first: Ident<I>,
    pub tails: Vec<(Option<S<I>>, PathSep<I>, Option<S<I>>, Ident<I>)>,
}
