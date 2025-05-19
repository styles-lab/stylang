use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    stmt::Stmts,
    tokens::{LeftBrace, RightBrace},
};

/// Code block with delimiter `{...}`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Block<I>
where
    I: LangInput,
{
    /// delimiter start token: `{`
    #[key_field]
    pub delimiter_start: LeftBrace<I>,
    /// optional stmts list.
    pub stmts: Stmts<I>,
    /// optional tail meta list.
    pub meta_list: MetaList<I>,
    /// delimiter end token: `}`
    pub delimiter_end: RightBrace<I>,
}

/// Code block with delimiter `{...}`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprBlock<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// block .
    pub block: Block<I>,
}
