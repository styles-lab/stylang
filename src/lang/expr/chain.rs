use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput};

use super::{Call, ExprLit, ExprPath, Field, Index};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum ChainInit<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum ChainSegment<I>
where
    I: LangInput,
{
    Call(Call<I>),
    Index(Index<I>),
    Field(Field<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprChain<I>
where
    I: LangInput,
{
    pub start: ChainInit<I>,
    pub segments: Vec<ChainSegment<I>>,
}
