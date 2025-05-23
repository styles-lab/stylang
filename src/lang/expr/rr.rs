use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput};

use super::{Call, Expr, ExprCall, ExprField, ExprIndex, ExprLit, ExprPath, Field, Index};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) enum RightRecursiveStart<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}

impl<I> From<RightRecursiveStart<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: RightRecursiveStart<I>) -> Self {
        match value {
            RightRecursiveStart::Lit(v) => Expr::Lit(v),
            RightRecursiveStart::Path(v) => Expr::Path(v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) enum RightRecursiveSegment<I>
where
    I: LangInput,
{
    Call(Call<I>),
    Index(Index<I>),
    Field(Field<I>),
}

impl<I> From<(Expr<I>, RightRecursiveSegment<I>)> for Expr<I>
where
    I: LangInput,
{
    fn from(value: (Expr<I>, RightRecursiveSegment<I>)) -> Self {
        match value.1 {
            RightRecursiveSegment::Call(v) => Expr::Call(ExprCall {
                target: Box::new(value.0),
                delimiter_start: v.delimiter_start,
                params: v.params,
                delimiter_end: v.delimiter_end,
            }),
            RightRecursiveSegment::Index(v) => Expr::Index(ExprIndex {
                base: Box::new(value.0),
                delimiter_start: v.delimiter_start,
                index: v.index,
                delimiter_end: v.delimiter_end,
            }),
            RightRecursiveSegment::Field(v) => Expr::Field(ExprField {
                base: Box::new(value.0),
                dot_token: v.dot_token,
                member: v.member,
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub(super) struct RightRecursive<I>
where
    I: LangInput,
{
    pub start: RightRecursiveStart<I>,
    pub segments: Vec<RightRecursiveSegment<I>>,
}

impl<I> From<RightRecursive<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: RightRecursive<I>) -> Self {
        let mut expr = value.start.into();

        if value.segments.is_empty() {
            return expr;
        }

        for seg in value.segments {
            expr = (expr, seg).into();
        }

        expr
    }
}
