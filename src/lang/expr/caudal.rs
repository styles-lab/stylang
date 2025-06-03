use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput};

use super::{Call, Expr, ExprCall, ExprField, ExprIndex, ExprLit, ExprPath, Field, Index};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum CaudalStart<I>
where
    I: LangInput,
{
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}

impl<I> From<CaudalStart<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: CaudalStart<I>) -> Self {
        match value {
            CaudalStart::Lit(v) => Expr::Lit(v),
            CaudalStart::Path(v) => Expr::Path(v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum CaudalSegment<I>
where
    I: LangInput,
{
    Call(Call<I>),
    Index(Index<I>),
    Field(Field<I>),
}

impl<I> From<(Expr<I>, CaudalSegment<I>)> for Expr<I>
where
    I: LangInput,
{
    fn from(value: (Expr<I>, CaudalSegment<I>)) -> Self {
        match value.1 {
            CaudalSegment::Call(v) => Expr::Call(ExprCall {
                target: Box::new(value.0),
                delimiter_start: v.delimiter_start,
                params: v.params,
                delimiter_end: v.delimiter_end,
            }),
            CaudalSegment::Index(v) => Expr::Index(ExprIndex {
                base: Box::new(value.0),
                delimiter_start: v.delimiter_start,
                index: v.index,
                delimiter_end: v.delimiter_end,
            }),
            CaudalSegment::Field(v) => Expr::Field(ExprField {
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
pub struct CaudalRecursion<I>
where
    I: LangInput,
{
    pub start: CaudalStart<I>,
    pub segments: Vec<CaudalSegment<I>>,
}

impl<I> From<CaudalRecursion<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: CaudalRecursion<I>) -> Self {
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
