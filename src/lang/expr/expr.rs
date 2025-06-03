use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    expr::{ExprBlock, ExprOperand, ExprReturn},
    inputs::LangInput,
};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Expr<I>
where
    I: LangInput,
{
    /// return expr: `return` or `return ..`
    Return(ExprReturn<I>),
    /// block expr: `{...}`
    Block(ExprBlock<I>),
    /// path expr: a.b().c[1]
    Operand(ExprOperand<I>),
}
