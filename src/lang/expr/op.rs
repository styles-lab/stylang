use parserc::{
    lang::LangInput,
    syntax::{PartialSyntax, Syntax},
};

use crate::lang::{errors::LangError, lit::Exp, meta::MetaList, token::*};

/// Unary ops: `!` or `-`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum UnOp<I>
where
    I: LangInput,
{
    Not(TokenNot<I>),
    Neg(TokenMinus<I>),
}

/// Unary ops.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprUnary<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// leading unary op
    pub op: (UnOp<I>, Option<S<I>>),
    /// The right-operand of unary expression.
    pub operand: Box<Exp<I>>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, UnOp<I>)> for ExprUnary<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, op): (MetaList<I>, UnOp<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;

        let (s, input) = input.parse()?;
        let op = (op, s);
        let (operand, input) = input.ensure_parse()?;

        Ok((
            Self {
                meta_list,
                op,
                operand,
            },
            input,
        ))
    }
}
