use parserc::{
    lang::LangInput,
    parser::Parser,
    syntax::{PartialSyntax, Syntax},
};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    meta::MetaList,
    token::*,
};

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
    pub operand: Box<Expr<I>>,
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
        let (operand, input) = Expr::into_parser()
            .boxed()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

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

#[cfg(test)]
mod tests {
    use parserc::{
        errors::ControlFlow,
        lang::{Span, TokenStream},
        syntax::Syntax,
    };

    use super::*;

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::parse(TokenStream::from("-value")),
            Ok((
                Expr::Unary(ExprUnary {
                    meta_list: vec![],
                    op: (
                        UnOp::Neg(TokenMinus(TokenStream {
                            offset: 0,
                            value: "-"
                        })),
                        None
                    ),
                    operand: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 1,
                            value: "value"
                        })
                    ))
                }),
                TokenStream {
                    offset: 6,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn unary_error_detect() {
        assert_eq!(
            Expr::parse(TokenStream::from("!")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: SyntaxKind::RightOperand,
                span: Span::Some { start: 1, end: 1 },
                item: None
            }))
        );
    }
}
