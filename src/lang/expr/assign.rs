use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    tokens::{Eq, S},
};

use super::{
    Expr, ExprChain,
    partial::{Partial, PartialParse},
};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprAssign<I>
where
    I: LangInput,
{
    /// Reft operand
    pub left: ExprChain<I>,
    /// token `=`
    pub eq_token: Eq<I>,
    /// Right operand.
    pub right: Box<Expr<I>>,
}

impl<I> PartialParse<I> for ExprAssign<I>
where
    I: LangInput,
{
    fn partial_parse(
        left: ExprChain<I>,
        input: I,
    ) -> parserc::Result<Self, I, crate::lang::errors::LangError> {
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (eq_token, input) = Eq::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (chain, input) = ExprChain::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .parse(input)?;

        let (right, input) = Partial::from(chain.clone())
            .map(|v| Expr::Binary(v))
            .boxed()
            .ok()
            .parse(input)?;

        let right = if let Some(right) = right {
            right
        } else {
            Box::new(Expr::Chain(chain))
        };

        Ok((
            Self {
                left,
                eq_token,
                right,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        expr::{BinOp, ChainInit, Expr, ExprAssign, ExprBinary, ExprChain, ExprLit, ExprPath},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        meta::MetaList,
        tokens::{Digits, Eq, Ident, Plus, PlusEq},
    };

    #[test]
    fn test_assign() {
        assert_eq!(
            Expr::parse(TokenStream::from("a = 1 + b += c = 4")),
            Ok((
                Expr::Assign(ExprAssign {
                    left: ExprChain {
                        start: ChainInit::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from("a")),
                            segments: vec![]
                        }),
                        segments: vec![]
                    },
                    eq_token: Eq(TokenStream::from((2, "="))),
                    right: Box::new(Expr::Binary(ExprBinary {
                        left: ExprChain {
                            start: ChainInit::Lit(ExprLit {
                                meta_list: MetaList(vec![]),
                                lit: Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream::from((4, "1")))),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            }),
                            segments: vec![]
                        },
                        right_chain: vec![
                            (
                                BinOp::Add(Plus(TokenStream::from((6, "+")))),
                                ExprChain {
                                    start: ChainInit::Path(ExprPath {
                                        meta_list: MetaList(vec![]),
                                        first: Ident(TokenStream::from((8, "b"))),
                                        segments: vec![]
                                    }),
                                    segments: vec![]
                                }
                            ),
                            (
                                BinOp::AndAssign(PlusEq(TokenStream::from((10, "+=")))),
                                ExprChain {
                                    start: ChainInit::Path(ExprPath {
                                        meta_list: MetaList(vec![]),
                                        first: Ident(TokenStream::from((13, "c"))),
                                        segments: vec![]
                                    }),
                                    segments: vec![]
                                }
                            )
                        ]
                    }))
                }),
                TokenStream::from((15, "= 4"))
            ))
        );
    }
}
