use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    expr::{Expr, Operand},
    input::LangInput,
    meta::MetaList,
    token::*,
};

/// A unary operation: !x, *x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UnOp<I>
where
    I: LangInput,
{
    /// !x
    Not(SepNot<I>),
    /// -y
    Neg(SepMinus<I>),
}

/// A binary operator: +,/,&.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum BinOp<I>
where
    I: LangInput,
{
    AndAssign(SepPlusEq<I>),
    SubAssign(SepMinusEq<I>),
    MulAssign(SepStarEq<I>),
    DivAssign(TokenSlashEq<I>),
    RemAssign(SepPercentEq<I>),
    BitXorAssign(SepCaretEq<I>),
    BitAndAssign(SepAndEq<I>),
    BitOrAssign(SepSlashEq<I>),
    ShlAssign(SepShlEq<I>),
    ShrAssign(SepShrEq<I>),
    Add(SepPlus<I>),
    Sub(SepMinus<I>),
    Mul(SepStar<I>),
    Div(SepSlash<I>),
    Rem(SepPercent<I>),
    And(SepAndAnd<I>),
    Or(SepOrOr<I>),
    BitXor(SepCaret<I>),
    BitAnd(SepAnd<I>),
    BitOr(SepOr<I>),
    Shl(SepShl<I>),
    Shr(SepShr<I>),
    Eq(SepEqEq<I>),
    /// <=
    Le(SepLe<I>),
    /// >=
    Ge(SepGe<I>),
    /// !=
    Ne(SepNe<I>),
    /// <
    Lt(SepLt<I>),
    /// >
    Gt(SepGt<I>),
}

/// A binary operation: a + b, a += b.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprUnary<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// unnary op.
    pub op: UnOp<I>,
    /// right operand.
    pub operand: Box<Expr<I>>,
}

impl<I> Parse<I> for ExprUnary<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (op, input) = UnOp::parse(input)?;
        let (oprand, input) = Operand::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                op,
                operand: Box::new(oprand.into()),
            },
            input,
        ))
    }
}

/// A binary operation: a + b, a += b.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprBinary<I>
where
    I: LangInput,
{
    /// left operand
    pub start: Box<Expr<I>>,
    pub rest: Vec<(BinOp<I>, Expr<I>)>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprPath, PathStart},
        input::TokenStream,
        lit::{Lit, LitNum},
        ty::TypePath,
    };

    use super::*;

    #[test]
    fn op_unary_binary() {
        assert_eq!(
            Expr::parse(TokenStream::from("!true && 1 != 4")),
            Ok((
                Expr::Binary(ExprBinary {
                    start: Box::new(Expr::Unary(ExprUnary {
                        meta_list: vec![],
                        op: UnOp::Not(SepNot(
                            None,
                            TokenStream {
                                offset: 0,
                                value: "!"
                            },
                            None
                        )),
                        operand: Box::new(Expr::Path(ExprPath {
                            meta_list: vec![],
                            first: PathStart::TypePath(TypePath {
                                first: Ident(TokenStream {
                                    offset: 1,
                                    value: "true"
                                }),
                                rest: vec![]
                            }),
                            rest: vec![]
                        }))
                    })),
                    rest: vec![
                        (
                            BinOp::And(SepAndAnd(
                                Some(S(TokenStream {
                                    offset: 5,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 6,
                                    value: "&&"
                                },
                                Some(S(TokenStream {
                                    offset: 8,
                                    value: " "
                                }))
                            )),
                            Expr::Path(ExprPath {
                                meta_list: vec![],
                                first: PathStart::Lit(Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 9,
                                        value: "1"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })),
                                rest: vec![]
                            })
                        ),
                        (
                            BinOp::Ne(SepNe(
                                Some(S(TokenStream {
                                    offset: 10,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 11,
                                    value: "!="
                                },
                                Some(S(TokenStream {
                                    offset: 13,
                                    value: " "
                                }))
                            )),
                            Expr::Path(ExprPath {
                                meta_list: vec![],
                                first: PathStart::Lit(Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 14,
                                        value: "4"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })),
                                rest: vec![]
                            })
                        )
                    ]
                }),
                TokenStream {
                    offset: 15,
                    value: ""
                }
            ))
        );
    }
}
