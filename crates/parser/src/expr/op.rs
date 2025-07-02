use parserc::{
    lang::LangInput,
    parser::Parser,
    syntax::{PartialSyntax, Syntax},
};

use crate::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    meta::MetaList,
    token::*,
};

/// Unary ops: `!` or `-`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
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
#[syntax(error = LangError)]
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

/// Ops: `&=`,`=`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum AssignOp<I>
where
    I: LangInput,
{
    /// `&=`
    AndAssign((Option<S<I>>, TokenPlusEq<I>, Option<S<I>>)),
    /// `-=`
    SubAssign((Option<S<I>>, TokenMinusEq<I>, Option<S<I>>)),
    /// `*=`
    MulAssign((Option<S<I>>, TokenStarEq<I>, Option<S<I>>)),
    /// `/=`
    DivAssign((Option<S<I>>, TokenSlashEq<I>, Option<S<I>>)),
    /// `%=`
    RemAssign((Option<S<I>>, TokenPercentEq<I>, Option<S<I>>)),
    /// `^=`
    BitXorAssign((Option<S<I>>, TokenCaretEq<I>, Option<S<I>>)),
    /// `&=`
    BitAndAssign((Option<S<I>>, TokenAndEq<I>, Option<S<I>>)),
    /// `|=`
    BitOrAssign((Option<S<I>>, TokenSlashEq<I>, Option<S<I>>)),
    /// `<<=`
    ShlAssign((Option<S<I>>, TokenShlEq<I>, Option<S<I>>)),
    /// `>>=`
    ShrAssign((Option<S<I>>, TokenShrEq<I>, Option<S<I>>)),
    /// `=`
    Assign((Option<S<I>>, TokenEq<I>, Option<S<I>>)),
}

/// assign expression.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprAssign<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: AssignOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

/// Ops: `||`,`&&`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum BoolOp<I>
where
    I: LangInput,
{
    /// `&&`
    And((Option<S<I>>, TokenAndAnd<I>, Option<S<I>>)),
    /// `||`
    Or((Option<S<I>>, TokenOrOr<I>, Option<S<I>>)),
}

/// bool expression.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprBool<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: BoolOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

/// compare ops: `==`,`<`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum CompOp<I>
where
    I: LangInput,
{
    /// `==`
    Eq(Option<S<I>>, TokenEqEq<I>, Option<S<I>>),
    /// <=
    Le(Option<S<I>>, TokenLe<I>, Option<S<I>>),
    /// >=
    Ge(Option<S<I>>, TokenGe<I>, Option<S<I>>),
    /// !=
    Ne(Option<S<I>>, TokenNe<I>, Option<S<I>>),
    /// <
    Lt(Option<S<I>>, TokenLt<I>, Option<S<I>>),
    /// >
    Gt(Option<S<I>>, TokenGt<I>, Option<S<I>>),
}

/// compare expression.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprComp<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: CompOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

/// Ops: `^`,`<<`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum BitsOp<I>
where
    I: LangInput,
{
    /// `^`
    BitXor(Option<S<I>>, TokenCaret<I>, Option<S<I>>),
    /// `&`
    BitAnd(Option<S<I>>, TokenAnd<I>, Option<S<I>>),
    /// `|`
    BitOr(Option<S<I>>, TokenOr<I>, Option<S<I>>),
    /// `<<`
    Shl(Option<S<I>>, TokenShl<I>, Option<S<I>>),
    /// `>>`
    Shr(Option<S<I>>, TokenShr<I>, Option<S<I>>),
}

/// expr `a ^ b`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprBits<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: BitsOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

/// ops: `+`,`-`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum TermOp<I>
where
    I: LangInput,
{
    /// `+`
    Add(Option<S<I>>, TokenPlus<I>, Option<S<I>>),
    /// `-`
    Sub(Option<S<I>>, TokenMinus<I>, Option<S<I>>),
}

/// expr `a + b`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprTerm<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: TermOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

/// ops: `*`,`/`,`%`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum FactorOp<I>
where
    I: LangInput,
{
    /// `*`
    Mul(Option<S<I>>, TokenStar<I>, Option<S<I>>),
    /// `/`
    Div(Option<S<I>>, TokenSlash<I>, Option<S<I>>),
    /// `%`
    Rem(Option<S<I>>, TokenPercent<I>, Option<S<I>>),
}

/// expr `a * b`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprFactor<I>
where
    I: LangInput,
{
    /// left expand.
    pub left: Box<Expr<I>>,
    /// op.
    pub op: FactorOp<I>,
    /// right expand.
    pub right: Box<Expr<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::{
        errors::ControlFlow,
        lang::{Span, TokenStream},
        syntax::Syntax,
    };

    use crate::lit::{Lit, LitNum};

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

    #[test]
    fn binary_errors() {
        assert_eq!(
            Expr::parse(TokenStream::from("a - ")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: SyntaxKind::RightOperand,
                span: Span::Some { start: 4, end: 4 },
                item: None
            }))
        );
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            Expr::parse(TokenStream::from("a + b * c / 3 + 4")),
            Ok((
                Expr::Term(ExprTerm {
                    left: Box::new(Expr::Term(ExprTerm {
                        left: Box::new(Expr::Ident(
                            vec![],
                            Ident(TokenStream {
                                offset: 0,
                                value: "a"
                            })
                        )),
                        op: TermOp::Add(
                            Some(S(TokenStream {
                                offset: 1,
                                value: " "
                            })),
                            TokenPlus(TokenStream {
                                offset: 2,
                                value: "+"
                            }),
                            Some(S(TokenStream {
                                offset: 3,
                                value: " "
                            }))
                        ),
                        right: Box::new(Expr::Factor(ExprFactor {
                            left: Box::new(Expr::Factor(ExprFactor {
                                left: Box::new(Expr::Ident(
                                    vec![],
                                    Ident(TokenStream {
                                        offset: 4,
                                        value: "b"
                                    })
                                )),
                                op: FactorOp::Mul(
                                    Some(S(TokenStream {
                                        offset: 5,
                                        value: " "
                                    })),
                                    TokenStar(TokenStream {
                                        offset: 6,
                                        value: "*"
                                    }),
                                    Some(S(TokenStream {
                                        offset: 7,
                                        value: " "
                                    }))
                                ),
                                right: Box::new(Expr::Ident(
                                    vec![],
                                    Ident(TokenStream {
                                        offset: 8,
                                        value: "c"
                                    })
                                ))
                            })),
                            op: FactorOp::Div(
                                Some(S(TokenStream {
                                    offset: 9,
                                    value: " "
                                })),
                                TokenSlash(TokenStream {
                                    offset: 10,
                                    value: "/"
                                }),
                                Some(S(TokenStream {
                                    offset: 11,
                                    value: " "
                                }))
                            ),
                            right: Box::new(Expr::Lit(
                                vec![],
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 12,
                                        value: "3"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            ))
                        }))
                    })),
                    op: TermOp::Add(
                        Some(S(TokenStream {
                            offset: 13,
                            value: " "
                        })),
                        TokenPlus(TokenStream {
                            offset: 14,
                            value: "+"
                        }),
                        Some(S(TokenStream {
                            offset: 15,
                            value: " "
                        }))
                    ),
                    right: Box::new(Expr::Lit(
                        vec![],
                        Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 16,
                                value: "4"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ))
                }),
                TokenStream {
                    offset: 17,
                    value: ""
                }
            ))
        );
    }
}
