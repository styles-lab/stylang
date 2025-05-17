use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::*,
};

use super::{Expr, parse::PartialParse};

/// A unary operation: !x, *x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UnOp<I>
where
    I: LangInput,
{
    /// !x
    Not(Not<I>),
    /// -y
    Neg(Minus<I>),
}

/// A binary operator: +,/,&.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum BinOp<I>
where
    I: LangInput,
{
    AndAssign(PlusEq<I>),
    SubAssign(MinusEq<I>),
    MulAssign(StarEq<I>),
    DivAssign(SlashEq<I>),
    RemAssign(PercentEq<I>),
    BitXorAssign(CaretEq<I>),
    BitAndAssign(AndEq<I>),
    BitOrAssign(SlashEq<I>),
    ShlAssign(ShlEq<I>),
    ShrAssign(ShrEq<I>),
    Add(Plus<I>),
    Sub(Minus<I>),
    Mul(Star<I>),
    Div(Slash<I>),
    Rem(Percent<I>),
    And(AndAnd<I>),
    Or(OrOr<I>),
    BitXor(Caret<I>),
    BitAnd(And<I>),
    BitOr(Or<I>),
    Shl(Shl<I>),
    Shr(Shr<I>),
    Eq(EqEq<I>),
    /// <=
    Le(Le<I>),
    /// >=
    Ge(Ge<I>),
    /// !=
    Ne(Ne<I>),
    Lt(Lt<I>),
    /// >
    Gt(Gt<I>),
}

/// A binary operation: a + b, a += b.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBinary<I>
where
    I: LangInput,
{
    /// left operand
    pub left: Box<Expr<I>>,
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// operator
    pub op: BinOp<I>,
    /// right operand
    pub right: Box<Expr<I>>,
}

impl<I> PartialParse<I> for ExprBinary<I>
where
    I: LangInput,
{
    type LeadingToken = BinOp<I>;

    fn partial_parse(
        meta_list: MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, LangError> {
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (right, input) = Expr::into_parser()
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Expr::Binary(Self {
                left: Box::new(parsed),
                meta_list,
                op: leading_token,
                right,
            }),
            input,
        ))
    }
}

/// A unary operation: !x, -x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprUnary<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// operator
    pub op: UnOp<I>,
    /// right operand
    pub operand: Box<Expr<I>>,
}

impl<I> Parse<I> for ExprUnary<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (op, input) = UnOp::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (operand, input) = Expr::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

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
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        expr::{BinOp, Expr, ExprBinary, ExprLit, ExprPath, ExprUnary, UnOp},
        inputs::TokenStream,
        lit::{Lit, LitBool, LitNum},
        meta::MetaList,
        tokens::{Digits, Ident, Minus, Ne, Not, Plus, True},
    };

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::parse(TokenStream::from("!true")),
            Ok((
                Expr::Unary(ExprUnary {
                    meta_list: MetaList(vec![]),
                    op: UnOp::Not(Not(TokenStream::from("!"))),
                    operand: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Bool(LitBool::True(True(TokenStream::from((1, "true")))))
                    }))
                }),
                TokenStream::from((5, ""))
            ))
        );

        assert_eq!(
            ExprUnary::parse(TokenStream::from("!")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::RightOperand,
                Span { offset: 1, len: 0 }
            )))
        );
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            Expr::parse(TokenStream::from("1 + 2 + 3")),
            Ok((
                Expr::Binary(ExprBinary {
                    meta_list: MetaList(vec![]),
                    left: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((0, "1")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    })),
                    op: BinOp::Add(Plus(TokenStream::from((2, "+")))),
                    right: Box::new(Expr::Binary(ExprBinary {
                        meta_list: MetaList(vec![]),
                        left: Box::new(Expr::Lit(ExprLit {
                            meta_list: MetaList(vec![]),
                            lit: Lit::Num(LitNum {
                                sign: None,
                                trunc: Some(Digits(TokenStream::from((4, "2")))),
                                dot: None,
                                fract: None,
                                exp: None,
                                unit: None
                            })
                        })),
                        op: BinOp::Add(Plus(TokenStream::from((6, "+")))),
                        right: Box::new(Expr::Lit(ExprLit {
                            meta_list: MetaList(vec![]),
                            lit: Lit::Num(LitNum {
                                sign: None,
                                trunc: Some(Digits(TokenStream::from((8, "3")))),
                                dot: None,
                                fract: None,
                                exp: None,
                                unit: None
                            })
                        })),
                    })),
                }),
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("hello+")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::RightOperand,
                Span { offset: 6, len: 0 }
            )))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("1 != -a")),
            Ok((
                Expr::Binary(ExprBinary {
                    meta_list: MetaList(vec![]),
                    left: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((0, "1")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    })),
                    op: BinOp::Ne(Ne(TokenStream::from((2, "!=")))),
                    right: Box::new(Expr::Unary(ExprUnary {
                        meta_list: MetaList(vec![]),
                        op: UnOp::Neg(Minus(TokenStream::from((5, "-")))),
                        operand: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((6, "a"))),
                            segments: vec![]
                        }))
                    }))
                }),
                TokenStream::from((7, ""))
            ))
        );
    }
}
