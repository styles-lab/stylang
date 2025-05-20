use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::*,
};

use super::{Expr, ExprChain, partial::PartialParse};

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
    pub left: ExprChain<I>,
    /// operator
    pub right_chain: Vec<(BinOp<I>, ExprChain<I>)>,
}

impl<I> PartialParse<I> for ExprBinary<I>
where
    I: LangInput,
{
    fn partial_parse(left: ExprChain<I>, mut input: I) -> parserc::Result<Self, I, LangError> {
        let mut right_chain = vec![];

        loop {
            (_, input) = S::into_parser().ok().parse(input)?;
            let op;
            (op, input) = BinOp::into_parser().ok().parse(input)?;

            let Some(op) = op else {
                break;
            };

            (_, input) = S::into_parser().ok().parse(input)?;
            let right;
            (right, input) = ExprChain::into_parser().ok().parse(input)?;
            let Some(right) = right else {
                return Err(ControlFlow::Fatal(LangError::expect(
                    TokenKind::RightOperand,
                    input.span(),
                )));
            };

            right_chain.push((op, right));
        }

        if right_chain.is_empty() {
            return Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::ExprBinary,
                input.span(),
            )));
        }

        Ok((Self { left, right_chain }, input))
    }
}

impl<I> Parse<I> for ExprBinary<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (left, input) = ExprChain::parse(input)?;
        Self::partial_parse(left, input)
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
        expr::{
            BinOp, Call, ChainInit, ChainSegment, Expr, ExprBinary, ExprChain, ExprLit, ExprPath,
            ExprUnary, Field, Member, UnOp,
        },
        inputs::TokenStream,
        lit::{Lit, LitBool},
        meta::MetaList,
        punct::Punctuated,
        tokens::{Dot, Ident, LeftParen, Not, Plus, RightParen, True},
    };

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::parse(TokenStream::from("!true")),
            Ok((
                Expr::Unary(ExprUnary {
                    meta_list: MetaList(vec![]),
                    op: UnOp::Not(Not(TokenStream::from("!"))),
                    operand: Box::new(Expr::Chain(ExprChain {
                        start: ChainInit::Lit(ExprLit {
                            meta_list: MetaList(vec![]),
                            lit: Lit::Bool(LitBool::True(True(TokenStream::from((1, "true")))))
                        }),
                        segments: vec![]
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
            Expr::parse(TokenStream::from("a + b.c() + d")),
            Ok((
                Expr::Binary(ExprBinary {
                    left: ExprChain {
                        start: ChainInit::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from("a")),
                            segments: vec![]
                        }),
                        segments: vec![]
                    },
                    right_chain: vec![
                        (
                            BinOp::Add(Plus(TokenStream::from((2, "+")))),
                            ExprChain {
                                start: ChainInit::Path(ExprPath {
                                    meta_list: MetaList(vec![]),
                                    first: Ident(TokenStream::from((4, "b"))),
                                    segments: vec![]
                                }),
                                segments: vec![
                                    ChainSegment::Field(Field {
                                        dot_token: Dot(TokenStream::from((5, "."))),
                                        member: Member::Named(Ident(TokenStream::from((6, "c"))))
                                    }),
                                    ChainSegment::Call(Call {
                                        delimiter_start: LeftParen(TokenStream::from((7, "("))),
                                        params: Punctuated {
                                            items: vec![],
                                            last: None
                                        },
                                        delimiter_end: RightParen(TokenStream::from((8, ")")))
                                    })
                                ]
                            }
                        ),
                        (
                            BinOp::Add(Plus(TokenStream::from((10, "+")))),
                            ExprChain {
                                start: ChainInit::Path(ExprPath {
                                    meta_list: MetaList(vec![]),
                                    first: Ident(TokenStream::from((12, "d"))),
                                    segments: vec![]
                                }),
                                segments: vec![]
                            }
                        )
                    ]
                }),
                TokenStream::from((13, ""))
            ))
        );
    }
}
