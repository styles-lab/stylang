use parserc::{ControlFlow, Parse, Parser, ParserExt, PartialParse, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::*,
};

use super::{Expr, caudal::CaudalRecursion};

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
    /// +=
    AndAssign(PlusEq<I>),
    /// -=
    SubAssign(MinusEq<I>),
    /// *=
    MulAssign(StarEq<I>),
    /// /=
    DivAssign(SlashEq<I>),
    /// %=
    RemAssign(PercentEq<I>),
    /// ^=
    BitXorAssign(CaretEq<I>),
    /// &=
    BitAndAssign(AndEq<I>),
    /// |=
    BitOrAssign(SlashEq<I>),
    /// <<=
    ShlAssign(ShlEq<I>),
    /// >>=
    ShrAssign(ShrEq<I>),
    /// +
    Add(Plus<I>),
    /// -
    Sub(Minus<I>),
    /// *
    Mul(Star<I>),
    /// `/`
    Div(Slash<I>),
    /// `%`
    Rem(Percent<I>),
    /// `&&`
    And(AndAnd<I>),
    /// `||`
    Or(OrOr<I>),
    /// `^`
    BitXor(Caret<I>),
    /// `&`
    BitAnd(And<I>),
    /// `|`
    BitOr(Or<I>),
    /// `<<`
    Shl(Shl<I>),
    /// `>>`
    Shr(Shr<I>),
    /// `==`
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
    /// operator
    pub op: BinOp<I>,
    /// right operand.
    pub right: Box<Expr<I>>,
}

impl<I> PartialParse<I> for ExprBinary<I>
where
    I: LangInput,
{
    type Error = LangError;
    type Parsed = CaudalRecursion<I>;

    fn parse(left: CaudalRecursion<I>, mut input: I) -> parserc::Result<Self, I, LangError> {
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
            (right, input) = CaudalRecursion::into_parser().ok().parse(input)?;
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

        let mut left: Expr<_> = left.into();
        let (op, right) = right_chain.pop().unwrap();

        for (op, seg) in right_chain {
            left = Expr::Binary(ExprBinary {
                left: Box::new(left),
                op,
                right: Box::new(seg.into()),
            });
        }

        Ok((
            Self {
                left: Box::new(left),
                op,
                right: Box::new(right.into()),
            },
            input,
        ))
    }
}

impl<I> Parse<I> for ExprBinary<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (left, input) = CaudalRecursion::parse(input)?;
        Self::into_parser_with(left).parse(input)
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
            .map(|v| v)
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
            BinOp, Expr, ExprBinary, ExprCall, ExprField, ExprLit, ExprPath, ExprUnary, Member,
            UnOp,
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
                    meta_list: MetaList::default(),
                    op: UnOp::Not(Not(TokenStream::from("!"))),
                    operand: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList::default(),
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
            Expr::parse(TokenStream::from("a + b.c() + d")),
            Ok((
                Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList::default(),
                            first: Ident(TokenStream::from("a")),
                            segments: vec![]
                        })),
                        op: BinOp::Add(Plus(TokenStream::from((2, "+")))),
                        right: Box::new(Expr::Call(ExprCall {
                            target: Box::new(Expr::Field(ExprField {
                                base: Box::new(Expr::Path(ExprPath {
                                    meta_list: MetaList::default(),
                                    first: Ident(TokenStream::from((4, "b"))),
                                    segments: vec![]
                                })),
                                dot_token: Dot(TokenStream::from((5, "."))),
                                member: Member::Named(Ident(TokenStream::from((6, "c"))))
                            })),
                            delimiter_start: LeftParen(TokenStream::from((7, "("))),
                            params: Punctuated {
                                items: vec![],
                                last: None
                            },
                            delimiter_end: RightParen(TokenStream::from((8, ")")))
                        })),
                    })),
                    op: BinOp::Add(Plus(TokenStream::from((10, "+")))),
                    right: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList::default(),
                        first: Ident(TokenStream::from((12, "d"))),
                        segments: vec![]
                    })),
                }),
                TokenStream::from((13, ""))
            ))
        );
    }
}
