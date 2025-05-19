use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::*,
};

use super::Expr;

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
        expr::{ChainInit, Expr, ExprChain, ExprLit, ExprUnary, UnOp},
        inputs::TokenStream,
        lit::{Lit, LitBool},
        meta::MetaList,
        tokens::{Not, True},
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
}
