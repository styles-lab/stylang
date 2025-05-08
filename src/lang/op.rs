use parserc::derive_parse;

use super::{
    And, AndAnd, AndEq, Caret, CaretEq, EqEq, Expr, Ge, Gt, Le, Lt, MetaList, Minus, MinusEq, Ne,
    Not, Or, OrOr, ParseError, Percent, PercentEq, Plus, PlusEq, Shl, ShlEq, Shr, ShrEq, Slash,
    SlashEq, Star, StarEq, StylangInput,
};

/// A unary operation: !x, *x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum UnOp<I>
where
    I: StylangInput,
{
    Not(Not<I>),
    Neg(Minus<I>),
}

/// A binary operator: +,/,&.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum BinOp<I>
where
    I: StylangInput,
{
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
    Lt(Lt<I>),
    /// <=
    Le(Le<I>),
    /// !=
    Ne(Ne<I>),
    /// >
    Gt(Gt<I>),
    /// >=
    Ge(Ge<I>),
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
}

/// A binary operation: a + b, a += b.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprBinary<I>
where
    I: StylangInput,
{
    /// left operand
    pub left: Box<Expr<I>>,
    /// operator
    pub op: BinOp<I>,
    /// right operand
    pub right: Box<Expr<I>>,
}

/// A unary operation: !x, -x.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprUnary<I>
where
    I: StylangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// operator
    pub op: UnOp<I>,
    /// right operand
    pub right: Box<Expr<I>>,
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        BinOp, Digits, Expr, ExprLit, ExprPath, ExprUnary, Ident, Lit, LitNum, MetaList, Minus,
        Not, Plus, Star, TokenStream, UnOp,
    };

    use super::ExprBinary;

    #[test]
    fn test_binary() {
        assert_eq!(
            Expr::parse(TokenStream::from("a *\nb + b")),
            Ok((
                Expr::Binary(ExprBinary {
                    left: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from((0, "a"))),
                        tails: vec![]
                    })),
                    op: BinOp::Mul(Star(TokenStream::from((2, "*")))),
                    right: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((4, "b"))),
                            tails: vec![]
                        })),
                        op: BinOp::Add(Plus(TokenStream::from((6, "+")))),
                        right: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((8, "b"))),
                            tails: vec![]
                        }))
                    }))
                }),
                TokenStream::from((9, ""))
            ))
        );
    }

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::parse(TokenStream::from("- 1024")),
            Ok((
                Expr::Unary(ExprUnary {
                    meta_list: MetaList(vec![]),
                    op: UnOp::Neg(Minus(TokenStream::from((0, "-")))),
                    right: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((2, "1024")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    }))
                }),
                TokenStream::from((6, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("!a")),
            Ok((
                Expr::Unary(ExprUnary {
                    meta_list: MetaList(vec![]),
                    op: UnOp::Not(Not(TokenStream::from("!"))),
                    right: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from((1, "a"))),
                        tails: vec![]
                    }))
                }),
                TokenStream::from((2, ""))
            ))
        );
    }
}
