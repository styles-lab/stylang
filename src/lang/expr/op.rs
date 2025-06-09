use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    expr::{Expr, ExprPath, XmlStart},
    input::LangInput,
    meta::MetaList,
    token::*,
};

/// Ops: `&=`,`=`,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum AssignOp<I>
where
    I: LangInput,
{
    /// `&=`
    AndAssign(SepPlusEq<I>),
    /// `-=`
    SubAssign(SepMinusEq<I>),
    /// `*=`
    MulAssign(SepStarEq<I>),
    /// `/=`
    DivAssign(TokenSlashEq<I>),
    /// `%=`
    RemAssign(SepPercentEq<I>),
    /// `^=`
    BitXorAssign(SepCaretEq<I>),
    /// `&=`
    BitAndAssign(SepAndEq<I>),
    /// `|=`
    BitOrAssign(SepSlashEq<I>),
    /// `<<=`
    ShlAssign(SepShlEq<I>),
    /// `>>=`
    ShrAssign(SepShrEq<I>),
    /// `=`
    Assign(SepEq<I>),
}

/// A bool expr: a && b, a || b,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprAssgin<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: AssignOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum AssignOperand<I>
where
    I: LangInput,
{
    Bool(ExprBool<I>),
    Comp(ExprComp<I>),
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<AssignOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: AssignOperand<I>) -> Self {
        match value {
            AssignOperand::Unary(expr) => Self::Unary(expr),
            AssignOperand::Path(expr) => expr.into(),
            AssignOperand::Factor(expr) => Self::Factor(expr),
            AssignOperand::Term(expr) => Self::Term(expr),
            AssignOperand::Bits(expr) => Self::Bits(expr),
            AssignOperand::Comp(expr) => Self::Comp(expr),
            AssignOperand::Bool(expr) => Self::Bool(expr),
        }
    }
}

impl<I> Parse<I> for ExprAssgin<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = AssignOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, input) = AssignOp::parse(input)?;
        let (right_operand, input) = AssignOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

/// Ops: `||`,`&&`,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum BoolOp<I>
where
    I: LangInput,
{
    /// `&&`
    And(SepAndAnd<I>),
    /// `||`
    Or(SepOrOr<I>),
}

/// A bool expr: a && b, a || b,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBool<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: BoolOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum BoolOperand<I>
where
    I: LangInput,
{
    Comp(ExprComp<I>),
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<BoolOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: BoolOperand<I>) -> Self {
        match value {
            BoolOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            BoolOperand::Path(expr_path) => expr_path.into(),
            BoolOperand::Factor(expr_factor) => Self::Factor(expr_factor),
            BoolOperand::Term(expr_term) => Self::Term(expr_term),
            BoolOperand::Bits(expr_term) => Self::Bits(expr_term),
            BoolOperand::Comp(expr_term) => Self::Comp(expr_term),
        }
    }
}

impl<I> Parse<I> for ExprBool<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = BitsOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, input) = BoolOp::parse(input)?;
        let (right_operand, input) = BitsOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

/// compare ops: `==`,`<`,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum CompOp<I>
where
    I: LangInput,
{
    /// `==`
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

/// A comp expr: a > b, a != b, ...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprComp<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: CompOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum CompOperand<I>
where
    I: LangInput,
{
    Bits(ExprBits<I>),
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<CompOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: CompOperand<I>) -> Self {
        match value {
            CompOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            CompOperand::Path(expr_path) => expr_path.into(),
            CompOperand::Factor(expr_factor) => Self::Factor(expr_factor),
            CompOperand::Term(expr_term) => Self::Term(expr_term),
            CompOperand::Bits(expr_term) => Self::Bits(expr_term),
        }
    }
}

impl<I> Parse<I> for ExprComp<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = CompOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, op_input) = CompOp::parse(input.clone())?;

        if let CompOp::Lt(_) = &op {
            let (start, _) = XmlStart::into_parser().ok().parse(input.clone())?;
            if start.is_some() {
                return Err(ControlFlow::Recovable(LangError::expect(
                    TokenKind::RightOperand,
                    input.span(),
                )));
            }
        }

        let input = op_input;

        let (right_operand, input) = CompOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

/// Ops: `^`,`<<`,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum BitsOp<I>
where
    I: LangInput,
{
    /// `^`
    BitXor(SepCaret<I>),
    /// `&`
    BitAnd(SepAnd<I>),
    /// `|`
    BitOr(SepOr<I>),
    /// `<<`
    Shl(SepShl<I>),
    /// `>>`
    Shr(SepShr<I>),
}

/// A bits expr: a ^ b, a & b,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprBits<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: BitsOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum BitsOperand<I>
where
    I: LangInput,
{
    Term(ExprTerm<I>),
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<BitsOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: BitsOperand<I>) -> Self {
        match value {
            BitsOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            BitsOperand::Path(expr_path) => expr_path.into(),
            BitsOperand::Factor(expr_factor) => Self::Factor(expr_factor),
            BitsOperand::Term(expr_term) => Self::Term(expr_term),
        }
    }
}

impl<I> Parse<I> for ExprBits<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = BitsOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, input) = BitsOp::parse(input)?;
        let (right_operand, input) = BitsOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

/// ops: `+`,`-`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum TermOp<I>
where
    I: LangInput,
{
    /// `+`
    Add(SepPlus<I>),
    /// `-`
    Sub(SepMinus<I>),
}

/// A factor expr: a + b, a - b,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprTerm<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: TermOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum TermOperand<I>
where
    I: LangInput,
{
    Factor(ExprFactor<I>),
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<TermOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: TermOperand<I>) -> Self {
        match value {
            TermOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            TermOperand::Path(expr_path) => expr_path.into(),
            TermOperand::Factor(expr_factor) => Self::Factor(expr_factor),
        }
    }
}

impl<I> Parse<I> for ExprTerm<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = TermOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, input) = TermOp::parse(input)?;
        let (right_operand, input) = TermOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

/// ops: `*`,`/`,`%`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum FactorOp<I>
where
    I: LangInput,
{
    /// `*`
    Mul(SepStar<I>),
    /// `/`
    Div(SepSlash<I>),
    /// `%`
    Rem(SepPercent<I>),
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum FactorOperand<I>
where
    I: LangInput,
{
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<FactorOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: FactorOperand<I>) -> Self {
        match value {
            FactorOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            FactorOperand::Path(expr_path) => expr_path.into(),
        }
    }
}

/// A factor expr: a * b, a / b,...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprFactor<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// left operand.
    pub left_operand: Box<Expr<I>>,
    /// unnary op.
    pub op: FactorOp<I>,
    /// right operand.
    pub right_operand: Box<Expr<I>>,
}

impl<I> Parse<I> for ExprFactor<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left_operand, input) = FactorOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (op, input) = FactorOp::parse(input)?;
        let (right_operand, input) = FactorOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left_operand,
                op,
                right_operand,
            },
            input,
        ))
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum UnOperand<I>
where
    I: LangInput,
{
    Unary(ExprUnary<I>),
    Path(ExprPath<I>),
}

impl<I> From<UnOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: UnOperand<I>) -> Self {
        match value {
            UnOperand::Unary(expr_unary) => Self::Unary(expr_unary),
            UnOperand::Path(expr_path) => expr_path.into(),
        }
    }
}

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
        let (oprand, input) = UnOperand::into_parser()
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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::Expr,
        input::TokenStream,
        lit::{Lit, LitNum},
    };

    use super::*;

    #[test]
    fn priority() {
        assert_eq!(
            Expr::parse(TokenStream::from("a ^= b > c ^ 1 + d*3")),
            Ok((
                Expr::Assgin(ExprAssgin {
                    meta_list: vec![],
                    left_operand: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 0,
                            value: "a"
                        })
                    )),
                    op: AssignOp::BitXorAssign(SepCaretEq(
                        Some(S(TokenStream {
                            offset: 1,
                            value: " "
                        })),
                        TokenStream {
                            offset: 2,
                            value: "^="
                        },
                        Some(S(TokenStream {
                            offset: 4,
                            value: " "
                        }))
                    )),
                    right_operand: Box::new(Expr::Comp(ExprComp {
                        meta_list: vec![],
                        left_operand: Box::new(Expr::Ident(
                            vec![],
                            Ident(TokenStream {
                                offset: 5,
                                value: "b"
                            })
                        )),
                        op: CompOp::Gt(SepGt(
                            Some(S(TokenStream {
                                offset: 6,
                                value: " "
                            })),
                            TokenStream {
                                offset: 7,
                                value: ">"
                            },
                            Some(S(TokenStream {
                                offset: 8,
                                value: " "
                            }))
                        )),
                        right_operand: Box::new(Expr::Bits(ExprBits {
                            meta_list: vec![],
                            left_operand: Box::new(Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 9,
                                    value: "c"
                                })
                            )),
                            op: BitsOp::BitXor(SepCaret(
                                Some(S(TokenStream {
                                    offset: 10,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 11,
                                    value: "^"
                                },
                                Some(S(TokenStream {
                                    offset: 12,
                                    value: " "
                                }))
                            )),
                            right_operand: Box::new(Expr::Term(ExprTerm {
                                meta_list: vec![],
                                left_operand: Box::new(Expr::Lit(
                                    vec![],
                                    Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream {
                                            offset: 13,
                                            value: "1"
                                        })),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: None
                                    })
                                )),
                                op: TermOp::Add(SepPlus(
                                    Some(S(TokenStream {
                                        offset: 14,
                                        value: " "
                                    })),
                                    TokenStream {
                                        offset: 15,
                                        value: "+"
                                    },
                                    Some(S(TokenStream {
                                        offset: 16,
                                        value: " "
                                    }))
                                )),
                                right_operand: Box::new(Expr::Factor(ExprFactor {
                                    meta_list: vec![],
                                    left_operand: Box::new(Expr::Ident(
                                        vec![],
                                        Ident(TokenStream {
                                            offset: 17,
                                            value: "d"
                                        })
                                    )),
                                    op: FactorOp::Mul(SepStar(
                                        None,
                                        TokenStream {
                                            offset: 18,
                                            value: "*"
                                        },
                                        None
                                    )),
                                    right_operand: Box::new(Expr::Lit(
                                        vec![],
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 19,
                                                value: "3"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ))
                                }))
                            }))
                        }))
                    }))
                }),
                TokenStream {
                    offset: 20,
                    value: ""
                }
            ))
        );
    }
}
