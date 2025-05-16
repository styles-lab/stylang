use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Eq, EqStart, S},
};

use super::{Expr, ExprBinary, ExprField, ExprPath, ExprUnary};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum LeftOperand<I>
where
    I: LangInput,
{
    Field(ExprField<I>),
    Path(ExprPath<I>),
}

impl<I> From<LeftOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: LeftOperand<I>) -> Self {
        match value {
            LeftOperand::Field(v) => Self::Field(v),
            LeftOperand::Path(v) => Self::Path(v),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum RightOperand<I>
where
    I: LangInput,
{
    Field(ExprField<I>),
    Path(ExprPath<I>),
    Binary(ExprBinary<I>),
    Unary(ExprUnary<I>),
}

impl<I> From<RightOperand<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: RightOperand<I>) -> Self {
        match value {
            RightOperand::Field(v) => Self::Field(v),
            RightOperand::Path(v) => Self::Path(v),
            RightOperand::Binary(v) => Self::Binary(v),
            RightOperand::Unary(v) => Self::Unary(v),
        }
    }
}

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprAssign<I>
where
    I: LangInput,
{
    /// Optional leading metadata list.
    pub meta_list: MetaList<I>,
    /// Reft operand
    pub left: Box<Expr<I>>,
    /// token `=`
    pub eq_token: Eq<I>,
    /// Right operand.
    pub right: Box<Expr<I>>,
}

impl<I> Parse<I> for ExprAssign<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (left, input) = LeftOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (Some(EqStart::Eq(eq_token)), input) =
            EqStart::into_parser().ok().parse(input.clone())?
        else {
            return Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("="),
                input.span(),
            )));
        };
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (right, input) = RightOperand::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                left,
                eq_token,
                right,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod test {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprPath, assign::ExprAssign},
        inputs::TokenStream,
        meta::MetaList,
        tokens::{Eq, Ident},
    };

    #[test]
    fn test_assing() {
        assert_eq!(
            Expr::parse(TokenStream::from((1, "a = b"))),
            Ok((
                Expr::Assign(ExprAssign {
                    meta_list: MetaList(vec![]),
                    left: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from((1, "a"))),
                        segments: vec![]
                    })),
                    eq_token: Eq(TokenStream::from((3, "="))),
                    right: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from((5, "b"))),
                        segments: vec![]
                    }))
                }),
                TokenStream::from((6, ""))
            ))
        );
    }
}
