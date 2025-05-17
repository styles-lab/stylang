use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Eq, S},
};

use super::{Expr, parse::PartialParse};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprAssign<I>
where
    I: LangInput,
{
    /// Reft operand
    pub left: Box<Expr<I>>,
    /// Optional leading metadata list.
    pub meta_list: MetaList<I>,
    /// token `=`
    pub eq_token: Eq<I>,
    /// Right operand.
    pub right: Box<Expr<I>>,
}

impl<I> PartialParse<I> for ExprAssign<I>
where
    I: LangInput,
{
    type LeadingToken = Eq<I>;
    fn partial_parse(
        meta_list: MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, crate::lang::errors::LangError> {
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (right, input) = Expr::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RightOperand, input.span()))
            .fatal()
            .boxed()
            .parse(input)?;

        Ok((
            Expr::Assign(Self {
                left: Box::new(parsed),
                meta_list,
                eq_token: leading_token,
                right,
            }),
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

    #[test]
    fn test_assign_chain() {
        Expr::parse(TokenStream::from((1, "a = b = 3"))).unwrap();
    }
}
