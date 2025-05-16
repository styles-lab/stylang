use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Digits, Dot, Ident},
};

use super::{Expr, parse::PartialParse};

/// A struct or tuple struct field accessed in a struct literal or field expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Member<I>
where
    I: LangInput,
{
    Named(Ident<I>),
    Unnamed(Digits<I>),
}

/// Access of a named struct field (obj.k) or unnamed tuple struct field (obj.0).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprField<I>
where
    I: LangInput,
{
    /// the target expr.
    pub target: Box<Expr<I>>,
    /// Optional meta list.
    pub meta_list: MetaList<I>,
    /// dot token `.`
    pub dot_token: Dot<I>,
    /// member expr.
    pub member: Member<I>,
}

impl<I> PartialParse<I> for ExprField<I>
where
    I: LangInput,
{
    type LeadingToken = Dot<I>;

    fn partial_parse(
        meta_list: MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, LangError> {
        let (member, input) = Member::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Member, input.span()))
            .fatal()
            .parse(input)?;

        Ok((
            Expr::Field(Self {
                meta_list,
                target: Box::new(parsed),
                dot_token: leading_token,
                member,
            }),
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        expr::{Expr, ExprField, ExprPath, Member},
        inputs::TokenStream,
        meta::MetaList,
        tokens::{Dot, Ident},
    };

    #[test]
    fn test_field() {
        assert_eq!(
            Expr::parse(TokenStream::from("hello.a")),
            Ok((
                Expr::Field(ExprField {
                    meta_list: MetaList(vec![]),
                    target: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from("hello")),
                        segments: vec![]
                    })),
                    dot_token: Dot(TokenStream::from((5, "."))),
                    member: Member::Named(Ident(TokenStream::from((6, "a"))))
                }),
                TokenStream::from((7, ""))
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("hello.")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Member,
                Span { offset: 6, len: 0 }
            )))
        );
    }
}
