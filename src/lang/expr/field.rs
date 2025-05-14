use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse, span::Span};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Digits, Dot, DotStart, Ident, S},
};

use super::{Expr, ExprLit, ExprPath, ExprRange};

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

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum FieldTarget<I>
where
    I: LangInput,
{
    Range(ExprRange<I>),
    Path(ExprPath<I>),
    Lit(ExprLit<I>),
}

impl<I> From<FieldTarget<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: FieldTarget<I>) -> Self {
        match value {
            FieldTarget::Range(v) => Expr::Range(v),
            FieldTarget::Path(v) => Expr::Path(v),
            FieldTarget::Lit(v) => Expr::Lit(v),
        }
    }
}

/// Access of a named struct field (obj.k) or unnamed tuple struct field (obj.0).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprField<I>
where
    I: LangInput,
{
    /// Optional meta list.
    pub meta_list: MetaList<I>,
    /// the target expr.
    pub target: Box<Expr<I>>,
    /// dot token `.`
    pub dot_token: Dot<I>,
    /// member expr.
    pub member: Member<I>,
}

impl<I> TryFrom<(Expr<I>, Span)> for ExprField<I>
where
    I: LangInput,
{
    type Error = ControlFlow<LangError>;

    fn try_from(value: (Expr<I>, Span)) -> Result<Self, Self::Error> {
        match value.0 {
            Expr::Field(v) => Ok(v),
            _ => Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::ExprField,
                value.1,
            ))),
        }
    }
}

impl<I> Parse<I> for ExprField<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;

        let span = input.span();

        let (mut target, mut input) = FieldTarget::into_parser()
            .map(|v| Expr::from(v))
            .parse(input)?;

        loop {
            (_, input) = S::into_parser().ok().parse(input)?;
            let dot_token: Dot<I>;

            (dot_token, input) = match DotStart::into_parser().ok().parse(input)? {
                (Some(DotStart::Dot(dot)), input) => (dot, input),
                (_, input) => {
                    return Self::try_from((target, span)).map(|mut v| {
                        v.meta_list = meta_list;
                        (v, input)
                    });
                }
            };

            (_, input) = S::into_parser().ok().parse(input)?;

            let member;
            (member, input) = Member::into_parser()
                .map_err(|input: I, _| LangError::expect(TokenKind::Member, input.span()))
                .fatal()
                .parse(input)?;

            target = Expr::Field(ExprField {
                meta_list: MetaList(vec![]),
                target: Box::new(target),
                dot_token,
                member,
            });
        }
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
            ExprField::parse(TokenStream::from("hello.")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Member,
                Span { offset: 6, len: 0 }
            )))
        );
    }
}
