use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    tokens::{Digits, Dot, DotStart, Ident, S},
};

use super::Expr;

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

/// Right part for accessing struct filed.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(super) struct Field<I>
where
    I: LangInput,
{
    /// dot token `.`
    pub dot_token: Dot<I>,
    /// member expr.
    pub member: Member<I>,
}

impl<I> Parse<I> for Field<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (DotStart::Dot(dot_token), input) = DotStart::parse(input.clone())? else {
            return Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("."),
                input.span(),
            )));
        };

        let (_, input) = S::into_parser().ok().parse(input)?;
        let (member, input) = Member::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Member, input.span()))
            .fatal()
            .parse(input)?;

        Ok((Self { dot_token, member }, input))
    }
}

/// Access of a named struct field (obj.k) or unnamed tuple struct field (obj.0).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprField<I>
where
    I: LangInput,
{
    /// base expr
    pub base: Box<Expr<I>>,
    /// dot token `.`
    pub dot_token: Dot<I>,
    /// member expr.
    pub member: Member<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        expr::Expr,
        inputs::TokenStream,
    };

    #[test]
    fn test_field() {
        assert_eq!(
            Expr::parse(TokenStream::from("hello.")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Member,
                Span { offset: 6, len: 0 }
            )))
        );
    }
}
