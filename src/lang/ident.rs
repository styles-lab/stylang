use parserc::{Kind, Parse, Parser, ParserExt, satisfy, take_while};

use super::{ParseError, StylangInput, Token};

/// Complex type name or legal variable name.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Ident<I>(pub I);

impl<I> Parse<I> for Ident<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Ident, input.span()))
            .parse(input)?;

        let (_, input) = take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

/// view element variable name.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct ElemIdent<I>(pub I);

impl<I> Parse<I> for ElemIdent<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_').parse(input)?;

        let (_, input) =
            take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'-').parse(input)?;

        Ok((Self(content.split_off(input.start() - start)), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::TokenStream;

    use super::Ident;

    #[test]
    fn test_ident() {
        assert_eq!(
            Ident::parse(TokenStream::from("option")),
            Ok((
                Ident(TokenStream::from("option")),
                TokenStream::from((6, ""))
            ))
        );
    }
}
