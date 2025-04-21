use parserc::{Kind, Parse, Parser, ParserExt, satisfy, take_while};

use super::{ParseError, StylangInput, TokenError};

/// Complex type name or legal variable name.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
            .map_err(|input: I, _: Kind| ParseError::Expect(TokenError::Ident, input.span()))
            .parse(input)?;

        let (_, input) = take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

/// xml element ident.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlIdent<I>(pub I);

impl<I> Parse<I> for XmlIdent<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|input: I, _: Kind| ParseError::Expect(TokenError::XmlIdent, input.span()))
            .parse(input)?;

        let (_, input) =
            take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'.' || c == b'_' || c == b'-')
                .parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{TokenStream, XmlIdent};

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

    #[test]
    fn test_xml_ident() {
        assert_eq!(
            XmlIdent::parse(TokenStream::from("on-click.solidity")),
            Ok((
                XmlIdent(TokenStream::from("on-click.solidity")),
                TokenStream::from((17, ""))
            ))
        );
    }
}
