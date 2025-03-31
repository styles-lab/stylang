use parserc::{Parse, Parser, satisfy, take_while};

use super::{ParseError, StylangInput};

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

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_').parse(input)?;

        let (_, input) = take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_').parse(input)?;

        Ok((Self(content.split_off(input.start() - start)), input))
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
