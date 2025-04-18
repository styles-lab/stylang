use parserc::{Kind, Parser, ParserExt, keyword};

use super::{ParseError, StylangInput, skip_ws};

/// A token surround by `start` and `end` tokens.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Delimiter<I> {
    /// prefix token.
    pub start: I,
    /// suffix token.
    pub end: I,
}

/// Match a sequence parsers: prefix + content + suffix.
pub fn delimited<I, P>(
    prefix: &'static str,
    mut parser: P,
    suffix: &'static str,
) -> impl Parser<I, Output = (Delimiter<I>, P::Output), Error = ParseError>
where
    I: StylangInput,
    P: Parser<I, Error = ParseError>,
{
    move |input: I| {
        let (start, input) = keyword(prefix)
            .map_err(|input: I, _: Kind| {
                ParseError::Expect(super::TokenError::Prefix(prefix), input.span())
            })
            .parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ty, input) = parser.parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (end, input) = keyword(suffix)
            .map_err(|input: I, _: Kind| {
                ParseError::Expect(super::TokenError::Suffix(suffix), input.span())
            })
            .parse(input)?;

        Ok(((Delimiter { start, end }, ty), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Parser, keyword};

    use crate::lang::{Delimiter, TokenStream};

    use super::delimited;

    #[test]
    fn parse_delimiter() {
        assert_eq!(
            delimited("(", keyword("hello world"), ")")
                .parse(TokenStream::from("(  hello world    )")),
            Ok((
                (
                    Delimiter {
                        start: TokenStream::from("("),

                        end: TokenStream::from((18, ")"))
                    },
                    TokenStream::from((3, "hello world"))
                ),
                TokenStream::from((19, ""))
            ))
        );
    }
}
