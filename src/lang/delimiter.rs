use parserc::Parser;

use super::{ParseError, StylangInput, skip_ws};

/// A token surround by `start` and `end` tokens.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Delimiter<I, C> {
    /// prefix token.
    pub prefix: I,
    /// body token.
    pub content: C,
    /// suffix token.
    pub suffix: I,
}

/// Match a sequence parsers: prefix + content + suffix.
pub fn delimited<I, P, C, S>(
    mut prefix: P,
    mut parser: C,
    mut suffix: S,
) -> impl Parser<I, Output = Delimiter<I, C::Output>, Error = ParseError>
where
    I: StylangInput,
    P: Parser<I, Output = I, Error = ParseError>,
    C: Parser<I, Error = ParseError>,
    S: Parser<I, Output = I, Error = ParseError>,
{
    move |input: I| {
        let (start, input) = prefix.parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (ty, input) = parser.parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (end, input) = suffix.parse(input)?;

        Ok((
            Delimiter {
                prefix: start,
                content: ty,
                suffix: end,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Parser, keyword, next};

    use crate::lang::{Delimiter, TokenStream};

    use super::delimited;

    #[test]
    fn parse_delimiter() {
        assert_eq!(
            delimited(next(b'('), keyword("hello world"), next(b')'))
                .parse(TokenStream::from("(  hello world    )")),
            Ok((
                Delimiter {
                    prefix: TokenStream::from("("),
                    content: TokenStream::from((3, "hello world")),
                    suffix: TokenStream::from((18, ")"))
                },
                TokenStream::from((19, ""))
            ))
        );
    }
}
