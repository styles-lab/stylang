use parserc::{Kind, Parse, Parser, ParserExt, keyword, next};

use super::{ParseError, StylangInput, TokenError, skip_ws};

/// Parse `[S] punctuation [S]` ...
pub fn parse_punctuation_sep<I>(p: u8) -> impl Parser<I, Error = ParseError, Output = I>
where
    I: StylangInput,
{
    move |input: I| {
        let (_, input) = skip_ws(input)?;

        let (comma, input) = next(p).parse(input)?;

        let (_, input) = skip_ws(input)?;

        Ok((comma, input))
    }
}

/// Parse `[S] punctuation [S]` ...
pub fn token_of<I>(kw: &'static str) -> impl Parser<I, Error = ParseError, Output = I>
where
    I: StylangInput,
{
    move |input: I| {
        let (_, input) = skip_ws(input)?;

        let (comma, input) = keyword(kw)
            .map_err(|input: I, _: Kind| ParseError::Expect(TokenError::Keyword(kw), input.span()))
            .parse(input)?;

        let (_, input) = skip_ws(input)?;

        Ok((comma, input))
    }
}

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Punctuated<I, T, const P: u8> {
    pub items: Vec<(T, I)>,
    pub last: Option<Box<T>>,
}

impl<I, T, const P: u8> Parse<I> for Punctuated<I, T, P>
where
    I: StylangInput,
    T: Parse<I, Error = ParseError>,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut items = vec![];

        let (_, mut input) = skip_ws(input)?;

        loop {
            let item;

            (item, input) = T::into_parser().ok().parse(input)?;

            if let Some(item) = item {
                let punctuated;
                (punctuated, input) = parse_punctuation_sep(P).ok().parse(input)?;

                if let Some(punctuated) = punctuated {
                    items.push((item, punctuated));
                } else {
                    return Ok((
                        Punctuated {
                            items,
                            last: Some(Box::new(item)),
                        },
                        input,
                    ));
                }
            } else {
                return Ok((Punctuated { items, last: None }, input));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_punctuated() {}
}
