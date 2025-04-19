use parserc::{Parse, Parser, ParserExt, next};

use super::{ParseError, S, StylangInput};

/// Parse `[S] punctuation [S]` ...
fn parse_punctuation_sep<I>(p: u8) -> impl Parser<I, Error = ParseError, Output = I>
where
    I: StylangInput,
{
    move |input: I| {
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (comma, input) = next(p).parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

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

        let (_, mut input) = S::into_parser().ok().parse(input)?;

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
    use parserc::Parse;

    use crate::lang::{I32, TokenStream};

    use super::Punctuated;

    #[test]
    fn test_punct() {
        assert_eq!(
            Punctuated::<_, I32<_>, b','>::parse(TokenStream::from("i32, i32 , i32")),
            Ok((
                Punctuated {
                    items: vec![
                        (
                            I32(TokenStream::from((0, "i32"))),
                            TokenStream::from((3, ","))
                        ),
                        (
                            I32(TokenStream::from((5, "i32"))),
                            TokenStream::from((9, ","))
                        )
                    ],
                    last: Some(Box::new(I32(TokenStream::from((11, "i32")))))
                },
                TokenStream::from((14, ""))
            ))
        );
    }
}
