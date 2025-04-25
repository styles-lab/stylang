use parserc::{Parse, Parser, ParserExt};

use super::{ParseError, S, StylangInput};

/// Parse `[S] punctuation [S]` ...
fn parse_punctuation_sep<I, P>() -> impl Parser<I, Error = ParseError, Output = P>
where
    I: StylangInput,
    P: Parse<I, Error = ParseError>,
{
    move |input: I| {
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (comma, input) = P::parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

        Ok((comma, input))
    }
}

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Punctuated<T, P> {
    pub items: Vec<(T, P)>,
    pub last: Option<Box<T>>,
}

impl<I, T, P> Parse<I> for Punctuated<T, P>
where
    I: StylangInput,
    T: Parse<I, Error = ParseError>,
    P: Parse<I, Error = ParseError>,
{
    type Error = ParseError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut items = vec![];

        loop {
            (_, input) = S::into_parser().ok().parse(input)?;

            let item;

            (item, input) = T::into_parser().ok().parse(input)?;

            if let Some(item) = item {
                let punctuated;
                (punctuated, input) = parse_punctuation_sep::<I, P>().ok().parse(input)?;

                if let Some(punctuated) = punctuated {
                    items.push((item, punctuated));
                } else {
                    (_, input) = S::into_parser().ok().parse(input)?;
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

    use crate::lang::{Comma, I32, Lit, LitStr, TokenStream};

    use super::Punctuated;

    #[test]
    fn test_punct() {
        assert_eq!(
            Punctuated::<I32<_>, Comma<_>>::parse(TokenStream::from("i32, i32 , i32")),
            Ok((
                Punctuated {
                    items: vec![
                        (
                            I32(TokenStream::from((0, "i32"))),
                            Comma(TokenStream::from((3, ",")))
                        ),
                        (
                            I32(TokenStream::from((5, "i32"))),
                            Comma(TokenStream::from((9, ",")))
                        )
                    ],
                    last: Some(Box::new(I32(TokenStream::from((11, "i32")))))
                },
                TokenStream::from((14, ""))
            ))
        );

        assert_eq!(
            Punctuated::<Lit<_>, Comma<_>>::parse(TokenStream::from(r#""hello world""#)),
            Ok((
                Punctuated {
                    items: vec![],
                    last: Some(Box::new(Lit::String(LitStr(TokenStream::from((
                        1,
                        "hello world"
                    ))))))
                },
                TokenStream::from((13, ""))
            ))
        );
    }
}
