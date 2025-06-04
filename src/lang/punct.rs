//! An untility to help parse punctuated token stream.

use parserc::{Parse, Parser, ParserExt};

use crate::lang::{errors::LangError, inputs::LangInput, tokens::S};

/// A punctuated sequence of syntax tree nodes of type T separated by punctuation of type P.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Punctuated<T, P> {
    /// (T,P) pairs
    pub pairs: Vec<(T, P)>,
    /// individual tail `T`
    pub tail: Option<Box<T>>,
}

impl<T, P> Default for Punctuated<T, P> {
    fn default() -> Self {
        Self {
            pairs: vec![],
            tail: None,
        }
    }
}

impl<T, P, I> Parse<I> for Punctuated<T, P>
where
    T: Parse<I, Error = LangError>,
    P: Parse<I, Error = LangError>,
    I: LangInput,
{
    type Error = LangError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut pairs = vec![];

        loop {
            let t;

            (_, input) = S::into_parser().ok().parse(input)?;

            (t, input) = T::into_parser().ok().parse(input)?;

            let Some(t) = t else {
                return Ok((Self { pairs, tail: None }, input));
            };

            (_, input) = S::into_parser().ok().parse(input)?;
            let p;
            (p, input) = P::into_parser().ok().parse(input)?;

            let Some(p) = p else {
                return Ok((
                    Self {
                        pairs,
                        tail: Some(Box::new(t)),
                    },
                    input,
                ));
            };

            pairs.push((t, p));
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        inputs::TokenStream,
        lit::{Lit, LitStr},
        tokens::{Comma, I32, S},
    };

    use super::Punctuated;

    #[test]
    fn test_punct() {
        assert_eq!(
            Punctuated::<I32<_>, Comma<_>>::parse(TokenStream::from("i32, i32 , i32")),
            Ok((
                Punctuated {
                    pairs: vec![
                        (
                            I32(TokenStream::from((0, "i32"))),
                            Comma(TokenStream::from((3, ",")))
                        ),
                        (
                            I32(TokenStream::from((5, "i32"))),
                            Comma(TokenStream::from((9, ","))),
                        )
                    ],
                    tail: Some(Box::new(I32(TokenStream::from((11, "i32")))))
                },
                TokenStream::from((14, ""))
            ))
        );

        assert_eq!(
            Punctuated::<Lit<_>, (Option<S<_>>, Comma<_>, Option<S<_>>)>::parse(TokenStream::from(
                r#""hello world""#
            )),
            Ok((
                Punctuated {
                    pairs: vec![],
                    tail: Some(Box::new(Lit::String(LitStr(TokenStream::from((
                        1,
                        "hello world"
                    ))))))
                },
                TokenStream::from((13, ""))
            ))
        );
    }
}
