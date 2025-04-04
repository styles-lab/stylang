use parserc::{Parse, Parser, ParserExt, keyword};

use super::{Ident, ParseError, StylangInput, skip_ws};

/// A named item path.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Path<I> {
    /// (segment,::) pairs.
    pub segments: Vec<(Ident<I>, I)>,
    /// optional last segment.
    pub last: Option<Ident<I>>,
}

impl<I> Parse<I> for Path<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut segments = vec![];

        let (_, mut input) = skip_ws(input)?;

        loop {
            let item;

            (item, input) = Ident::into_parser().ok().parse(input)?;

            if let Some(item) = item {
                let punctuated;

                (punctuated, input) = keyword("::").ok().parse(input)?;

                if let Some(punctuated) = punctuated {
                    segments.push((item, punctuated));
                } else {
                    return Ok((
                        Self {
                            segments,
                            last: Some(item),
                        },
                        input,
                    ));
                }
            } else {
                return Ok((
                    Self {
                        segments,
                        last: None,
                    },
                    input,
                ));
            }
        }
    }
}
