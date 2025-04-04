use parserc::{Parse, Parser, ParserExt, keyword};

use super::{ParseError, StylangInput, skip_ws};

/// The visibility level of an item: `pub`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Visibility<I> {
    Public(I),
    Private,
}

impl<I> Parse<I> for Visibility<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = skip_ws(input)?;
        let (v, input) = keyword("pub").ok().parse(input)?;
        let (_, input) = skip_ws(input)?;

        if let Some(v) = v {
            Ok((Self::Public(v), input))
        } else {
            Ok((Self::Private, input))
        }
    }
}
