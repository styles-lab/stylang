use parserc::{Parse, Parser, ParserExt, keyword};

use super::{ParseError, StylangInput};

/// The visibility level of an item: `pub`.
#[derive(Debug, PartialEq, Clone)]
pub struct Visibility<I>(pub I);

impl<I> Parse<I> for Visibility<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        keyword("pub").map(|v| Self(v)).parse(input)
    }
}
