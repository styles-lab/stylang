use parserc::{Parse, Parser, derive_parse, keyword, take_till, take_until};

use super::{ParseError, StylangInput};

/// Outer line doc, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutlineDoc<I>(pub I);

impl<I> Parse<I> for OutlineDoc<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("///").parse(input)?;

        let (content, mut input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Self(content), input.split_off(1)))
    }
}

/// Line comment, be like: `// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineComment<I>(pub I);

impl<I> Parse<I> for LineComment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("//").parse(input)?;

        let (content, mut input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Self(content), input.split_off(1)))
    }
}

/// Block comment, be like: `/* ... */`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockComment<I>(pub I);

impl<I> Parse<I> for BlockComment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("/*").parse(input)?;

        let (content, mut input) = take_until("*/").parse(input)?;

        Ok((Self(content), input.split_off(2)))
    }
}

/// Outer block comment, be like: `/** ... */`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutBlockDoc<I>(pub I);

impl<I> Parse<I> for OutBlockDoc<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("/**").parse(input)?;

        let (content, mut input) = take_until("*/").parse(input)?;

        Ok((Self(content), input.split_off(2)))
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Comment<I>
where
    I: StylangInput,
{
    LineComment(LineComment<I>),
    OutlineDoc(OutlineDoc<I>),
    BlockComment(BlockComment<I>),
    OutBlockDoc(OutBlockDoc<I>),
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_comment() {}
}
