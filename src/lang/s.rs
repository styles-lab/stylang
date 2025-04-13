use parserc::{Parse, Parser, ParserExt, keyword, take_till, take_while};

use super::{ParseError, StylangInput, TokenError};

/// Skip `S` characters.
pub fn skip_ws<I>(input: I) -> parserc::Result<I, I, ParseError>
where
    I: StylangInput,
{
    let (s, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

    Ok((s, input))
}

/// parse `S` characters.
pub fn ws<I>(input: I) -> parserc::Result<(), I, ParseError>
where
    I: StylangInput,
{
    let (s, input) = skip_ws(input)?;

    if s.is_empty() {
        let mut span = input.span();
        span.len = 0;

        return Err(parserc::ControlFlow::Recovable(ParseError::Expect(
            TokenError::S,
            span,
        )));
    }

    Ok(((), input))
}

/// Comment of the function, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Comment<I>(pub I);

impl<I> Parse<I> for Comment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("///").parse(input)?;

        let (content, input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Comment(content), input))
    }
}

/// parse comment list.
pub fn parse_comment_list<I>(mut input: I) -> parserc::Result<Vec<Comment<I>>, I, ParseError>
where
    I: StylangInput,
{
    let mut result = vec![];

    loop {
        (_, input) = skip_ws(input)?;
        let v;

        (v, input) = Comment::into_parser().ok().parse(input)?;

        if let Some(v) = v {
            result.push(v);
            continue;
        }

        break;
    }

    Ok((result, input))
}
