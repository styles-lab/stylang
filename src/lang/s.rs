use parserc::{
    AsBytes, Input, Parse, Parser, keyword, next,
    span::{Span, WithSpan},
    take_till, take_while,
};

use super::ParseError;

pub(super) fn skip_ws<I>(input: I) -> parserc::Result<(), I, ParseError>
where
    I: Input<Item = u8>,
{
    let (_, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

    Ok(((), input))
}

/// be like: `[S],[S]`
pub(super) fn parse_comma_sep<I>(input: I) -> parserc::Result<I, I, ParseError>
where
    I: Input<Item = u8>,
{
    let (_, input) = skip_ws(input)?;

    let (comma, input) = next(b',').parse(input)?;

    let (_, input) = skip_ws(input)?;

    Ok((comma, input))
}

/// be like: `[S]->[S]`
pub(super) fn parse_return_type_sep<I>(input: I) -> parserc::Result<I, I, ParseError>
where
    I: Input<Item = u8> + AsBytes,
{
    let (_, input) = skip_ws(input)?;

    let (sep, input) = keyword("->").parse(input)?;

    let (_, input) = skip_ws(input)?;

    Ok((sep, input))
}

/// Comment of the function, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
pub struct Comment<I> {
    pub content: I,
    pub span: Span,
}

impl<I> Parse<I> for Comment<I>
where
    I: Input<Item = u8> + AsBytes + WithSpan,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (prefix, input) = keyword("///").parse(input)?;

        let (content, input) = take_till(|c| c == b'\n').parse(input)?;

        let span = prefix.span().extend_to(input.span());

        Ok((Comment { content, span }, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{Parse, span::Span};

    use crate::lang::{Comment, Source};

    #[test]
    fn test_comment_line() {
        assert_eq!(
            Comment::parse(Source::from("/// hello world  \n")),
            Ok((
                Comment {
                    content: Source::from((3, " hello world  ")),
                    span: Span { offset: 0, len: 17 }
                },
                Source::from((17, "\n"))
            ))
        );
    }
}
