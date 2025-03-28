use parserc::{Parser, take_while};

use super::{ParseError, StylangInput, Token};

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
            Token::S,
            span,
        )));
    }

    Ok(((), input))
}
