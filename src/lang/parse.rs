use parserc::{ControlFlow, Parse, Parser, ParserExt};

use super::{Item, ParseError, S, StylangInput, TokenError, TokenStream};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Script<I>(pub Vec<Item<I>>)
where
    I: StylangInput;

impl<I> Parse<I> for Script<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut stmts = vec![];

        loop {
            let stmt;
            (stmt, input) = Item::into_parser().ok().parse(input)?;

            if let Some(stmt) = stmt {
                stmts.push(stmt);

                (_, input) = S::into_parser().ok().parse(input)?;

                if input.is_empty() {
                    return Ok((Self(stmts), input));
                }

                continue;
            }

            return Err(ControlFlow::Fatal(ParseError::Unexpect(
                TokenError::Unknown,
                input.span(),
            )));
        }
    }
}

/// Parse a source file.
pub fn parse(source: &str) -> Result<Script<TokenStream<'_>>, ControlFlow<ParseError>> {
    let input = TokenStream::from(source);

    let (script, _) = Script::parse(input)?;

    Ok(script)
}
