//! A parser for `stylang` script.
use parserc::{ControlFlow, Parse, Parser, ParserExt};

use super::{
    errors::{LangError, TokenKind},
    inputs::{LangInput, TokenStream},
    item::Item,
    tokens::S,
};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Script<I>(pub Vec<Item<I>>)
where
    I: LangInput;

impl<I> Parse<I> for Script<I>
where
    I: LangInput,
{
    type Error = LangError;

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

            return Err(ControlFlow::Fatal(LangError::unexpect(
                TokenKind::Unknown,
                input.span(),
            )));
        }
    }
}

impl<'a> TryFrom<&'a str> for Script<TokenStream<'a>> {
    type Error = ControlFlow<LangError>;

    fn try_from(source: &'a str) -> Result<Self, Self::Error> {
        let input = TokenStream::from(source);

        let (script, _) = Script::parse(input)?;

        Ok(script)
    }
}

impl<'a> TryFrom<&'a String> for Script<TokenStream<'a>> {
    type Error = ControlFlow<LangError>;

    fn try_from(source: &'a String) -> Result<Self, Self::Error> {
        let input = TokenStream::from(source.as_str());

        let (script, _) = Script::parse(input)?;

        Ok(script)
    }
}
