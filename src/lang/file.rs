use std::ops::Deref;

use parserc::{ControlFlow, Input, Parse, span::WithSpan};

use crate::lang::{
    errors::LangError,
    input::{LangInput, TokenStream},
    item::Item,
};

/// A parser for one `stylang` source file.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct File<I>(pub Vec<Item<I>>)
where
    I: LangInput;

impl<I> Deref for File<I>
where
    I: LangInput,
{
    type Target = Vec<Item<I>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<I> Parse<I> for File<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(mut input: I) -> parserc::errors::Result<Self, I, Self::Error> {
        let mut items = vec![];

        loop {
            let item;
            (item, input) = Item::parse(input)?;

            match &item {
                Item::MetaList(meta_list) if meta_list.is_empty() => {
                    break;
                }
                _ => {}
            }

            items.push(item);
        }

        Ok((Self(items), input))
    }
}

impl<'a> TryFrom<&'a str> for File<TokenStream<'a>> {
    type Error = ControlFlow<LangError>;

    fn try_from(source: &'a str) -> Result<Self, Self::Error> {
        let input = TokenStream::from(source);

        let (script, _) = File::parse(input)?;

        Ok(script)
    }
}

impl<'a> TryFrom<&'a String> for File<TokenStream<'a>> {
    type Error = ControlFlow<LangError>;

    fn try_from(source: &'a String) -> Result<Self, Self::Error> {
        let input = TokenStream::from(source.as_str());

        let (script, input) = File::parse(input)?;

        if !input.is_empty() {
            return Err(ControlFlow::Fatal(LangError::Unparsed(input.span())));
        }

        Ok(script)
    }
}
