use parserc::{Parse, derive_parse};

use crate::lang::{errors::LangError, input::LangInput, item::Item, token::Brace};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Stmt<I>
where
    I: LangInput,
{
    /// declare a `class`,`data`,`fn`,...
    Item(Item<I>),
}

/// A list of stmts.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Stmts<I>(pub Vec<Stmt<I>>)
where
    I: LangInput;

impl<I> Parse<I> for Stmts<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut stmts = vec![];

        loop {
            let stmt;
            (stmt, input) = Stmt::parse(input)?;

            match &stmt {
                Stmt::Item(Item::MetaList(meta_list)) if meta_list.is_empty() => {
                    break;
                }
                _ => {}
            }

            stmts.push(stmt);
        }

        Ok((Self(stmts), input))
    }
}

/// stmts group by `{..}`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Block<I>(pub Brace<I, Stmts<I>>)
where
    I: LangInput;
