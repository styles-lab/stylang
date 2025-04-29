use parserc::derive_parse;

use super::{Expr, Item, ParseError, SemiColon, StylangInput};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Stmt<I>
where
    I: StylangInput,
{
    Item(Item<I>),
    Expr(Expr<I>, Option<SemiColon<I>>),
}
