use parserc::Parse;

use super::{Delimiter, ParseError, StylangInput};

/// A variable declaration stmt, be like: `let .. = ...;`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Let<I> {
    /// let stmt keyword `let`.
    pub keyword: I,
}

/// Variant for in block stmt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BlockStmt<I> {
    Let(Let<I>),
}

/// A code block delimited by `{...}`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block<I> {
    /// delimiter of the block: `{...}`
    pub delimiter: Delimiter<I>,
    /// stmts belongs to this block.
    pub stmts: Vec<BlockStmt<I>>,
}

impl<I> Parse<I> for Block<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(_input: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}
