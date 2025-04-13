use parserc::{Parse, Parser};

use crate::lang::{delimited, parse_stmts};

use super::{Delimiter, ParseError, Stmt, StylangInput};

/// A braced block containing stylang statements.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block<I> {
    /// delimiter: `{ ... }`
    pub delimiter: Delimiter<I>,
    /// statements in this block.
    pub stmts: Vec<Stmt<I>>,
}

impl<I> Parse<I> for Block<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let ((delimiter, stmts), input) = delimited("{", parse_stmts, "}").parse(input)?;
        Ok((Self { delimiter, stmts }, input))
    }
}
