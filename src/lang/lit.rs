use parserc::Parse;

use super::{LitColor, LitHexNum, LitNum, LitStr, ParseError, StylangInput, Unit};

/// Variant for literial expr.
#[derive(Debug, PartialEq, Clone)]
pub enum LitExpr<I> {
    Num(LitNum<I>, Option<Unit<I>>),
    Hex(LitHexNum<I>),
    Str(LitStr<I>),
    Color(LitColor<I>),
}

impl<I> Parse<I> for LitExpr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(_: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}
