use super::{Comma, Expr, LeftParenthesis, Punctuated, RightParenthesis, StylangInput};

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprCall<I>
where
    I: StylangInput,
{
    pub target: Box<Expr<I>>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParenthesis<I>,
    /// params list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParenthesis<I>,
}
