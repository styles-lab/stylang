use super::{Expr, Item};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I> {
    /// A item definition.
    Item(Item<I>),
    /// Expression, with or without trailing semicolon.
    Expr(Expr<I>, Option<I>),
}
