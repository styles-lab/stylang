use super::LitExpr;

/// A stylang expression.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<I> {
    Lit(LitExpr<I>),
}
