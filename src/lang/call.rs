use super::{Delimiter, LitExpr, Punctuated};

/// All params of this type call must be literial exprs.
#[derive(Debug, PartialEq, Clone)]
pub struct LitCall<I> {
    /// delimiter: `(...)`
    pub delimiter: Delimiter<I>,
    /// input params.
    pub inputs: Punctuated<I, LitExpr<I>, b','>,
}
