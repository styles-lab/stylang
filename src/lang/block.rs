use super::Delimiter;

/// A code block delimited by `{...}`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block<I> {
    pub delimiter: Delimiter<I>,
}
