use std::borrow::Cow;

/// Value type used by widget.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Value<'a, T> {
    Constant(T),
    XPath(Cow<'a, str>),
}
