use super::Id;

/// Use a navigation stack to present a stack of views over a root view.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NavigationStack<'a>(
    /// The root view id.
    pub Id<'a>,
);

/// Display content by `NavigationStack`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct NavigationView<'a>(
    /// The id of the navigation view.
    pub Id<'a>,
);

/// Present content in a separate view
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ModalView<'a>(
    /// The id of the modal view.
    pub Id<'a>,
);
