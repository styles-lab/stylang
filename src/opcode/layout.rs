use super::Variable;

/// The ZStack assigns each successive subview a higher z-axis value than the one before it,
/// meaning later subviews appear “on top” of earlier ones.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ZStack;

/// A convenience widget that combines common painting, positioning, and sizing widgets.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Container<'a> {
    /// The fixed width of the container.
    pub width: Variable<'a, f32>,
    /// The fixed height of the container.
    pub height: Variable<'a, f32>,
}

/// A alias of Aligment.Center.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Center;

/// A widget that displays its children in a horizontal array.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Row;

/// A widget that displays its children in a vertical array.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Column;

/// A widget that expands a child of a Row, Column,
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Expanded;

/// A widget that controls how a child of a Row, Column,
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Flexible<'a>(
    /// The flex factor to use for this child.
    pub Variable<'a, usize>,
);
