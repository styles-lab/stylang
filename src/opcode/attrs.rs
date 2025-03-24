use std::borrow::Cow;

use super::Variable;

/// The widget display/search name.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Id<'a> {
    /// points to the offset num in the id table.
    Offset(usize),
    /// points to the real display string.
    Display(Cow<'a, str>),
}

/// The event type raised by stylang ui.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Event<'a> {
    Click,
    Hover,
    Custom(Cow<'a, str>),
}

/// Event handle type.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Handle<'a> {
    /// In-app navigation action.
    Navigation(Cow<'a, str>),
    /// A html href jumping action.
    Hyperlink(Cow<'a, str>),
    /// (Solidity) A smart contract calling.
    Solidity(Cow<'a, str>),
    /// Inline closure fn.
    Inline(Cow<'a, str>),
    /// Generic callback method,
    Generic(Cow<'a, str>),
}

///An attribute that append an event handler to an element.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OnEvent<'a> {
    /// event type.
    pub event: Event<'a>,
    /// event handle.
    pub handle: Handle<'a>,
}

/// Class attribute that applies to drawing element.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Class<'a> {
    /// Class from theme scope.
    Theme(Variable<'a, Cow<'a, str>>),
    /// Class from application scope.
    App(Variable<'a, Cow<'a, str>>),
}

/// A direction along either the horizontal or vertical Axis in which the origin, or zero position, is determined.
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AxisDirection {
    Up,
    Right,
    Down,
    Left,
}
