use std::borrow::Cow;

use super::{AxisDirection, Handle, Id, Value};

/// A scrollable list of widgets arranged linearly.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ListView<'a>(
    /// The scrolling direction of the list view.
    pub Value<'a, AxisDirection>,
);

/// Standard Image view.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Image<'a>(
    /// image url.
    pub Value<'a, Cow<'a, str>>,
);

/// Standard button view.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Button<'a> {
    /// Button label text.
    pub text: Value<'a, Cow<'a, str>>,
    /// Click handle.
    pub click: Handle<'a>,
    /// Optional icon.
    pub icon: Option<Value<'a, Cow<'a, str>>>,
}

/// Optional icon of the [`Button`] view.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TextField<'a> {
    /// Reference id of this text field.
    pub id: Id<'a>,
    /// A Text representing the prompt of the text field which provides users
    /// with guidance on what to type into the text field.
    pub prompt: Value<'a, Cow<'a, str>>,
}

/// A standard label for user interface items, consisting of an icon with a title.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Label<'a> {
    /// Button label text.
    pub text: Value<'a, Cow<'a, str>>,
    /// Optional icon.
    pub icon: Option<Value<'a, Cow<'a, str>>>,
}
