use parserc::{Parse, Parser, ParserExt};

use super::{ItemClass, ItemData, ItemEnum, ItemFn, ItemMod, ItemUse, ParseError, StylangInput};

/// A variant for `stylang` stmt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Item<I> {
    Class(ItemClass<I>),
    Data(ItemData<I>),
    Enum(ItemEnum<I>),
    Fn(ItemFn<I>),
    Mod(ItemMod<I>),
    Use(ItemUse<I>),
}

impl<I> Parse<I> for Item<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        ItemClass::into_parser()
            .map(|v| Self::Class(v))
            .or(ItemData::into_parser().map(|v| Self::Data(v)))
            .or(ItemEnum::into_parser().map(|v| Self::Enum(v)))
            .or(ItemFn::into_parser().map(|v| Self::Fn(v)))
            .or(ItemMod::into_parser().map(|v| Self::Mod(v)))
            .or(ItemUse::into_parser().map(|v| Self::Use(v)))
            .parse(input)
    }
}
