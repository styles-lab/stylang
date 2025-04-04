use parserc::{Parse, Parser, ParserExt};

use super::{Class, Data, Enum, Fn, Mod, ParseError, StylangInput, Use};

/// A variant for `stylang` stmt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I> {
    Class(Class<I>),
    Data(Data<I>),
    Enum(Enum<I>),
    Fn(Fn<I>),
    Mod(Mod<I>),
    Use(Use<I>),
}

impl<I> Parse<I> for Stmt<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        Class::into_parser()
            .map(|v| Self::Class(v))
            .or(Data::into_parser().map(|v| Self::Data(v)))
            .or(Enum::into_parser().map(|v| Self::Enum(v)))
            .or(Fn::into_parser().map(|v| Self::Fn(v)))
            .or(Mod::into_parser().map(|v| Self::Mod(v)))
            .or(Use::into_parser().map(|v| Self::Use(v)))
            .parse(input)
    }
}
