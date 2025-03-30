use parserc::{Parse, Parser, ParserExt, keyword};

use super::{ParseError, StylangInput};

/// literial length unit: em,px,...
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnitLen<I>(pub I);

impl<I> Parse<I> for UnitLen<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (unit, input) = keyword("em")
            .or(keyword("ex"))
            .or(keyword("px"))
            .or(keyword("in"))
            .or(keyword("cm"))
            .or(keyword("mm"))
            .or(keyword("pt"))
            .or(keyword("pc"))
            .parse(input)?;

        Ok((UnitLen(unit), input))
    }
}

/// literial length unit: deg, grad, rad
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnitAngle<I>(pub I);

impl<I> Parse<I> for UnitAngle<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (unit, input) = keyword("deg")
            .or(keyword("grad"))
            .or(keyword("rad"))
            .parse(input)?;

        Ok((Self(unit), input))
    }
}

/// literial length unit: u8,i32,i64,i128,ib,ub
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnitInteger<I>(pub I);

impl<I> Parse<I> for UnitInteger<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (unit, input) = keyword("i8")
            .or(keyword("i16"))
            .or(keyword("i32"))
            .or(keyword("i64"))
            .or(keyword("i128"))
            .or(keyword("bigint"))
            .or(keyword("u8"))
            .or(keyword("u16"))
            .or(keyword("u32"))
            .or(keyword("u64"))
            .or(keyword("u128"))
            .parse(input)?;

        Ok((Self(unit), input))
    }
}

/// literial length unit: f32,f64,bignum,
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnitFloat<I>(pub I);

impl<I> Parse<I> for UnitFloat<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (unit, input) = keyword("f32")
            .or(keyword("f64"))
            .or(keyword("bignum"))
            .parse(input)?;

        Ok((Self(unit), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{TokenStream, UnitAngle, UnitLen};

    #[test]
    fn test_unit_len() {
        assert_eq!(
            UnitLen::parse(TokenStream::from("em")),
            Ok((UnitLen(TokenStream::from("em")), TokenStream::from((2, ""))))
        );
    }

    #[test]
    fn test_unit_angle() {
        assert_eq!(
            UnitAngle::parse(TokenStream::from("deg")),
            Ok((
                UnitAngle(TokenStream::from("deg")),
                TokenStream::from((3, ""))
            ))
        );
    }
}
