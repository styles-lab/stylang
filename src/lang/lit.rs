//! lit exprs.

use parserc::{ControlFlow, Kind, Parse, Parser, ParserExt, next, take_while};

use super::{Delimiter, ParseError, StylangInput, Token};

/// An ascii hex-digit characters sequence: [0-9a-f]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct HexDigits<I>(pub I);

impl<I> Parse<I> for HexDigits<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (digits, input) = take_while(|c: u8| c.is_ascii_hexdigit()).parse(input)?;

        if digits.is_empty() {
            return Err(ControlFlow::Recovable(ParseError::Expect(
                Token::HexDigits,
                input.span(),
            )));
        }

        Ok((HexDigits(digits), input))
    }
}

/// An ascii digit characters sequence: [0-9]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Digits<I>(pub I);

impl<I> Parse<I> for Digits<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (digits, input) = take_while(|c: u8| c.is_ascii_digit()).parse(input)?;

        if digits.is_empty() {
            return Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Digits,
                input.span(),
            )));
        }

        Ok((Digits(digits), input))
    }
}

/// An ascii digit characters sequence + `%`.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct DigitsPercent<I> {
    /// integer: [0-9]+
    pub digits: Digits<I>,
    /// punct `%`
    pub percent: I,
}

impl<I> Parse<I> for DigitsPercent<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (digits, input) = Digits::parse(input)?;

        let (percent, input) = next(b'%')
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Suffix("%"), input.span()))
            .parse(input)?;

        Ok((DigitsPercent { digits, percent }, input))
    }
}

/// literial color expr.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum LitColor<I> {
    /// be like: `rgb(255,255,255)`
    Rgb {
        /// keyword `rgb`
        keyword: I,
        /// delimiter `(...)`
        delimiter: Delimiter<I>,
        /// read component integer: 0~255
        red: Digits<I>,
        /// ,
        comma1: I,
        /// green component integer: 0~255
        green: Digits<I>,
        /// ,
        comma2: I,
        /// blue component integer: 0~255
        blue: Digits<I>,
    },
    /// be like: `rgb(10%,10%,10%)`
    RgbPercent {
        /// keyword `rgb`
        keyword: I,
        /// delimiter `(...)`
        delimiter: Delimiter<I>,
        /// read component integer: 0~100
        red: DigitsPercent<I>,
        /// ,
        comma1: I,
        /// green component integer: 0~100
        green: DigitsPercent<I>,
        /// ,
        comma2: I,
        /// blue component integer: 0~100
        blue: DigitsPercent<I>,
    },
    /// be like: `#fff` or `#f0f0f0`
    Hex {
        /// hex color prefix `#`
        prefix: I,
        /// sequence of color hex digits.
        digits: HexDigits<I>,
    },
}

impl<I> Parse<I> for LitColor<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(_input: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use parserc::span::Span;

    use crate::lang::TokenStream;

    use super::*;

    #[test]
    fn test_digits() {
        assert_eq!(
            Digits::parse(TokenStream::from("123")),
            Ok((Digits(TokenStream::from("123")), TokenStream::from((3, ""))))
        );

        assert_eq!(
            Digits::parse(TokenStream::from("")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Digits,
                Span { offset: 0, len: 0 }
            )))
        );
    }

    #[test]
    fn test_hex_digits() {
        assert_eq!(
            HexDigits::parse(TokenStream::from("f0a0")),
            Ok((
                HexDigits(TokenStream::from("f0a0")),
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            HexDigits::parse(TokenStream::from("")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::HexDigits,
                Span { offset: 0, len: 0 }
            )))
        );
    }

    #[test]
    fn test_digits_percent() {
        assert_eq!(
            DigitsPercent::parse(TokenStream::from("10%")),
            Ok((
                DigitsPercent {
                    digits: Digits(TokenStream::from("10")),
                    percent: TokenStream::from((2, "%"))
                },
                TokenStream::from((3, ""))
            ))
        );

        assert_eq!(
            DigitsPercent::parse(TokenStream::from("10a")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Suffix("%"),
                Span { offset: 2, len: 1 }
            )))
        );
    }
}
