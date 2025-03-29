//! lit exprs.

use parserc::{ControlFlow, Kind, Parse, Parser, ParserExt, keyword, next, take_while};

use crate::lang::{parse_punctuation_sep, skip_ws};

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

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Rgb<I, D> {
    /// keyword `rgb`
    keyword: I,
    /// delimiter `(...)`
    delimiter: Delimiter<I>,
    /// read component integer: 0~255
    red: D,
    /// ,
    comma1: I,
    /// green component integer: 0~255
    green: D,
    /// ,
    comma2: I,
    /// blue component integer: 0~255
    blue: D,
}

fn parse_rgb_color<I>(input: I) -> parserc::Result<LitColor<I>, I, ParseError>
where
    I: StylangInput,
{
    let (kw, input) = keyword("rgb").parse(input)?;

    let (prefix, input) = next(b'(')
        .map_err(|input: I, _: Kind| ParseError::Expect(Token::Prefix("("), input.span()))
        .fatal()
        .parse(input)?;

    let (_, input) = skip_ws(input)?;

    let (red, input) = Digits::into_parser().fatal().parse(input)?;

    let (red_percent, input) = next(b'%').ok().parse(input)?;

    let (comma1, input) = parse_punctuation_sep(b',')
        .map_err(|input: I, _| ParseError::Expect(Token::Punct(","), input.span()))
        .fatal()
        .parse(input)?;

    let (green, input) = Digits::into_parser().fatal().parse(input)?;

    let (green_percent, input) = next(b'%').ok().parse(input)?;

    if red_percent.is_none() {
        if let Some(green_percent) = green_percent {
            return Err(ControlFlow::Fatal(ParseError::Unexpect(
                Token::Suffix("%"),
                green_percent.span(),
            )));
        }
    } else {
        if green_percent.is_none() {
            return Err(ControlFlow::Fatal(ParseError::Expect(
                Token::Suffix("%"),
                input.span(),
            )));
        }
    }

    let (comma2, input) = parse_punctuation_sep(b',')
        .map_err(|input: I, _| ParseError::Expect(Token::Punct(","), input.span()))
        .fatal()
        .parse(input)?;

    let (blue, input) = Digits::into_parser().fatal().parse(input)?;

    let (blue_percent, input) = next(b'%').ok().parse(input)?;

    if red_percent.is_none() {
        if let Some(blue_percent) = blue_percent {
            return Err(ControlFlow::Fatal(ParseError::Unexpect(
                Token::Suffix("%"),
                blue_percent.span(),
            )));
        }
    } else {
        if blue_percent.is_none() {
            return Err(ControlFlow::Fatal(ParseError::Expect(
                Token::Suffix("%"),
                input.span(),
            )));
        }
    }

    let (_, input) = skip_ws(input)?;

    let (suffix, input) = next(b')')
        .map_err(|input: I, _: Kind| ParseError::Expect(Token::Suffix(")"), input.span()))
        .fatal()
        .parse(input)?;

    if red_percent.is_some() {
        Ok((
            LitColor::RgbPercent(Rgb {
                keyword: kw,
                delimiter: Delimiter { prefix, suffix },
                red: DigitsPercent {
                    digits: red,
                    percent: red_percent.unwrap(),
                },
                comma1,
                green: DigitsPercent {
                    digits: green,
                    percent: green_percent.unwrap(),
                },
                comma2,
                blue: DigitsPercent {
                    digits: blue,
                    percent: blue_percent.unwrap(),
                },
            }),
            input,
        ))
    } else {
        Ok((
            LitColor::Rgb(Rgb {
                keyword: kw,
                delimiter: Delimiter { prefix, suffix },
                red,
                comma1,
                green,
                comma2,
                blue,
            }),
            input,
        ))
    }
}

fn parse_hex_color<I>(input: I) -> parserc::Result<LitColor<I>, I, ParseError>
where
    I: StylangInput,
{
    let (prefix, input) = next(b'#')
        .map_err(|input: I, _: Kind| ParseError::Expect(Token::Color, input.span()))
        .parse(input)?;

    let (digits, input) = HexDigits::into_parser().fatal().parse(input)?;

    Ok((LitColor::Hex { prefix, digits }, input))
}

/// literial color expr.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum LitColor<I> {
    /// be like: `rgb(255,255,255)`
    Rgb(Rgb<I, Digits<I>>),
    /// be like: `rgb(10%,10%,10%)`
    RgbPercent(Rgb<I, DigitsPercent<I>>),
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

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        parse_rgb_color.or(parse_hex_color).parse(input)
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

    #[test]
    fn test_color() {
        assert_eq!(
            LitColor::parse(TokenStream::from("rgb( 255, 255, 255)")),
            Ok((
                LitColor::Rgb(Rgb {
                    keyword: TokenStream::from("rgb"),
                    delimiter: Delimiter {
                        prefix: TokenStream::from((3, "(")),
                        suffix: TokenStream::from((18, ")"))
                    },
                    red: Digits(TokenStream::from((5, "255"))),
                    comma1: TokenStream::from((8, ",")),
                    green: Digits(TokenStream::from((10, "255"))),
                    comma2: TokenStream::from((13, ",")),
                    blue: Digits(TokenStream::from((15, "255")))
                }),
                TokenStream::from((19, ""))
            ),)
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("rgb( 10%, 10%, 30%)")),
            Ok((
                LitColor::RgbPercent(Rgb {
                    keyword: TokenStream::from("rgb"),
                    delimiter: Delimiter {
                        prefix: TokenStream::from((3, "(")),
                        suffix: TokenStream::from((18, ")"))
                    },
                    red: DigitsPercent {
                        digits: Digits(TokenStream::from((5, "10"))),
                        percent: TokenStream::from((7, "%"))
                    },
                    comma1: TokenStream::from((8, ",")),
                    green: DigitsPercent {
                        digits: Digits(TokenStream::from((10, "10"))),
                        percent: TokenStream::from((12, "%"))
                    },
                    comma2: TokenStream::from((13, ",")),
                    blue: DigitsPercent {
                        digits: Digits(TokenStream::from((15, "30"))),
                        percent: TokenStream::from((17, "%"))
                    }
                }),
                TokenStream::from((19, ""))
            ),)
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("#fff")),
            Ok((
                LitColor::Hex {
                    prefix: TokenStream::from("#"),
                    digits: HexDigits(TokenStream::from((1, "fff")))
                },
                TokenStream::from((4, ""))
            ),)
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("rgb( ff, 10, 30%)")),
            Err(ControlFlow::Fatal(ParseError::Expect(
                Token::Digits,
                Span { offset: 5, len: 12 }
            )))
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("rgb(10, 10, 30%)")),
            Err(ControlFlow::Fatal(ParseError::Unexpect(
                Token::Suffix("%"),
                Span { offset: 14, len: 1 }
            )))
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("rgb(10%, 10, 30%)")),
            Err(ControlFlow::Fatal(ParseError::Expect(
                Token::Suffix("%"),
                Span { offset: 11, len: 6 }
            )))
        );

        assert_eq!(
            LitColor::parse(TokenStream::from("#")),
            Err(ControlFlow::Fatal(ParseError::Expect(
                Token::HexDigits,
                Span { offset: 1, len: 0 }
            )))
        );
    }
}
