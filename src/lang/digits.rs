use parserc::{ControlFlow, Parse, Parser, ParserExt, keyword, next, take_while};

use super::{ParseError, StylangInput, TokenError, Unit};

/// An ascii hex-digit characters sequence: [0-9a-f]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
                TokenError::HexDigits,
                input.span(),
            )));
        }

        Ok((HexDigits(digits), input))
    }
}

/// An ascii digit characters sequence: [0-9]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
                TokenError::Digits,
                input.span(),
            )));
        }

        Ok((Digits(digits), input))
    }
}

/// Sign character used by [`LitInt`] or [`LitNum`].
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Sign<I>(pub I);

impl<I> Parse<I> for Sign<I>
where
    I: StylangInput,
{
    type Error = ParseError;
    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (v, input) = next(b'+').or(next(b'-')).parse(input)?;

        Ok((Self(v), input))
    }
}

/// Exp part: [Ee]{1} [+-]? digits.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Exponent<I> {
    /// exp prefix, `E` or `e`
    pub prefix: I,
    /// sign character: `+` or `-`
    pub sign: Option<Sign<I>>,
    /// digits part.
    pub digits: Digits<I>,
}

impl<I> Parse<I> for Exponent<I>
where
    I: StylangInput,
{
    type Error = ParseError;
    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (prefix, input) = next(b'E').or(next(b'e')).parse(input)?;
        let (sign, input) = Sign::into_parser().ok().parse(input)?;
        let (digits, input) = Digits::parse(input)?;

        Ok((
            Self {
                prefix,
                sign,
                digits,
            },
            input,
        ))
    }
}

/// literial integer value: `[+-]? [0-9]+`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitNum<I> {
    /// optional sign character: `+` or `-`
    pub sign: Option<Sign<I>>,
    /// optional trunc part.
    pub trunc: Option<Digits<I>>,
    /// optional [S].[S]
    pub comma: Option<I>,
    /// optional fractional part.
    pub fract: Option<Digits<I>>,
    /// optional exp part.
    pub exp: Option<Exponent<I>>,
    /// optional unit part.
    pub unit: Option<Unit<I>>,
}

impl<I> Parse<I> for LitNum<I>
where
    I: StylangInput,
{
    type Error = ParseError;
    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (sign, input) = Sign::into_parser().ok().parse(input)?;
        let (trunc, input) = Digits::into_parser().ok().parse(input)?;
        let (comma, input) = next(b'.').ok().parse(input)?;
        let (fract, input) = Digits::into_parser().ok().parse(input)?;

        if trunc.is_none() && fract.is_none() {
            return Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Digits,
                input.span(),
            )));
        }

        let (exp, input) = Exponent::into_parser().ok().parse(input)?;

        let (unit, input) = Unit::into_parser().ok().parse(input)?;

        Ok((
            Self {
                sign,
                trunc,
                comma,
                fract,
                exp,
                unit,
            },
            input,
        ))
    }
}

/// literial hex integer num: `0x[0-9a-fA-F]+`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitHexNum<I> {
    /// The required hex prefix string: `0x`
    pub prefix: I,
    /// optional trunc part.
    pub digits: HexDigits<I>,
}

impl<I> Parse<I> for LitHexNum<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (prefix, input) = keyword("0x").parse(input)?;
        let (digits, input) = HexDigits::parse(input)?;

        Ok((LitHexNum { prefix, digits }, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        Digits, Exponent, HexDigits, LitHexNum, LitNum, ParseError, Sign, TokenError, TokenStream, Unit,
        UnitLen,
    };

    #[test]
    fn test_lit_hex_num() {
        assert_eq!(
            LitHexNum::parse(TokenStream::from("0xf0a0B0")),
            Ok((
                LitHexNum {
                    prefix: TokenStream::from("0x"),
                    digits: HexDigits(TokenStream::from((2, "f0a0B0"))),
                },
                TokenStream::from((8, ""))
            ))
        );
    }

    #[test]
    fn test_lit_num() {
        assert_eq!(
            LitNum::parse(TokenStream::from("10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: Some(Digits(TokenStream::from("10"))),
                    comma: None,
                    fract: None,
                    exp: None,
                    unit: None
                },
                TokenStream::from((2, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from("0.10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: Some(Digits(TokenStream::from("0"))),
                    comma: Some(TokenStream::from((1, "."))),
                    fract: Some(Digits(TokenStream::from((2, "10")))),
                    exp: None,
                    unit: None
                },
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from(".10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: None,
                    comma: Some(TokenStream::from((0, "."))),
                    fract: Some(Digits(TokenStream::from((1, "10")))),
                    exp: None,
                    unit: None
                },
                TokenStream::from((3, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from(".10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: None,
                    comma: Some(TokenStream::from((0, "."))),
                    fract: Some(Digits(TokenStream::from((1, "10")))),
                    exp: None,
                    unit: None
                },
                TokenStream::from((3, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from(".10e-10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: None,
                    comma: Some(TokenStream::from((0, "."))),
                    fract: Some(Digits(TokenStream::from((1, "10")))),
                    exp: Some(Exponent {
                        prefix: TokenStream::from((3, "e")),
                        sign: Some(Sign(TokenStream::from((4, "-")))),
                        digits: Digits(TokenStream::from((5, "10")))
                    }),
                    unit: None
                },
                TokenStream::from((7, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from("10E+10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: Some(Digits(TokenStream::from((0, "10")))),
                    comma: None,
                    fract: None,
                    exp: Some(Exponent {
                        prefix: TokenStream::from((2, "E")),
                        sign: Some(Sign(TokenStream::from((3, "+")))),
                        digits: Digits(TokenStream::from((4, "10")))
                    }),
                    unit: None
                },
                TokenStream::from((6, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from("10ex")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: Some(Digits(TokenStream::from((0, "10")))),
                    comma: None,
                    fract: None,
                    exp: None,
                    unit: Some(Unit::Len(UnitLen(TokenStream::from((2, "ex")))))
                },
                TokenStream::from((4, ""))
            ))
        );
    }

    #[test]
    fn test_num_error() {
        assert_eq!(
            LitNum::parse(TokenStream::from(".e10")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Digits,
                Span { offset: 1, len: 3 }
            )))
        );
    }
}
