use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    input::LangInput,
    token::{Digits, HexDigits, SepComma, SepLeftParen, SepRightParen, TokenNumSign, TokenRgb},
};

/// Hex color: `#fff` or `#f0a010`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct HexColor<I>
where
    I: LangInput,
{
    pub num_sign_token: TokenNumSign<I>,
    pub digits: HexDigits<I>,
}

impl<I> Parse<I> for HexColor<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (num_sign_token, input) = TokenNumSign::parse(input)?;

        let (digits, input) = HexDigits::into_parser().fatal().parse(input)?;

        match digits.0.len() {
            3 | 6 => {}
            _ => {
                return Err(ControlFlow::Fatal(LangError::invalid(
                    TokenKind::HexColor,
                    digits.0.span(),
                )));
            }
        }

        Ok((
            Self {
                num_sign_token,
                digits,
            },
            input,
        ))
    }
}

/// Literial rgb color: `rgb(255,100,20)`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RgbColor<I>
where
    I: LangInput,
{
    /// token `rgb`
    pub rgb_token: TokenRgb<I>,
    /// delimiter start token: `(`
    pub delimiter_start: SepLeftParen<I>,
    /// red component value.
    pub red: Digits<I>,
    /// green component value.
    pub green: (SepComma<I>, Digits<I>),
    /// blue component value.
    pub blue: (SepComma<I>, Digits<I>),
    /// delimiter end token: `)`
    pub delimiter_end: SepRightParen<I>,
}

impl<I> RgbColor<I>
where
    I: LangInput,
{
    fn check_digits(digits: &I) -> Result<(), ControlFlow<LangError>> {
        match usize::from_str_radix(digits.as_str(), 10) {
            Ok(v) if v > 255 => Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::RgbDigits,
                digits.span(),
            ))),
            Err(_) => Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::RgbDigits,
                digits.span(),
            ))),
            Ok(_) => Ok(()),
        }
    }
}

impl<I> Parse<I> for RgbColor<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (rgb_token, input) = TokenRgb::parse(input)?;
        let (delimiter_start, input) = SepLeftParen::into_parser().fatal().parse(input)?;

        let (red, input) = Digits::into_parser().fatal().parse(input)?;

        Self::check_digits(&red.0)?;

        let (green, input) = <(SepComma<_>, Digits<_>)>::into_parser()
            .fatal()
            .parse(input)?;

        Self::check_digits(&green.1.0)?;

        let (blue, input) = <(SepComma<_>, Digits<_>)>::into_parser()
            .fatal()
            .parse(input)?;

        Self::check_digits(&blue.1.0)?;

        let (delimiter_end, input) = SepRightParen::into_parser().fatal().parse(input)?;

        Ok((
            Self {
                rgb_token,
                delimiter_start,
                red,
                green,
                blue,
                delimiter_end,
            },
            input,
        ))
    }
}

/// A literial color expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum LitColor<I>
where
    I: LangInput,
{
    Hex(HexColor<I>),
    Rgb(RgbColor<I>),
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::{LangError, TokenKind},
        input::TokenStream,
        lit::{HexColor, RgbColor},
        token::{
            Digits, HexDigits, S, SepComma, SepLeftParen, SepRightParen, TokenNumSign, TokenRgb,
        },
    };

    #[test]
    fn hex_color() {
        assert_eq!(
            HexColor::parse(TokenStream::from("#fff")),
            Ok((
                HexColor {
                    num_sign_token: TokenNumSign(TokenStream::from("#")),
                    digits: HexDigits(TokenStream::from((1, "fff")))
                },
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            HexColor::parse(TokenStream::from("#fff000")),
            Ok((
                HexColor {
                    num_sign_token: TokenNumSign(TokenStream::from("#")),
                    digits: HexDigits(TokenStream::from((1, "fff000")))
                },
                TokenStream::from((7, ""))
            ))
        );
    }

    #[test]
    fn invalid_hex_color_len() {
        assert_eq!(
            HexColor::parse(TokenStream::from("#ff")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::HexColor,
                Span { offset: 1, len: 2 }
            )))
        );

        assert_eq!(
            HexColor::parse(TokenStream::from("#ffff")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::HexColor,
                Span { offset: 1, len: 4 }
            )))
        );

        assert_eq!(
            HexColor::parse(TokenStream::from("#fffffff")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::HexColor,
                Span { offset: 1, len: 7 }
            )))
        );
    }

    #[test]
    fn rgb_color() {
        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb (1,1,1)")),
            Ok((
                RgbColor {
                    rgb_token: TokenRgb(TokenStream::from("rgb")),
                    delimiter_start: SepLeftParen(
                        Some(S(TokenStream::from((3, " ")))),
                        TokenStream::from((4, "(")),
                        None
                    ),
                    red: Digits(TokenStream::from((5, "1"))),
                    green: (
                        SepComma(None, TokenStream::from((6, ",")), None),
                        Digits(TokenStream::from((7, "1")))
                    ),
                    blue: (
                        SepComma(None, TokenStream::from((8, ",")), None),
                        Digits(TokenStream::from((9, "1")))
                    ),
                    delimiter_end: SepRightParen(None, TokenStream::from((10, ")")), None)
                },
                TokenStream::from((11, ""))
            ))
        );
    }

    #[test]
    fn rgb_color_out_of_range() {
        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb(1000,1,1)")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::RgbDigits,
                Span { offset: 4, len: 4 }
            )))
        );

        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb(1,256,1)")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::RgbDigits,
                Span { offset: 6, len: 3 }
            )))
        );

        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb(1,255,256)")),
            Err(ControlFlow::Fatal(LangError::invalid(
                TokenKind::RgbDigits,
                Span { offset: 10, len: 3 }
            )))
        );
    }

    #[test]
    fn rgb_delimiter() {
        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb(1,255,255")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token(")"),
                Span { offset: 13, len: 0 }
            )))
        );

        assert_eq!(
            RgbColor::parse(TokenStream::from("rgb(1,255)")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token(","),
                Span { offset: 9, len: 1 }
            )))
        );
    }
}
