use parserc::derive_parse;

use crate::lang::{errors::LangError, inputs::LangInput, tokens::*};

/// Hex color: `#fff`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct HexColor<I>
where
    I: LangInput,
{
    pub keyword: NumberSign<I>,
    pub digits: HexDigits<I>,
}

/// rgb(xxx,xxx,xx)
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Rgb<I>
where
    I: LangInput,
{
    pub keyword: (KeywordRgb<I>, Option<S<I>>),
    pub delimiter_start: LeftParenthesis<I>,
    pub r: (Option<S<I>>, Digits<I>, Option<Percent<I>>),
    pub comma1: (Option<S<I>>, Comma<I>, Option<S<I>>),
    pub g: (Digits<I>, Option<Percent<I>>),
    pub comma2: (Option<S<I>>, Comma<I>, Option<S<I>>),
    pub b: (Digits<I>, Option<Percent<I>>, Option<S<I>>),
    pub delimiter_end: RightParenthesis<I>,
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
    Rgb(Rgb<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::inputs::TokenStream;

    use super::*;

    #[test]
    fn test_hex_color() {
        assert_eq!(
            HexColor::parse(TokenStream::from("#fff")),
            Ok((
                HexColor {
                    keyword: NumberSign(TokenStream::from("#")),
                    digits: HexDigits(TokenStream::from((1, "fff")))
                },
                TokenStream::from((4, ""))
            ))
        );
    }

    #[test]
    fn test_rgb() {
        assert_eq!(
            Rgb::parse(TokenStream::from("rgb(255, 255, 255)")),
            Ok((
                Rgb {
                    keyword: (KeywordRgb(TokenStream::from("rgb")), None),
                    delimiter_start: LeftParenthesis(TokenStream::from((3, "("))),
                    r: (None, Digits(TokenStream::from((4, "255"))), None),
                    comma1: (
                        None,
                        Comma(TokenStream::from((7, ","))),
                        Some(S(TokenStream::from((8, " "))))
                    ),

                    g: (Digits(TokenStream::from((9, "255"))), None),
                    comma2: (
                        None,
                        Comma(TokenStream::from((12, ","))),
                        Some(S(TokenStream::from((13, " "))))
                    ),

                    b: (Digits(TokenStream::from((14, "255"))), None, None),
                    delimiter_end: RightParenthesis(TokenStream::from((17, ")")))
                },
                TokenStream::from((18, ""))
            ))
        );

        assert_eq!(
            Rgb::parse(TokenStream::from("rgb(255%, 255%, 255%)")),
            Ok((
                Rgb {
                    keyword: (KeywordRgb(TokenStream::from("rgb")), None),
                    delimiter_start: LeftParenthesis(TokenStream::from((3, "("))),
                    r: (
                        None,
                        Digits(TokenStream::from((4, "255"))),
                        Some(Percent(TokenStream::from((7, "%"))))
                    ),

                    comma1: (
                        None,
                        Comma(TokenStream::from((8, ","))),
                        Some(S(TokenStream::from((9, " "))))
                    ),
                    g: (
                        Digits(TokenStream::from((10, "255"))),
                        Some(Percent(TokenStream::from((13, "%"))))
                    ),

                    comma2: (
                        None,
                        Comma(TokenStream::from((14, ","))),
                        Some(S(TokenStream::from((15, " "))))
                    ),

                    b: (
                        Digits(TokenStream::from((16, "255"))),
                        Some(Percent(TokenStream::from((19, "%")))),
                        None
                    ),

                    delimiter_end: RightParenthesis(TokenStream::from((20, ")")))
                },
                TokenStream::from((21, ""))
            ))
        );
    }
}
