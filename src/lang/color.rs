use parserc::derive_parse;

use super::{
    Comma, Digits, HexDigits, KeywordRgb, LeftParenthesis, NumberSign, ParseError, Percent,
    RightParenthesis, S, StylangInput,
};

/// Hex color: `#fff`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct HexColor<I>
where
    I: StylangInput,
{
    pub keyword: NumberSign<I>,
    pub digits: HexDigits<I>,
}

/// rgb(xxx,xxx,xx)
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct Rgb<I>
where
    I: StylangInput,
{
    pub keyword: KeywordRgb<I>,
    pub s1: Option<S<I>>,
    pub delimiter_start: LeftParenthesis<I>,
    pub s2: Option<S<I>>,
    pub r: Digits<I>,
    pub r_percent: Option<Percent<I>>,
    pub s3: Option<S<I>>,
    pub comma1: Comma<I>,
    pub s4: Option<S<I>>,
    pub g: Digits<I>,
    pub g_percent: Option<Percent<I>>,
    pub s5: Option<S<I>>,
    pub comma2: Comma<I>,
    pub s6: Option<S<I>>,
    pub b: Digits<I>,
    pub b_percent: Option<Percent<I>>,
    pub s7: Option<S<I>>,
    pub delimiter_end: RightParenthesis<I>,
}

/// A literial color expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum LitColor<I>
where
    I: StylangInput,
{
    Hex(HexColor<I>),
    Rgb(Rgb<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Comma, Digits, HexDigits, KeywordRgb, LeftParenthesis, NumberSign, Percent,
        RightParenthesis, S, TokenStream,
    };

    use super::{HexColor, Rgb};

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
                    keyword: KeywordRgb(TokenStream::from("rgb")),
                    s1: None,
                    delimiter_start: LeftParenthesis(TokenStream::from((3, "("))),
                    s2: None,
                    r: Digits(TokenStream::from((4, "255"))),
                    r_percent: None,
                    s3: None,
                    comma1: Comma(TokenStream::from((7, ","))),
                    s4: Some(S(TokenStream::from((8, " ")))),
                    g: Digits(TokenStream::from((9, "255"))),
                    g_percent: None,
                    s5: None,
                    comma2: Comma(TokenStream::from((12, ","))),
                    s6: Some(S(TokenStream::from((13, " ")))),
                    b: Digits(TokenStream::from((14, "255"))),
                    b_percent: None,
                    s7: None,
                    delimiter_end: RightParenthesis(TokenStream::from((17, ")")))
                },
                TokenStream::from((18, ""))
            ))
        );

        assert_eq!(
            Rgb::parse(TokenStream::from("rgb(255%, 255%, 255%)")),
            Ok((
                Rgb {
                    keyword: KeywordRgb(TokenStream::from("rgb")),
                    s1: None,
                    delimiter_start: LeftParenthesis(TokenStream::from((3, "("))),
                    s2: None,
                    r: Digits(TokenStream::from((4, "255"))),
                    r_percent: Some(Percent(TokenStream::from((7, "%")))),
                    s3: None,
                    comma1: Comma(TokenStream::from((8, ","))),
                    s4: Some(S(TokenStream::from((9, " ")))),
                    g: Digits(TokenStream::from((10, "255"))),
                    g_percent: Some(Percent(TokenStream::from((13, "%")))),
                    s5: None,
                    comma2: Comma(TokenStream::from((14, ","))),
                    s6: Some(S(TokenStream::from((15, " ")))),
                    b: Digits(TokenStream::from((16, "255"))),
                    b_percent: Some(Percent(TokenStream::from((19, "%")))),
                    s7: None,
                    delimiter_end: RightParenthesis(TokenStream::from((20, ")")))
                },
                TokenStream::from((21, ""))
            ))
        );
    }
}
