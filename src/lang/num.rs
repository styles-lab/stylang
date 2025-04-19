use parserc::derive_parse;

use super::*;

/// Number sign.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Sign<I>
where
    I: StylangInput,
{
    Plus(Plus<I>),
    Minus(Minus<I>),
}

/// Exponent part of number.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct Exp<I>
where
    I: StylangInput,
{
    /// keyword `E` or `e`
    pub token: KeywordExp<I>,
    /// number sign part.
    pub sign: Option<Sign<I>>,
    /// digits part.
    pub digits: Digits<I>,
}

/// unit part of number.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum NumUnit<I>
where
    I: StylangInput,
{
    I8(I8<I>),
    I16(I16<I>),
    I32(I32<I>),
    I64(I64<I>),
    I128(I128<I>),
    U8(U8<I>),
    U16(U16<I>),
    U32(U32<I>),
    U64(U64<I>),
    U128(U128<I>),
    BigInt(BigInt<I>),
    BigNum(BigNum<I>),
    Ex(Ex<I>),
    Px(Px<I>),
    In(In<I>),
    Cm(Cm<I>),
    Mm(Mm<I>),
    Pt(Pt<I>),
    Pc(Pc<I>),
    Percent(Percent<I>),
    Deg(Deg<I>),
    Grad(Grad<I>),
    Rad(Rad<I>),
}

/// unit part of number.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct LitNum<I>
where
    I: StylangInput,
{
    /// optional sign character: `+` or `-`
    pub sign: Option<Sign<I>>,
    /// optional trunc part.
    pub trunc: Option<Digits<I>>,
    /// optional [S].[S]
    pub dot: Option<Dot<I>>,
    /// optional fractional part.
    pub fract: Option<Digits<I>>,
    /// optional exp part.
    pub exp: Option<Exp<I>>,
    /// optional unit part.
    pub unit: Option<NumUnit<I>>,
}
/// unit suffix of literial hex number, be like: `_ex`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct HexNumUnit<I>(pub(crate) Underscore<I>, pub NumUnit<I>)
where
    I: StylangInput;
/// literial hex integer num: `0x[0-9a-fA-F]+`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct LitHexNum<I>
where
    I: StylangInput,
{
    /// The required hex prefix string: `0x`
    pub sign: HexSign<I>,
    /// optional trunc part.
    pub digits: HexDigits<I>,
    /// optional unit part.
    pub unit: Option<HexNumUnit<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Digits, Dot, Ex, Exp, HexDigits, HexNumUnit, HexSign, KeywordExp, LitHexNum, LitNum, Minus,
        NumUnit, Percent, Plus, TokenStream, Underscore,
    };

    use super::Sign;

    #[test]
    fn test_sign() {
        assert_eq!(
            Sign::parse(TokenStream::from("+")),
            Ok((
                Sign::Plus(Plus(TokenStream::from("+"))),
                TokenStream::from((1, ""))
            ))
        );

        assert_eq!(
            Sign::parse(TokenStream::from("-")),
            Ok((
                Sign::Minus(Minus(TokenStream::from("-"))),
                TokenStream::from((1, ""))
            ))
        );
    }

    #[test]
    fn test_exp() {
        assert_eq!(
            Exp::parse(TokenStream::from("E-10")),
            Ok((
                Exp {
                    token: KeywordExp(TokenStream::from("E")),
                    sign: Some(Sign::Minus(Minus(TokenStream::from((1, "-"))))),
                    digits: Digits(TokenStream::from((2, "10")))
                },
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            Exp::parse(TokenStream::from("e10")),
            Ok((
                Exp {
                    token: KeywordExp(TokenStream::from("e")),
                    sign: None,
                    digits: Digits(TokenStream::from((1, "10")))
                },
                TokenStream::from((3, ""))
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
                    dot: None,
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
                    dot: Some(Dot(TokenStream::from((1, ".")))),
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
                    dot: Some(Dot(TokenStream::from((0, ".")))),
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
                    dot: Some(Dot(TokenStream::from((0, ".")))),
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
                    dot: Some(Dot(TokenStream::from((0, ".")))),
                    fract: Some(Digits(TokenStream::from((1, "10")))),
                    exp: Some(Exp {
                        token: KeywordExp(TokenStream::from((3, "e"))),
                        sign: Some(Sign::Minus(Minus(TokenStream::from((4, "-"))))),
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
                    dot: None,
                    fract: None,
                    exp: Some(Exp {
                        token: KeywordExp(TokenStream::from((2, "E"))),
                        sign: Some(Sign::Plus(Plus(TokenStream::from((3, "+"))))),
                        digits: Digits(TokenStream::from((4, "10")))
                    }),
                    unit: None
                },
                TokenStream::from((6, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from("10%")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: Some(Digits(TokenStream::from((0, "10")))),
                    dot: None,
                    fract: None,
                    exp: None,
                    unit: Some(NumUnit::Percent(Percent(TokenStream::from((2, "%")))))
                },
                TokenStream::from((3, ""))
            ))
        );
    }

    #[test]
    fn test_lit_hex_num() {
        assert_eq!(
            LitHexNum::parse(TokenStream::from("0xf0a0B0")),
            Ok((
                LitHexNum {
                    sign: HexSign(TokenStream::from("0x")),
                    digits: HexDigits(TokenStream::from((2, "f0a0B0"))),
                    unit: None
                },
                TokenStream::from((8, ""))
            ))
        );

        assert_eq!(
            LitHexNum::parse(TokenStream::from("0xf0a0B0_ex")),
            Ok((
                LitHexNum {
                    sign: HexSign(TokenStream::from("0x")),
                    digits: HexDigits(TokenStream::from((2, "f0a0B0"))),
                    unit: Some(HexNumUnit(
                        Underscore(TokenStream::from((8, "_"))),
                        NumUnit::Ex(Ex(TokenStream::from((9, "ex"))))
                    ))
                },
                TokenStream::from((11, ""))
            ))
        );
    }
}
