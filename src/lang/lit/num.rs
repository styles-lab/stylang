use parserc::{errors::ControlFlow, inputs::lang::LangInput, parser::Parser, syntax::Syntax};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    token::*,
    ty::TypeNum,
};

/// Number sign.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Sign<I>
where
    I: LangInput,
{
    /// `+`
    Plus(TokenPlus<I>),
    /// `-`
    Minus(TokenMinus<I>),
}

/// Exponent part of literial number.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct Exp<I>
where
    I: LangInput,
{
    /// keyword `E` or `e`
    pub token: TokenExp<I>,
    /// number sign part.
    pub sign: Option<Sign<I>>,
    /// digits part.
    pub digits: Digits<I>,
}

/// Unit part of literial number.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Unit<I>
where
    I: LangInput,
{
    Num(Option<TokenUnderscore<I>>, TypeNum<I>),
    Ex(Option<TokenUnderscore<I>>, TokenEx<I>),
    Px(Option<TokenUnderscore<I>>, TokenPx<I>),
    In(Option<TokenUnderscore<I>>, KeywordIn<I>),
    Cm(Option<TokenUnderscore<I>>, TokenCm<I>),
    Mm(Option<TokenUnderscore<I>>, TokenMm<I>),
    Pt(Option<TokenUnderscore<I>>, TokenPt<I>),
    Pc(Option<TokenUnderscore<I>>, TokenPc<I>),
    Percent(TokenUnderscore<I>, TokenPercent<I>),
    Deg(Option<TokenUnderscore<I>>, TokenDeg<I>),
    Grad(Option<TokenUnderscore<I>>, TokenGrad<I>),
    Rad(Option<TokenUnderscore<I>>, TokenRad<I>),
}

/// The literial number parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitNum<I>
where
    I: LangInput,
{
    /// optional sign character: `+` or `-`
    pub sign: Option<Sign<I>>,
    /// optional trunc part.
    pub trunc: Option<Digits<I>>,
    /// optional [S].[S]
    pub dot: Option<TokenDot<I>>,
    /// optional fractional part.
    pub fract: Option<Digits<I>>,
    /// optional exp part.
    pub exp: Option<Exp<I>>,
    /// optional unit part.
    pub unit: Option<Unit<I>>,
}

impl<I> Syntax<I, LangError> for LitNum<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (sign, input) = Sign::into_parser().ok().parse(input)?;
        let (trunc, input) = Digits::into_parser().ok().parse(input)?;

        let (dot, input) = TokenDot::into_parser().ok().parse(input)?;

        let (trunc, dot, fract, input) = match (trunc, dot) {
            (trunc, Some(dot)) => {
                let (fract, input) = Digits::into_parser()
                    .map_err(|_: LangError| LangError::expect(SyntaxKind::Digits, input.span()))
                    .parse(input.clone())?;

                (trunc, Some(dot), Some(fract), input)
            }
            (None, None) => {
                return Err(ControlFlow::Recovable(LangError::expect(
                    SyntaxKind::Digits,
                    input.span(),
                )));
            }
            (trunc, _) => (trunc, None, None, input),
        };

        let (exp, input) = Exp::into_parser().ok().parse(input)?;
        let (unit, input) = Unit::into_parser().ok().parse(input)?;

        Ok((
            Self {
                sign,
                trunc,
                dot,
                fract,
                exp,
                unit,
            },
            input,
        ))
    }
}

/// literial hex integer num: `0x[0-9a-fA-F]+`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct LitHexNum<I>
where
    I: LangInput,
{
    /// The required hex prefix string: `0x`
    pub sign: TokenHexSign<I>,
    /// optional trunc part.
    pub digits: HexDigits<I>,
    /// optional unit part.
    pub unit: Option<Unit<I>>,
}

#[cfg(test)]
mod tests {
    use parserc::inputs::lang::TokenStream;

    use super::*;

    #[test]
    fn with_sign() {
        assert_eq!(
            LitNum::parse(TokenStream::from("+1")),
            Ok((
                LitNum {
                    sign: Some(Sign::Plus(TokenPlus(TokenStream::from("+")))),
                    trunc: Some(Digits(TokenStream::from((1, "1")))),
                    dot: None,
                    fract: None,
                    exp: None,
                    unit: None,
                },
                TokenStream::from((2, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from("+1ex")),
            Ok((
                LitNum {
                    sign: Some(Sign::Plus(TokenPlus(TokenStream::from("+")))),
                    trunc: Some(Digits(TokenStream::from((1, "1")))),
                    dot: None,
                    fract: None,
                    exp: None,
                    unit: Some(Unit::Ex(None, TokenEx(TokenStream::from((2, "ex"))))),
                },
                TokenStream::from((4, ""))
            ))
        );
    }

    #[test]
    fn without_trunc() {
        assert_eq!(
            LitNum::parse(TokenStream::from("+.123e10")),
            Ok((
                LitNum {
                    sign: Some(Sign::Plus(TokenPlus(TokenStream::from("+")))),
                    trunc: None,
                    dot: Some(TokenDot(TokenStream::from((1, ".")))),
                    fract: Some(Digits(TokenStream::from((2, "123")))),
                    exp: Some(Exp {
                        token: TokenExp(TokenStream::from((5, "e"))),
                        sign: None,
                        digits: Digits(TokenStream::from((6, "10")))
                    }),
                    unit: None,
                },
                TokenStream::from((8, ""))
            ))
        );

        assert_eq!(
            LitNum::parse(TokenStream::from(".123e10")),
            Ok((
                LitNum {
                    sign: None,
                    trunc: None,
                    dot: Some(TokenDot(TokenStream::from((0, ".")))),
                    fract: Some(Digits(TokenStream::from((1, "123")))),
                    exp: Some(Exp {
                        token: TokenExp(TokenStream::from((4, "e"))),
                        sign: None,
                        digits: Digits(TokenStream::from((5, "10")))
                    }),
                    unit: None,
                },
                TokenStream::from((7, ""))
            ))
        );
    }
}
