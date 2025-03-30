use parserc::{ControlFlow, Parse, Parser, ParserExt, next, take_while};

use super::{ParseError, StylangInput, Token};

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

/// Sign character used by [`LitInt`] or [`LitNum`].
#[derive(Debug, PartialEq, PartialOrd, Clone)]
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
pub struct Exp<I> {
    /// exp prefix, `E` or `e`
    prefix: I,
    /// sign character: `+` or `-`
    sign: Option<Sign<I>>,
    /// digits part.
    digits: Digits<I>,
}

impl<I> Parse<I> for Exp<I>
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
pub struct LitNum<I> {
    /// optional sign character: `+` or `-`
    sign: Option<Sign<I>>,
    /// optional trunc part.
    trunc: Option<Digits<I>>,
    /// optional [S].[S]
    comma: Option<I>,
    /// optional fractional part.
    fract: Option<Digits<I>>,
    /// optional exp part.
    exp: Option<Exp<I>>,
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
        let (exp, input) = Exp::into_parser().ok().parse(input)?;

        Ok((
            Self {
                sign,
                trunc,
                comma,
                fract,
                exp,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Digits, Exp, LitNum, Sign, TokenStream};

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
                    exp: None
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
                    exp: None
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
                    exp: None
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
                    exp: None
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
                    exp: Some(Exp {
                        prefix: TokenStream::from((3, "e")),
                        sign: Some(Sign(TokenStream::from((4, "-")))),
                        digits: Digits(TokenStream::from((5, "10")))
                    }),
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
                    exp: Some(Exp {
                        prefix: TokenStream::from((2, "E")),
                        sign: Some(Sign(TokenStream::from((3, "+")))),
                        digits: Digits(TokenStream::from((4, "10")))
                    }),
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
                },
                TokenStream::from((2, "ex"))
            ))
        );
    }
}
