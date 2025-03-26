use parserc::{Kind, Parse, Parser, ParserExt, keyword, next, take_till, take_while};

use crate::lang::Token;

use super::{ParseError, TokenStream};

/// literal string value, be like: `"...\"... "`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LitStr<I>(pub I);

impl<I> Parse<I> for LitStr<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, mut input) = next(b'"').parse(input)?;

        let mut content = input.clone();

        loop {
            let seg;
            (seg, input) = take_till(|c: u8| c == b'"').parse(input)?;

            if seg.len() == 0 {
                break;
            }

            if *seg.as_bytes().last().unwrap() != b'\\' {
                break;
            }

            input.split_to(1);
        }

        content.split_off(input.start() - content.start());

        input.split_to(1);

        Ok((LitStr(content), input))
    }
}

/// signed/unsigned decimal value.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Decimal<I> {
    /// optional sign part
    sign: Option<I>,
    /// digits part of the decimal value.
    digits: I,
}

impl<I> Parse<I> for Decimal<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (sign, input) = keyword("+").or(keyword("-")).ok().parse(input)?;

        let (digits, input) = take_while(|c: u8| c.is_ascii_digit()).parse(input)?;

        if digits.is_empty() {
            return Err(parserc::ControlFlow::Recovable(ParseError::Expect(
                Token::Digits,
                input.span(),
            )));
        }

        Ok((Decimal { sign, digits }, input))
    }
}

/// Exp part of f32/f64.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Exponent<I> {
    /// prefix part, be like: `E` or `e`
    e_token: I,
    /// value part.
    decimal: Decimal<I>,
}

impl<I> Parse<I> for Exponent<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (e_token, input) = next(b'E')
            .or(next(b'e'))
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::ExpToken, input.span()))
            .parse(input)?;

        let (decimal, input) = Decimal::into_parser()
            .map_err(|input: I, _| ParseError::Expect(Token::ExpDigits, input.span()))
            .parse(input)?;

        Ok((Exponent { e_token, decimal }, input))
    }
}

/// literal signed/unsigned decimal value with optional type label.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LitInt<I> {
    /// required value part.
    decimal: Decimal<I>,
    /// optional type label,
    ty_label: Option<I>,
}

impl<I> Parse<I> for LitInt<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (value, input) = Decimal::parse(input)?;

        let (ty_label, input) = keyword("i8")
            .or(keyword("i16"))
            .or(keyword("i32"))
            .or(keyword("i64"))
            .or(keyword("i128"))
            .or(keyword("u8"))
            .or(keyword("u16"))
            .or(keyword("u32"))
            .or(keyword("u64"))
            .or(keyword("u128"))
            .ok()
            .parse(input)?;

        Ok((
            Self {
                decimal: value,
                ty_label,
            },
            input,
        ))
    }
}

/// literal signed/unsigned f32/f64 value.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LitNum<I> {
    /// optional sign part
    sign: Option<I>,
    /// integer part
    integer: I,
    /// optional `.`
    dot: Option<I>,
    /// optional fractional part.
    fractional: Option<I>,
    /// optional type label,
    type_label: Option<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{Decimal, Exponent, LitInt, LitStr, ParseError, Source, Token};

    #[test]
    fn test_lit_str() {
        assert_eq!(
            LitStr::parse(Source::from(r#""""#)),
            Ok((LitStr(Source::from((1, ""))), Source::from((2, ""))))
        );

        assert_eq!(
            LitStr::parse(Source::from(r#""helo world""#)),
            Ok((
                LitStr(Source::from((1, "helo world"))),
                Source::from((12, ""))
            ))
        );

        assert_eq!(
            LitStr::parse(Source::from(r#""helo \" world""#)),
            Ok((
                LitStr(Source::from((1, r#"helo \" world"#))),
                Source::from((15, ""))
            ))
        );
    }

    #[test]
    fn parse_decimal() {
        assert_eq!(
            Decimal::parse(Source::from("123")),
            Ok((
                Decimal {
                    sign: None,
                    digits: Source::from("123")
                },
                Source::from((3, ""))
            ))
        );

        assert_eq!(
            Decimal::parse(Source::from("+123")),
            Ok((
                Decimal {
                    sign: Some(Source::from("+")),
                    digits: Source::from((1, "123"))
                },
                Source::from((4, ""))
            ))
        );

        assert_eq!(
            Decimal::parse(Source::from("- 123")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Digits,
                Span { offset: 1, len: 4 }
            )))
        );
    }

    #[test]
    fn parse_exponent() {
        assert_eq!(
            Exponent::parse(Source::from("E+123")),
            Ok((
                Exponent {
                    e_token: Source::from("E"),
                    decimal: Decimal {
                        sign: Some(Source::from((1, "+"))),
                        digits: Source::from((2, "123"))
                    },
                },
                Source::from((5, ""))
            ))
        );

        assert_eq!(
            Exponent::parse(Source::from("e123")),
            Ok((
                Exponent {
                    e_token: Source::from("e"),
                    decimal: Decimal {
                        sign: None,
                        digits: Source::from((1, "123"))
                    },
                },
                Source::from((4, ""))
            ))
        );

        assert_eq!(
            Exponent::parse(Source::from("-123")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::ExpToken,
                Span { offset: 0, len: 4 }
            )))
        );

        assert_eq!(
            Exponent::parse(Source::from("e -123")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::ExpDigits,
                Span { offset: 1, len: 5 }
            )))
        );
    }

    #[test]
    fn parse_lit_int() {
        assert_eq!(
            LitInt::parse(Source::from("+123")),
            Ok((
                LitInt {
                    decimal: Decimal {
                        sign: Some(Source::from("+")),
                        digits: Source::from((1, "123"))
                    },
                    ty_label: None,
                },
                Source::from((4, ""))
            ))
        );

        assert_eq!(
            LitInt::parse(Source::from("1234u8")),
            Ok((
                LitInt {
                    decimal: Decimal {
                        sign: None,
                        digits: Source::from((0, "1234"))
                    },
                    ty_label: Some(Source::from((4, "u8"))),
                },
                Source::from((6, ""))
            ))
        );

        assert_eq!(
            LitInt::parse(Source::from("1234 u8")),
            Ok((
                LitInt {
                    decimal: Decimal {
                        sign: None,
                        digits: Source::from((0, "1234"))
                    },
                    ty_label: None,
                },
                Source::from((4, " u8"))
            ))
        );
    }
}
