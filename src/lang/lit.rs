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

/// sign part of LitNum/Exponent.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Sign<I>(I);

impl<I> Parse<I> for Sign<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (sign, input) = next(b'+').or(next(b'-')).parse(input)?;

        Ok((Self(sign), input))
    }
}

/// signed/unsigned decimal value.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Digits<I>(pub I);

impl<I> Parse<I> for Digits<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (digits, input) = take_while(|c: u8| c.is_ascii_digit()).parse(input)?;

        if digits.is_empty() {
            return Err(parserc::ControlFlow::Recovable(ParseError::Expect(
                Token::Digits,
                input.span(),
            )));
        }

        Ok((Self(digits), input))
    }
}

/// Exp part of f32/f64.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Exp<I> {
    /// prefix part, be like: `E` or `e`
    e_token: I,
    /// optional sign part.
    sign: Option<Sign<I>>,
    /// digits part.
    digits: Digits<I>,
}

impl<I> Parse<I> for Exp<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (e_token, input) = next(b'E')
            .or(next(b'e'))
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::ExpToken, input.span()))
            .parse(input)?;

        let (sign, input) = Sign::into_parser().ok().parse(input)?;

        let (digits, input) = Digits::into_parser()
            .map_err(|input: I, _| ParseError::Expect(Token::ExpDigits, input.span()))
            .parse(input)?;

        Ok((
            Exp {
                e_token,
                sign,
                digits,
            },
            input,
        ))
    }
}

/// literal signed/unsigned f32/f64 value.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LitNum<I> {
    /// optional sign part.
    sign: Option<Sign<I>>,
    /// required integer part.
    integer: Option<Digits<I>>,
    /// optional `.`
    dot: Option<I>,
    /// optional fractional part.
    fractional: Option<I>,
    /// optional exp part.
    exp: Option<Exp<I>>,
    /// optional type label,
    ty_label: Option<I>,
}

impl<I> Parse<I> for LitNum<I>
where
    I: TokenStream,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (sign, input) = Sign::into_parser().ok().parse(input)?;

        let (integer, input) = Digits::into_parser()
            .ok()
            .map_err(|input: I, _| ParseError::Expect(Token::Digits, input.span()))
            .parse(input)?;

        let (dot, input) = next(b'.').ok().parse(input)?;

        let (fractional, input) = next(b'.').ok().parse(input)?;

        let (exp, input) = Exp::into_parser().ok().parse(input)?;

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
            .or(keyword("bigint"))
            .or(keyword("bignum"))
            .ok()
            .parse(input)?;

        Ok((
            LitNum {
                sign,
                integer,
                dot,
                fractional,
                exp,
                ty_label,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Digits, LitNum, LitStr, Source};

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
    fn parse_lit_num() {
        assert_eq!(
            LitNum::parse(Source::from("123")),
            Ok((
                LitNum {
                    sign: None,
                    integer: Some(Digits(Source::from("123"))),
                    dot: None,
                    fractional: None,
                    exp: None,
                    ty_label: None
                },
                Source::from((3, ""))
            ))
        );
    }
}
