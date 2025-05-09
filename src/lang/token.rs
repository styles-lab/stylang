use parserc::{ControlFlow, Parse, Parser, take_while};

use super::{ParseError, StylangInput, TokenError};

/// token `{`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LeftCurlyBracket<I>(pub I);

impl<I> parserc::Parse<I> for LeftCurlyBracket<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("{")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("{"), input.span())
            })
            .parse(input)
    }
}

/// token `}`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RightCurlyBracket<I>(pub I);

impl<I> parserc::Parse<I> for RightCurlyBracket<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("}")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("}"), input.span())
            })
            .parse(input)
    }
}

/// token `[`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LeftBracket<I>(pub I);

impl<I> parserc::Parse<I> for LeftBracket<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("[")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("["), input.span())
            })
            .parse(input)
    }
}

/// token `]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RightBracket<I>(pub I);

impl<I> parserc::Parse<I> for RightBracket<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("]")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("]"), input.span())
            })
            .parse(input)
    }
}

/// token `(`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LeftParenthesis<I>(pub I);

impl<I> parserc::Parse<I> for LeftParenthesis<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("(")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("("), input.span())
            })
            .parse(input)
    }
}

/// token `]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RightParenthesis<I>(pub I);

impl<I> parserc::Parse<I> for RightParenthesis<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword(")")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword(")"), input.span())
            })
            .parse(input)
    }
}

/// token `]`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct SlashGt<I>(pub I);

impl<I> parserc::Parse<I> for SlashGt<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("/>")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("/>"), input.span())
            })
            .parse(input)
    }
}

/// token `</`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LtSlash<I>(pub I);

impl<I> parserc::Parse<I> for LtSlash<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("</")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("</"), input.span())
            })
            .parse(input)
    }
}
/// `S` characters
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct S<I>(pub I);

impl<I> Parse<I> for S<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (s, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

        if s.is_empty() {
            return Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::S,
                input.span(),
            )));
        }

        Ok((Self(s), input))
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

/// keyword `E` or `e`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct KeywordExp<I>(pub I);

impl<I> parserc::Parse<I> for KeywordExp<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("E")
            .or(parserc::keyword("e"))
            .map(|v| Self(v))
            .parse(input)
    }
}

/// keyword `0x`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct HexSign<I>(pub I);

impl<I> parserc::Parse<I> for HexSign<I>
where
    I: super::StylangInput,
{
    type Error = super::ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        use parserc::{Parser, ParserExt};

        parserc::keyword("0x")
            .map(|v| Self(v))
            .map_err(|input: I, _: parserc::Kind| {
                super::ParseError::Expect(super::TokenError::Keyword("0x"), input.span())
            })
            .parse(input)
    }
}

macro_rules! define_token {
    ($expr: tt => $ident: ident) => {
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: super::StylangInput,
        {
            type Error = super::ParseError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                use parserc::{Parser, ParserExt};

                parserc::keyword(stringify!($expr))
                    .map(|v| Self(v))
                    .map_err(|input: I, _: parserc::Kind| {
                        super::ParseError::Expect(
                            super::TokenError::Keyword(stringify!($expr)),
                            input.span(),
                        )
                    })
                    .parse(input)
            }
        }
    };
}

define_token!(. => Dot);
define_token!(, => Comma);
define_token!(: => Colon);
define_token!(* => Star);
define_token!(:: => PathSep);
define_token!(; => SemiColon);
define_token!(# => NumberSign);
define_token!(+ => Plus);
define_token!(- => Minus);
define_token!(/ => Slash);
define_token!(_ => Underscore);
define_token!(? => QuestionMark);
define_token!(@ => At);
define_token!(= => Eq);
define_token!(< => Lt);
define_token!(> => Gt);
define_token!(^ => Caret);
define_token!(& => And);
define_token!(| => Or);
define_token!(! => Not);
define_token!(~ => Tilde);
define_token!(&& => AndAnd);
define_token!(|| => OrOr);
define_token!(<< => Shl);
define_token!(>> => Shr);
define_token!(-> => ArrowRight);
define_token!(== => EqEq);
define_token!(+= => PlusEq);
define_token!(-= => MinusEq);
define_token!(*= => StarEq);
define_token!(/= => SlashEq);
define_token!(|= => OrEq);
define_token!(%= => PercentEq);
define_token!(^= => CaretEq);
define_token!(&= => AndEq);
define_token!(<<= => ShlEq);
define_token!(>>= => ShrEq);
define_token!(<= => Le);
define_token!(>= => Ge);
define_token!(!= => Ne);
define_token!(.. => DotDot);
define_token!(..= => DotDotEq);
define_token!(fn => KeywordFn);
define_token!(pub => KeywordPub);
define_token!(crate => KeywordCrate);
define_token!(super => KeywordSuper);
define_token!(mod => KeywordMod);
define_token!(use => KeywordUse);
define_token!(data => KeywordData);
define_token!(enum => KeywordEnum);
define_token!(class => KeywordClass);
define_token!(color => KeywordColor);
define_token!(length => KeywordLength);
define_token!(string => KeywordString);
define_token!(angle => KeywordAngle);
define_token!(none => KeywordNone);
define_token!(view => KeywordView);
define_token!(extern => KeywordExtern);
define_token!(let => KeywordLet);
define_token!(if => KeywordIf);
define_token!(else => KeywordElse);
define_token!(elif => KeywordElif);
define_token!(for => KeywordFor);
define_token!(rgb => KeywordRgb);
define_token!(return => KeywordReturn);
define_token!(i8 => I8);
define_token!(i16 => I16);
define_token!(i32 => I32);
define_token!(i64 => I64);
define_token!(i128 => I128);
define_token!(u8 => U8);
define_token!(u16 => U16);
define_token!(u32 => U32);
define_token!(u64 => U64);
define_token!(u128 => U128);
define_token!(bigint => BigInt);
define_token!(f32 => F32);
define_token!(f64 => F64);
define_token!(bignum => BigNum);
define_token!(ex => Ex);
define_token!(px => Px);
define_token!(in => In);
define_token!(cm => Cm);
define_token!(mm => Mm);
define_token!(pt => Pt);
define_token!(pc => Pc);
define_token!(% => Percent);
define_token!(deg => Deg);
define_token!(grad => Grad);
define_token!(rad => Rad);
