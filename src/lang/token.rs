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

define_token!(, => Comma);
define_token!(: => Colon);
define_token!(; => SemiColon);
define_token!(-> => ArrowRight);
define_token!(fn => KeywordFn);
define_token!(data => KeywordData);
define_token!(enum => KeywordEnum);
define_token!(class => KeywordClass);
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
