//! Token types for stylang.
//!

use parserc::{ControlFlow, Delimiter, Kind, Parse, Parser, ParserExt, satisfy, take_while};

use crate::lang::errors::{LangError, TokenKind};
use crate::lang::input::LangInput;

/// Sequence of `whitespace` chars
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct S<I>(pub I);

impl<I> Parse<I> for S<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (s, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

        if s.is_empty() {
            return Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::S,
                input.span(),
            )));
        }

        Ok((Self(s), input))
    }
}

/// `stylang` type name token.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Ident<I>(pub I);

impl<I> Parse<I> for Ident<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|input: I, _: Kind| LangError::expect(TokenKind::Ident, input.span()))
            .parse(input)?;

        let (_, input) = take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

/// Xml elem name token.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlIdent<I>(pub I);

impl<I> Parse<I> for XmlIdent<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = satisfy(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|input: I, _: Kind| LangError::expect(TokenKind::XmlIdent, input.span()))
            .parse(input)?;

        let (_, input) =
            take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'-').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

macro_rules! keyword {
    ($expr: literal => $ident: ident) => {
        /// Keyword: `
        #[doc=stringify!($expr)]
        /// `
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I, pub S<I>);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: LangInput,
        {
            type Error = LangError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                let (kw, input) = parserc::keyword($expr)
                    .map_err(|input: I, _: parserc::Kind| {
                        LangError::expect(TokenKind::Token($expr), input.span())
                    })
                    .parse(input)?;

                let (tail, input) = S::parse(input)?;

                Ok((Self(kw, tail), input))
            }
        }
    };
}

macro_rules! token {
    ($expr: literal => $ident: ident) => {
        /// Token: `
        #[doc=stringify!($expr)]
        /// `
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: LangInput,
        {
            type Error = LangError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                let (kw, input) = parserc::keyword($expr)
                    .map_err(|input: I, _: parserc::Kind| {
                        LangError::expect(TokenKind::Token($expr), input.span())
                    })
                    .parse(input)?;

                Ok((Self(kw), input))
            }
        }
    };
}

macro_rules! token_lookahead {
    ($lookahead: expr, $expr: literal => $ident: ident) => {
        /// Token: `
        #[doc=stringify!($expr)]
        /// `
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub I);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: LangInput,
        {
            type Error = LangError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                let (None, input) = $lookahead.ok().parse(input.clone())? else {
                    return Err(parserc::ControlFlow::Recovable(LangError::expect(
                        TokenKind::Token($expr),
                        input.span(),
                    )));
                };

                let (kw, input) = parserc::keyword($expr)
                    .map_err(|input: I, _: parserc::Kind| {
                        LangError::expect(TokenKind::Token($expr), input.span())
                    })
                    .parse(input)?;

                Ok((Self(kw), input))
            }
        }
    };
}

macro_rules! sep {
    ($expr: literal => $ident: ident) => {
        /// sep: `
        #[doc=stringify!($expr)]
        /// `
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub Option<S<I>>, pub I, pub Option<S<I>>);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: LangInput,
        {
            type Error = LangError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                let (leading, input) = S::into_parser().ok().parse(input)?;

                let (kw, input) = parserc::keyword($expr)
                    .map_err(|input: I, _: parserc::Kind| {
                        LangError::expect(TokenKind::Token($expr), input.span())
                    })
                    .parse(input)?;

                let (tail, input) = S::into_parser().ok().parse(input)?;

                Ok((Self(leading, kw, tail), input))
            }
        }
    };
}

macro_rules! sep_lookahead {
    ($lookahead: expr, $expr: literal => $ident: ident) => {
        /// sep: `
        #[doc=stringify!($expr)]
        /// `
        #[derive(Debug, PartialEq, Clone)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
        pub struct $ident<I>(pub Option<S<I>>, pub I, pub Option<S<I>>);

        impl<I> parserc::Parse<I> for $ident<I>
        where
            I: LangInput,
        {
            type Error = LangError;

            fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
                let (leading, input) = S::into_parser().ok().parse(input)?;

                let (None, input) = $lookahead.ok().parse(input.clone())? else {
                    return Err(parserc::ControlFlow::Recovable(LangError::expect(
                        TokenKind::Token($expr),
                        input.span(),
                    )));
                };

                let (kw, input) = parserc::keyword($expr)
                    .map_err(|input: I, _: parserc::Kind| {
                        LangError::expect(TokenKind::Token($expr), input.span())
                    })
                    .parse(input)?;

                let (tail, input) = S::into_parser().ok().parse(input)?;

                Ok((Self(leading, kw, tail), input))
            }
        }
    };
}

keyword!("fn" => KeywordFn);
keyword!("view" => KeywordView);
keyword!("enum" => KeywordEnum);
keyword!("data" => KeywordData);
keyword!("class" => KeywordClass);
keyword!("pub" => KeywordPub);
keyword!("mod" => KeywordMod);
keyword!("use" => KeywordUse);
keyword!("extern" => KeywordExtern);
keyword!("let" => KeywordLet);
keyword!("for" => KeywordFor);
keyword!("in" => KeywordIn);
keyword!("loop" => KeywordLoop);
keyword!("while" => KeywordWhile);
keyword!("match" => KeywordMatch);

token!("fn" => TokenFn);
token!("crate" => TokenCrate);
token!("super" => TokenSuper);
token!("view" => TokenView);
token!("enum" => TokenEnum);
token!("data" => TokenData);
token!("class" => TokenClass);
token!("color" => TokenColor);
token!("length" => TokenLength);
token!("string" => TokenString);
token!("angle" => TokenAngle);
token!("#" => TokenNumSign);
token!("_" => TokenUnderscore);
token!("bool" => TokenBool);
token!("true" => TokenTrue);
token!("false" => TokenFalse);
token!("return" => TokenReturn);
token!("break" => TokenBreak);
token!("continue" => TokenContinue);
token!("i8" => TokenI8);
token!("i16" => TokenI16);
token!("i32" => TokenI32);
token!("i64" => TokenI64);
token!("i128" => TokenI128);
token!("u8" => TokenU8);
token!("u16" => TokenU16);
token!("u32" => TokenU32);
token!("u64" => TokenU64);
token!("u128" => TokenU128);
token!("f32" => TokenF32);
token!("f64" => TokenF64);

token!("<=" => TokenLe);
token!(">=" => TokenGe);
token!("</" => TokenLtSlash);
token!("/>" => TokenSlashGt);
token!("!=" => TokenNotEq);
token!("==" => TokenEqEq);
token!("-=" => TokenMinusEq);

token_lookahead!(
    SepAtAt::into_parser(),
    "@" => TokenAt
);

token_lookahead!(
    SepArrowRight::into_parser().map(|_|())
        .or(TokenMinusEq::into_parser().map(|_|())),
    "-" => TokenMinus
);

token_lookahead!(
    TokenEqEq::into_parser(),
    "=" => TokenEq
);

token_lookahead!(
    TokenNotEq::into_parser(),
    "!" => TokenNot
);

token_lookahead!(
    TokenLe::into_parser().map(|_|())
        .or(TokenLtSlash::into_parser().map(|_|())),
    "<" => TokenLt
);

token_lookahead!(
    TokenGe::into_parser(),
    ">" => TokenGt
);

sep!("," => SepComma);
sep!("::" => SepColonColon);
sep!(";" => SepSemiColon);
sep!("(" => SepLeftParen);
sep!(")" => SepRightParen);
sep!("[" => SepLeftBracket);
sep!("]" => SepRightBracket);
sep!("{" => SepLeftBrace);
sep!("}" => SepRightBrace);
sep!("@@" => SepAtAt);
sep!("->" => SepArrowRight);

sep_lookahead!(SepColonColon::into_parser(),":" => SepColon);

/// Delimiter group `{...}`
pub type Brace<I, T> = Delimiter<SepLeftBrace<I>, SepRightBrace<I>, T>;

/// Delimiter group `[...]`
pub type Bracket<I, T> = Delimiter<SepLeftBracket<I>, SepRightBracket<I>, T>;

/// Delimiter group `(...)`
pub type Paren<I, T> = Delimiter<SepLeftParen<I>, SepRightParen<I>, T>;

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::input::TokenStream;

    use super::*;

    #[test]
    fn test_lookahead() {
        assert_eq!(
            TokenAt::parse(TokenStream::from("@a")),
            Ok((TokenAt(TokenStream::from("@")), TokenStream::from((1, "a"))))
        );

        assert_eq!(
            TokenAt::parse(TokenStream::from("@@a")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("@"),
                Span { offset: 0, len: 3 }
            )))
        );

        assert_eq!(
            SepColon::parse(TokenStream::from("::a")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token(":"),
                Span { offset: 0, len: 3 }
            )))
        );

        assert_eq!(
            TokenMinus::parse(TokenStream::from("->")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("-"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenMinus::parse(TokenStream::from("-=")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("-"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenEq::parse(TokenStream::from("==")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("="),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenNot::parse(TokenStream::from("!=")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("!"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenLt::parse(TokenStream::from("<=")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("<"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenGt::parse(TokenStream::from(">=")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token(">"),
                Span { offset: 0, len: 2 }
            )))
        );
    }
}
