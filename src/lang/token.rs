//! Token types for stylang.
//!

use parserc::parser::{Parser, keyword, next_if, take_while};
use parserc::syntax::{Delimiter, Syntax, tokens};
use parserc::{errors::ControlFlow, inputs::lang::LangInput};

use crate::lang::errors::{LangError, SyntaxKind};

/// Sequence of `whitespace` chars
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct S<I>(pub I);

impl<I> Syntax<I, LangError> for S<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = take_while(|c: u8| c.is_ascii_whitespace()).parse(input)?;

        if s.is_empty() {
            return Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::S,
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

impl<I> Syntax<I, LangError> for Ident<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let mut content = input.clone();
        let start = input.start();

        let span = input.span();
        let (_, input) = next_if(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|_: LangError| LangError::expect(SyntaxKind::Ident, span))
            .parse(input)?;

        let (_, input) = take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

/// Xml elem name token.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlIdent<I>(pub I);

impl<I> Syntax<I, LangError> for XmlIdent<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let mut content = input.clone();
        let start = input.start();

        let (_, input) = next_if(|c: u8| c.is_ascii_alphabetic() || c == b'_')
            .map_err(|_: LangError| LangError::expect(SyntaxKind::XmlIdent, input.span()))
            .parse(input.clone())?;

        let (_, input) =
            take_while(|c: u8| c.is_ascii_alphanumeric() || c == b'_' || c == b'-').parse(input)?;

        Ok((Self(content.split_to(input.start() - start)), input))
    }
}

/// An ascii digit characters sequence: [0-9]+
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Digits<I>(pub I);

impl<I> Syntax<I, LangError> for Digits<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (digits, input) = take_while(|c: u8| c.is_ascii_digit()).parse(input)?;

        if digits.is_empty() {
            return Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Digits,
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

impl<I> Syntax<I, LangError> for HexDigits<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (digits, input) = take_while(|c: u8| c.is_ascii_hexdigit()).parse(input)?;

        if digits.is_empty() {
            return Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::HexDigits,
                input.span(),
            )));
        }

        Ok((HexDigits(digits), input))
    }
}

/// Token `E` or `e`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct TokenExp<I>(pub I);

impl<I> Syntax<I, LangError> for TokenExp<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let span = input.span();

        keyword("E")
            .or(keyword("e"))
            .map(|v| Self(v))
            .map_err(|_: LangError| LangError::expect(SyntaxKind::Token("E or e"), span))
            .parse(input)
    }
}

/// Limit types of a range, inclusive or exclusive.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[input(I)]
#[error(LangError)]
pub enum RangeLimits<I>
where
    I: LangInput,
{
    /// ..=
    Closed((Option<S<I>>, TokenDotDotEq<I>, Option<S<I>>)),
    /// ..
    HalfOpen((Option<S<I>>, TokenDotDot<I>, Option<S<I>>)),
}

tokens!(match Token {
    "fn" => KeywordFn,
    "view" => KeywordView,
    "enum" => KeywordEnum,
    "data" => KeywordData,
    "class" => KeywordClass,
    "pub" => KeywordPub,
    "mod" => KeywordMod,
    "use" => KeywordUse,
    "extern" => KeywordExtern,
    "let" => KeywordLet,
    "if" => KeywordIf,
    "else" => KeywordElse,
    "for" => KeywordFor,
    "in" => KeywordIn,
    "loop" => KeywordLoop,
    "while" => KeywordWhile,
    "match" => KeywordMatch,
    "crate" => TokenCrate,
    "super" => TokenSuper,
    "color" => TokenColor,
    "length" => TokenLength,
    "string" => TokenString,
    "angle" => TokenAngle,
    "rgb" => TokenRgb,
    "#" => TokenNumSign,
    "_" => TokenUnderscore,
    "bool" => TokenBool,
    "true" => TokenTrue,
    "false" => TokenFalse,
    "return" => TokenReturn,
    "break" => TokenBreak,
    "continue" => TokenContinue,
    "i8" => TokenI8,
    "i16" => TokenI16,
    "i32" => TokenI32,
    "i64" => TokenI64,
    "i128" => TokenI128,
    "u8" => TokenU8,
    "u16" => TokenU16,
    "u32" => TokenU32,
    "u64" => TokenU64,
    "u128" => TokenU128,
    "f32" => TokenF32,
    "f64" => TokenF64,
    "bigint" => TokenBigInt,
    "bignum" => TokenBigNum,
    "ex" => TokenEx,
    "px" => TokenPx,
    "cm" => TokenCm,
    "mm" => TokenMm,
    "pt" => TokenPt,
    "pc" => TokenPc,
    "deg" => TokenDeg,
    "grad" => TokenGrad,
    "rad" => TokenRad,
    "%" => TokenPercent,
    "0x" => TokenHexSign,
    "none" => TokenNone,
    "@" => TokenAt,
    "///" => TokenOuterline,
    "//" => TokenInline,
    "/*" => TokenBlockStart,
    "*/" => TokenBlockEnd,
    "/**" => TokenOuterBlockStart,
    "\n" => TokenNewLine,
    "<=" => TokenLe,
    ">=" => TokenGe,
    "</" => TokenLtSlash,
    "/>" => TokenSlashGt,
    "==" => TokenEqEq,
    "-=" => TokenMinusEq,
    "*=" => TokenStarEq,
    "+=" => TokenPlusEq,
    "," => TokenComma,
    "::" => TokenColonColon,
    ";" => TokenSemiColon,
    "(" => TokenLeftParen,
    ")" => TokenRightParen,
    "[" => TokenLeftBracket,
    "]" => TokenRightBracket,
    "{" => TokenLeftBrace,
    "}" => TokenRightBrace,
    "->" => TokenArrowRight,
    "..=" => TokenDotDotEq,
    "||" => TokenOrOr,
    "|=" => TokenOrEq,
    "%=" => TokenPercentEq,
    "^=" => TokenCaretEq,
    "&=" => TokenAndEq,
    "/=" => TokenSlashEq,
    "<<=" => TokenShlEq,
    ">>=" => TokenShrEq,
    "&&" => TokenAndAnd,
    "!" => TokenNot,
    "!=" => TokenNe,
    "<<" => TokenShl,
    ">>" => TokenShr,
    "&" => TokenAnd,
    "^" => TokenCaret,
    "/" => TokenSlash,
    "*" => TokenStar,
    "+" => TokenPlus,
    "-" => TokenMinus,
    "=" => TokenEq,
    "<" => TokenLt,
    ">" => TokenGt,
    "|" => TokenOr,
    ".." => TokenDotDot,
    "." => TokenDot,
    ":" => TokenColon,
});

/// Seperator `[S]*,[S]*`
pub type SepComma<I> = (Option<S<I>>, TokenComma<I>, Option<S<I>>);
/// Seperator `[S]*;[S]*`
pub type SepSemiColon<I> = (Option<S<I>>, TokenSemiColon<I>, Option<S<I>>);
/// Seperator `[S]*->[S]*`
pub type SepArrowRight<I> = (Option<S<I>>, TokenArrowRight<I>, Option<S<I>>);

/// Delimiter group `{...}`
pub type Brace<I, T> = Delimiter<
    (Option<S<I>>, TokenLeftBrace<I>, Option<S<I>>),
    (Option<S<I>>, TokenRightBrace<I>, Option<S<I>>),
    T,
>;

/// Delimiter group `[...]`
pub type Bracket<I, T> = Delimiter<
    (Option<S<I>>, TokenLeftBracket<I>, Option<S<I>>),
    (Option<S<I>>, TokenRightBracket<I>, Option<S<I>>),
    T,
>;

/// Delimiter group `(...)`
pub type Paren<I, T> = Delimiter<
    (Option<S<I>>, TokenLeftParen<I>, Option<S<I>>),
    (Option<S<I>>, TokenRightParen<I>, Option<S<I>>),
    T,
>;

#[cfg(test)]
mod tests {

    use parserc::inputs::{Span, lang::TokenStream};

    use super::*;

    #[test]
    fn test_lookahead() {
        assert_eq!(
            <TokenAt<_> as Syntax<_, LangError>>::parse(TokenStream::from("@a")),
            Ok((TokenAt(TokenStream::from("@")), TokenStream::from((1, "a"))))
        );

        assert_eq!(
            TokenColon::parse(TokenStream::from("::a")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token(":"),
                Span { offset: 0, len: 3 }
            )))
        );

        assert_eq!(
            TokenMinus::parse(TokenStream::from("->")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token("-"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenMinus::parse(TokenStream::from("-=")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token("-"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenEq::parse(TokenStream::from("==")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token("="),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenNot::parse(TokenStream::from("!=")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token("!"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenLt::parse(TokenStream::from("<=")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token("<"),
                Span { offset: 0, len: 2 }
            )))
        );

        assert_eq!(
            TokenGt::parse(TokenStream::from(">=")),
            Err(ControlFlow::Recovable(LangError::expect(
                SyntaxKind::Token(">"),
                Span { offset: 0, len: 2 }
            )))
        );
    }
}
