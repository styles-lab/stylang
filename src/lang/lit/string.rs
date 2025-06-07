use parserc::{ControlFlow, Parse, Parser, next, take_till};

use crate::lang::{
    errors::{LangError, TokenKind},
    input::LangInput,
};

/// literal string value, be like: `"...\"... "`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitStr<I>(pub I);

impl<I> Parse<I> for LitStr<I>
where
    I: LangInput,
{
    type Error = LangError;

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

        if let Some('"') = input.as_str().chars().next() {
            content.split_off(input.start() - content.start());

            input.split_to(1);

            Ok((LitStr(content), input))
        } else {
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token("\""),
                input.span(),
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{errors::TokenKind, input::TokenStream};

    use super::*;

    #[test]
    fn incomplete_str() {
        assert_eq!(
            LitStr::parse(TokenStream::from(r#""hello"#)),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token("\""),
                Span { offset: 6, len: 0 }
            )))
        );
    }

    #[test]
    fn test_lit_str() {
        assert_eq!(
            LitStr::parse(TokenStream::from(r#""""#)),
            Ok((
                LitStr(TokenStream::from((1, ""))),
                TokenStream::from((2, ""))
            ))
        );

        assert_eq!(
            LitStr::parse(TokenStream::from(r#""helo world""#)),
            Ok((
                LitStr(TokenStream::from((1, "helo world"))),
                TokenStream::from((12, ""))
            ))
        );

        assert_eq!(
            LitStr::parse(TokenStream::from(r#""helo \" world""#)),
            Ok((
                LitStr(TokenStream::from((1, r#"helo \" world"#))),
                TokenStream::from((15, ""))
            ))
        );
    }
}
