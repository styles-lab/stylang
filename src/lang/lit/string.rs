use parserc::{
    errors::ControlFlow,
    inputs::lang::LangInput,
    parser::{Parser, next, take_till},
    syntax::Syntax,
};

use crate::lang::errors::{LangError, SyntaxKind};

/// literal string value, be like: `"...\"... "`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitStr<I>(pub I);

impl<I> Syntax<I, LangError> for LitStr<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
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
                SyntaxKind::Token("\""),
                input.span(),
            )))
        }
    }
}

#[cfg(test)]
mod tests {

    use parserc::inputs::{Span, lang::TokenStream};

    use super::*;

    #[test]
    fn incomplete_str() {
        assert_eq!(
            LitStr::parse(TokenStream::from(r#""hello"#)),
            Err(ControlFlow::Fatal(LangError::expect(
                SyntaxKind::Token("\""),
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
