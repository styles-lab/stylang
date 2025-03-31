use parserc::{Parse, Parser, next, take_till};

use super::{ParseError, StylangInput};

/// literal string value, be like: `"...\"... "`
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct LitStr<I>(pub I);

impl<I> Parse<I> for LitStr<I>
where
    I: StylangInput,
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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{LitStr, TokenStream};

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
