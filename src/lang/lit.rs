use parserc::{Parse, Parser, next, take_till};

use super::{ParseError, TokenStream};

/// literal string declaration, be like: `"..." or '...'`
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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{LitStr, Source};

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
}
