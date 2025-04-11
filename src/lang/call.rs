use parserc::{Parse, Parser};

use crate::lang::delimited;

use super::{Delimiter, Lit, ParseError, Punctuated, StylangInput};

/// All params of this type call must be literial exprs.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LitCallBody<I> {
    /// delimiter: `(...)`
    pub delimiter: Delimiter<I>,
    /// input params.
    pub inputs: Punctuated<I, Lit<I>, b','>,
}

impl<I> Parse<I> for LitCallBody<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let ((delimiter, inputs), input) =
            delimited("(", Punctuated::into_parser(), ")").parse(input)?;

        Ok((Self { delimiter, inputs }, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        Delimiter, Digits, Lit, LitCallBody, LitNum, LitStr, ParseError, Punctuated, Token,
        TokenStream,
    };

    #[test]
    fn test_lit_call_body() {
        assert_eq!(
            LitCallBody::parse(TokenStream::from("()")),
            Ok((
                LitCallBody {
                    delimiter: Delimiter {
                        start: TokenStream::from("("),
                        end: TokenStream::from((1, ")")),
                    },
                    inputs: Punctuated {
                        items: vec![],
                        last: None
                    }
                },
                TokenStream::from((2, ""))
            ))
        );

        assert_eq!(
            LitCallBody::parse(TokenStream::from(r#"(123,"hello")"#)),
            Ok((
                LitCallBody {
                    delimiter: Delimiter {
                        start: TokenStream::from("("),
                        end: TokenStream::from((12, ")")),
                    },
                    inputs: Punctuated {
                        items: vec![(
                            Lit::Num(LitNum {
                                sign: None,
                                trunc: Some(Digits(TokenStream::from((1, "123")))),
                                comma: None,
                                fract: None,
                                exp: None,
                                unit: None
                            }),
                            TokenStream::from((4, ","))
                        )],
                        last: Some(Box::new(Lit::Str(LitStr(TokenStream::from((6, "hello"))))))
                    }
                },
                TokenStream::from((13, ""))
            ))
        );

        assert_eq!(
            LitCallBody::parse(TokenStream::from("")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Prefix("("),
                Span { offset: 0, len: 0 }
            )))
        );
    }
}
