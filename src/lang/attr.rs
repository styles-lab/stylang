use parserc::{Parse, Parser, ParserExt, next};

use super::{Ident, LitCallBody, ParseError, StylangInput};

/// Attribute, be like: `@option`,`@state`,...
#[derive(Debug, PartialEq, Clone)]
pub struct Attr<I> {
    /// attribute prefix character `@`,
    pub prefix: I,
    /// attribute ident.
    pub ident: Ident<I>,
    /// optional attribute call body.
    pub body: Option<LitCallBody<I>>,
}

impl<I> Parse<I> for Attr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (prefix, input) = next(b'@').parse(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (body, input) = LitCallBody::into_parser().ok().parse(input)?;

        Ok((
            Self {
                prefix,
                ident,
                body,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        Attr, Delimiter, Ident, LitCallBody, LitExpr, LitStr, ParseError, Punctuated, Token,
        TokenStream,
    };

    #[test]
    fn test_attr() {
        assert_eq!(
            Attr::parse(TokenStream::from("@option")),
            Ok((
                Attr {
                    prefix: TokenStream::from("@"),
                    ident: Ident(TokenStream::from((1, "option"))),
                    body: None
                },
                TokenStream::from((7, ""))
            ))
        );

        assert_eq!(
            Attr::parse(TokenStream::from(r#"@sol("./erc20.json")"#)),
            Ok((
                Attr {
                    prefix: TokenStream::from("@"),
                    ident: Ident(TokenStream::from((1, "sol"))),
                    body: Some(LitCallBody {
                        delimiter: Delimiter {
                            prefix: TokenStream::from((4, "(")),
                            suffix: TokenStream::from((19, ")")),
                        },
                        inputs: Punctuated {
                            items: vec![],
                            last: Some(Box::new(LitExpr::Str(LitStr(TokenStream::from((
                                6,
                                "./erc20.json"
                            ))))))
                        }
                    })
                },
                TokenStream::from((20, ""))
            ))
        );

        assert_eq!(
            Attr::parse(TokenStream::from("@state")),
            Ok((
                Attr {
                    prefix: TokenStream::from("@"),
                    ident: Ident(TokenStream::from((1, "state"))),
                    body: None
                },
                TokenStream::from((6, ""))
            ))
        );

        assert_eq!(
            Attr::parse(TokenStream::from("@")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                Token::Ident,
                Span { offset: 1, len: 0 }
            )))
        );
    }
}
