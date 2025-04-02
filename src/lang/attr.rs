use parserc::{Parse, Parser, ParserExt, keyword, next, take_till};

use super::{Ident, LitCallBody, ParseError, StylangInput, skip_ws};

/// Comment of the function, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
pub struct Comment<I>(pub I);

impl<I> Parse<I> for Comment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("///").parse(input)?;

        let (content, input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Comment(content), input))
    }
}

/// Attribute, be like: `@option`,`@state`,...
#[derive(Debug, PartialEq, Clone)]
pub struct Attr<I> {
    /// attribute prefix character `@`,
    pub keyword: I,
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
                keyword: prefix,
                ident,
                body,
            },
            input,
        ))
    }
}

/// attr or comment.
#[derive(Debug, PartialEq, Clone)]
pub enum AttrOrComment<I> {
    Comment(Comment<I>),
    Attr(Attr<I>),
}

impl<I> Parse<I> for AttrOrComment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        Comment::into_parser()
            .map(|v| AttrOrComment::Comment(v))
            .or(Attr::into_parser().map(|v| AttrOrComment::Attr(v)))
            .parse(input)
    }
}

/// parse attr or comment sequence.
pub fn parse_attr_comment_list<I>(
    mut input: I,
) -> parserc::Result<Vec<AttrOrComment<I>>, I, ParseError>
where
    I: StylangInput,
{
    let mut result = vec![];

    loop {
        (_, input) = skip_ws(input)?;
        let v;

        (v, input) = AttrOrComment::into_parser().ok().parse(input)?;

        if let Some(v) = v {
            result.push(v);
            continue;
        }

        break;
    }

    Ok((result, input))
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        Attr, AttrOrComment, Comment, Delimiter, Ident, LitCallBody, LitExpr, LitStr, ParseError,
        Punctuated, Token, TokenStream,
    };

    use super::parse_attr_comment_list;

    #[test]
    fn test_attr() {
        assert_eq!(
            Attr::parse(TokenStream::from("@option")),
            Ok((
                Attr {
                    keyword: TokenStream::from("@"),
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
                    keyword: TokenStream::from("@"),
                    ident: Ident(TokenStream::from((1, "sol"))),
                    body: Some(LitCallBody {
                        delimiter: Delimiter {
                            start: TokenStream::from((4, "(")),
                            end: TokenStream::from((19, ")")),
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
                    keyword: TokenStream::from("@"),
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

    #[test]
    fn test_attr_commnet() {
        assert_eq!(
            parse_attr_comment_list(TokenStream::from("/// hello\n@hello @world///hello")),
            Ok((
                vec![
                    AttrOrComment::Comment(Comment(TokenStream::from((3, " hello")))),
                    AttrOrComment::Attr(Attr {
                        keyword: TokenStream::from((10, "@")),
                        ident: Ident(TokenStream::from((11, "hello"))),
                        body: None
                    }),
                    AttrOrComment::Attr(Attr {
                        keyword: TokenStream::from((17, "@")),
                        ident: Ident(TokenStream::from((18, "world"))),
                        body: None
                    }),
                    AttrOrComment::Comment(Comment(TokenStream::from((26, "hello")))),
                ],
                TokenStream::from((31, ""))
            ))
        );
    }
}
