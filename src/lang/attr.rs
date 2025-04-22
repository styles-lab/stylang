use parserc::{Parse, Parser, ParserExt, derive_parse};

use super::{
    At, Comma, CommentList, Ident, LeftParenthesis, Lit, ParseError, Punctuated, RightParenthesis,
    S, StylangInput,
};

/// An Attribute, like `@platform` or `@sol("./erc20.json")`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct Attr<I>
where
    I: StylangInput,
{
    /// optional comment list.
    pub comments: CommentList<I>,
    /// token `@`
    pub keyword: At<I>,
    /// attribute name.
    pub ident: Ident<I>,
    pub s1: Option<S<I>>,
    /// optional call parameter list.
    pub params: Option<LitParams<I>>,
}

/// A literial parameter list used by attribute.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct LitParams<I>
where
    I: StylangInput,
{
    pub delimiter_start: LeftParenthesis<I>,
    pub s1: Option<S<I>>,
    pub params: Punctuated<Lit<I>, Comma<I>>,
    pub s2: Option<S<I>>,
    pub delimiter_end: RightParenthesis<I>,
}

/// A list of attributes.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AttrList<I>(pub Vec<Attr<I>>)
where
    I: StylangInput;

impl<I> Parse<I> for AttrList<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut attrs = vec![];
        loop {
            let attr;
            (attr, input) = Attr::into_parser().ok().parse(input)?;

            if let Some(attr) = attr {
                attrs.push(attr);
            } else {
                break;
            }
        }

        Ok((Self(attrs), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        At, Comment, CommentList, Ident, LeftParenthesis, Lit, LitParams, LitStr, OutlineDoc,
        Punctuated, RightParenthesis, S, TokenStream,
    };

    use super::Attr;

    #[test]
    fn test_lit_params() {
        LitParams::parse(TokenStream::from(r#"( "erc20.json" )"#)).unwrap();
    }

    #[test]
    fn test_attr() {
        assert_eq!(
            Attr::parse(TokenStream::from("///hello\n@platform")),
            Ok((
                Attr {
                    comments: CommentList(vec![Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((3, "hello"))
                    ))]),
                    keyword: At(TokenStream::from((9, "@"))),
                    ident: Ident(TokenStream::from((10, "platform"))),
                    s1: None,
                    params: None
                },
                TokenStream::from((18, ""))
            ))
        );

        assert_eq!(
            Attr::parse(TokenStream::from(r#"@sol ( "erc20.json" )"#)),
            Ok((
                Attr {
                    comments: CommentList(vec![]),
                    keyword: At(TokenStream::from("@")),
                    ident: Ident(TokenStream::from((1, "sol"))),
                    s1: Some(S(TokenStream::from((4, " ")))),
                    params: Some(LitParams {
                        delimiter_start: LeftParenthesis(TokenStream::from((5, "("))),
                        s1: Some(S(TokenStream::from((6, " ")))),
                        params: Punctuated {
                            items: vec![],
                            last: Some(Box::new(Lit::String(LitStr(TokenStream::from((
                                8,
                                "erc20.json"
                            ))))))
                        },
                        s2: Some(S(TokenStream::from((19, " ")))),
                        delimiter_end: RightParenthesis(TokenStream::from((20, ")"))),
                    })
                },
                TokenStream::from((21, ""))
            ))
        );
    }
}
