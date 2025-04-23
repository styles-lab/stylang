use parserc::{Parse, Parser, ParserExt, derive_parse};

use super::{
    At, Comma, Ident, LeftParenthesis, Lit, ParseError, Punctuated, RightParenthesis, S,
    StylangInput,
};

use parserc::{keyword, take_till, take_until};

/// An Attribute, like `@platform` or `@sol("./erc20.json")`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct Attr<I>
where
    I: StylangInput,
{
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

/// Outer line doc, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutlineDoc<I>(pub I);

impl<I> Parse<I> for OutlineDoc<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("///").parse(input)?;

        let (content, mut input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Self(content), input.split_off(1)))
    }
}

/// Line comment, be like: `// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineComment<I>(pub I);

impl<I> Parse<I> for LineComment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("//").parse(input)?;

        let (content, mut input) = take_till(|c| c == b'\n').parse(input)?;

        Ok((Self(content), input.split_off(1)))
    }
}

/// Block comment, be like: `/* ... */`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockComment<I>(pub I);

impl<I> Parse<I> for BlockComment<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("/*").parse(input)?;

        let (content, mut input) = take_until("*/").parse(input)?;

        Ok((Self(content), input.split_off(2)))
    }
}

/// Outer block comment, be like: `/** ... */`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutBlockDoc<I>(pub I);

impl<I> Parse<I> for OutBlockDoc<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("/**").parse(input)?;

        let (content, mut input) = take_until("*/").parse(input)?;

        Ok((Self(content), input.split_off(2)))
    }
}

/// Comment token.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Comment<I>
where
    I: StylangInput,
{
    OutlineDoc(OutlineDoc<I>),
    LineComment(LineComment<I>),
    OutBlockDoc(OutBlockDoc<I>),
    BlockComment(BlockComment<I>),
}

/// Metadata for item/patt...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Meta<I>
where
    I: StylangInput,
{
    Attr(Attr<I>),
    Comment(Comment<I>),
}

/// Meta list
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MetaList<I>(pub Vec<Meta<I>>)
where
    I: StylangInput;

impl<I> Parse<I> for MetaList<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut meta_list = vec![];
        loop {
            (_, input) = S::into_parser().ok().parse(input)?;

            let meta;
            (meta, input) = Meta::into_parser().ok().parse(input)?;

            if let Some(meta) = meta {
                meta_list.push(meta);
            } else {
                break;
            }
        }

        Ok((Self(meta_list), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        At, Ident, LeftParenthesis, Lit, LitParams, LitStr, Punctuated, RightParenthesis, S,
        TokenStream,
    };

    use super::*;

    #[test]
    fn test_lit_params() {
        LitParams::parse(TokenStream::from(r#"( "erc20.json" )"#)).unwrap();
    }

    #[test]
    fn test_attr() {
        assert_eq!(
            Attr::parse(TokenStream::from("@platform")),
            Ok((
                Attr {
                    keyword: At(TokenStream::from((0, "@"))),
                    ident: Ident(TokenStream::from((1, "platform"))),
                    s1: None,
                    params: None
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            Attr::parse(TokenStream::from(r#"@sol ( "erc20.json" )"#)),
            Ok((
                Attr {
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

    #[test]
    fn test_comment() {
        assert_eq!(
            Comment::parse(TokenStream::from("/// hello world\n")),
            Ok((
                Comment::OutlineDoc(OutlineDoc(TokenStream::from((3, " hello world")))),
                TokenStream::from((16, ""))
            ))
        );

        assert_eq!(
            Comment::parse(TokenStream::from("// hello world\n")),
            Ok((
                Comment::LineComment(LineComment(TokenStream::from((2, " hello world")))),
                TokenStream::from((15, ""))
            ))
        );

        assert_eq!(
            Comment::parse(TokenStream::from("/** \n\nhello*/")),
            Ok((
                Comment::OutBlockDoc(OutBlockDoc(TokenStream::from((3, " \n\nhello")))),
                TokenStream::from((13, ""))
            ))
        );

        assert_eq!(
            Comment::parse(TokenStream::from("/* \n\nhello*/")),
            Ok((
                Comment::BlockComment(BlockComment(TokenStream::from((2, " \n\nhello")))),
                TokenStream::from((12, ""))
            ))
        );
    }

    #[test]
    fn test_meta_list() {
        assert_eq!(
            MetaList::parse(TokenStream::from("/// hello\n@platform")),
            Ok((
                MetaList(vec![
                    Meta::Comment(Comment::OutlineDoc(OutlineDoc(TokenStream::from((
                        3, " hello"
                    ))))),
                    Meta::Attr(Attr {
                        keyword: At(TokenStream::from((10, "@"))),
                        ident: Ident(TokenStream::from((11, "platform"))),
                        s1: None,
                        params: None
                    })
                ]),
                TokenStream::from((19, ""))
            ))
        );
    }
}
