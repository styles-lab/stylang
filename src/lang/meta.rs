//! meta types for `stylang`

use parserc::{Parse, Parser, ParserExt, derive_parse};

use parserc::{keyword, take_till, take_until};

use super::errors::LangError;
use super::inputs::LangInput;
use super::lit::Lit;
use super::punct::Punctuated;
use super::tokens::*;

/// An Attribute, like `@platform` or `@sol("./erc20.json")`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Attr<I>
where
    I: LangInput,
{
    /// token `@`
    pub keyword: At<I>,
    /// attribute name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// optional call parameter list.
    pub params: Option<LitParams<I>>,
}

/// A literial parameter list used by attribute.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct LitParams<I>
where
    I: LangInput,
{
    pub delimiter_start: LeftParen<I>,
    pub params: Punctuated<Lit<I>, Comma<I>>,
    pub delimiter_end: RightParen<I>,
}

/// Outer line doc, be like: `/// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OutlineDoc<I>(pub I);

impl<I> Parse<I> for OutlineDoc<I>
where
    I: LangInput,
{
    type Error = LangError;

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
    I: LangInput,
{
    type Error = LangError;

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
    I: LangInput,
{
    type Error = LangError;

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
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (_, input) = keyword("/**").parse(input)?;

        let (content, mut input) = take_until("*/").parse(input)?;

        Ok((Self(content), input.split_off(2)))
    }
}

/// Comment token.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Comment<I>
where
    I: LangInput,
{
    OutlineDoc(OutlineDoc<I>),
    LineComment(LineComment<I>),
    OutBlockDoc(OutBlockDoc<I>),
    BlockComment(BlockComment<I>),
}

/// Metadata for item/patt...
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Meta<I>
where
    I: LangInput,
{
    Attr(Attr<I>),
    Comment(Comment<I>),
}

/// Meta list
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct MetaList<I>(pub Vec<Meta<I>>)
where
    I: LangInput;

impl<I> Parse<I> for MetaList<I>
where
    I: LangInput,
{
    type Error = LangError;

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

    use crate::lang::{inputs::TokenStream, lit::LitStr};

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
                    ident: (Ident(TokenStream::from((1, "platform"))), None),
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
                    ident: (
                        Ident(TokenStream::from((1, "sol"))),
                        Some(S(TokenStream::from((4, " "))))
                    ),

                    params: Some(LitParams {
                        delimiter_start: LeftParen(TokenStream::from((5, "("))),
                        params: Punctuated {
                            items: vec![],
                            last: Some(Box::new(Lit::String(LitStr(TokenStream::from((
                                8,
                                "erc20.json"
                            ))))))
                        },
                        delimiter_end: RightParen(TokenStream::from((20, ")"))),
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
                        ident: (Ident(TokenStream::from((11, "platform"))), None),
                        params: None
                    })
                ]),
                TokenStream::from((19, ""))
            ))
        );
    }
}
