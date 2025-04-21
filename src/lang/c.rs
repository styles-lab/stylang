use parserc::{Parse, Parser, ParserExt, derive_parse, keyword, take_till, take_until};

use crate::lang::S;

use super::{ParseError, StylangInput};

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

/// Comment list.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct CommentList<I>(pub Vec<Comment<I>>)
where
    I: StylangInput;

impl<I> Parse<I> for CommentList<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut comments = vec![];
        loop {
            (_, input) = S::into_parser().ok().parse(input)?;

            let comment;
            (comment, input) = Comment::into_parser().ok().parse(input)?;

            if let Some(comment) = comment {
                comments.push(comment);
            } else {
                break;
            }
        }

        Ok((Self(comments), input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{BlockComment, Comment, LineComment, OutBlockDoc, OutlineDoc, TokenStream};

    use super::CommentList;

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
    fn test_comments() {
        assert_eq!(
            CommentList::parse(TokenStream::from(
                r#"/// hello
                

                /// world
                "#
            )),
            Ok((
                CommentList(vec![
                    Comment::OutlineDoc(OutlineDoc(TokenStream::from((3, " hello")))),
                    Comment::OutlineDoc(OutlineDoc(TokenStream::from((47, " world"))))
                ]),
                TokenStream::from((70, ""))
            ))
        );
    }
}
