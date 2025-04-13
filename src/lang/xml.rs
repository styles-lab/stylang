use parserc::{Parse, Parser, ParserExt};

use crate::lang::{parse_punctuation_sep, token_of};

use super::{Block, Comment, Lit, ParseError, StylangInput, XmlIdent, parse_comment_list};

/// value expr for xml attribute.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum XmlExpr<I> {
    /// a stylang block.
    Block(Block<I>),
    /// A lit value.
    Lit(Lit<I>),
}

impl<I> Parse<I> for XmlExpr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        Lit::into_parser()
            .map(|v| Self::Lit(v))
            .or(Block::into_parser().map(|v| Self::Block(v)))
            .parse(input)
    }
}

/// Xml attribute name/value pair: `xx=...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlAttr<I> {
    /// comment list.
    pub comment_list: Vec<Comment<I>>,
    /// xml attribute name.
    pub name: XmlIdent<I>,
    /// equal token: `=`
    pub eq_token: I,
    /// xml attribute value.
    pub value: XmlExpr<I>,
}

impl<I> Parse<I> for XmlAttr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comment_list, input) = parse_comment_list(input)?;
        let (name, input) = XmlIdent::parse(input)?;
        let (eq_token, input) = parse_punctuation_sep(b'=').parse(input)?;
        let (value, input) = XmlExpr::parse(input)?;

        Ok((
            Self {
                comment_list,
                name,
                eq_token,
                value,
            },
            input,
        ))
    }
}

/// Xml start tag: `<xxx xx=xx ..>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlStart<I> {
    /// comment list.
    pub comment_list: Vec<Comment<I>>,
    /// token `<`
    pub start_token: I,
    /// xml start tag name.
    pub ident: XmlIdent<I>,
    /// optional attribute name/value pairs.
    pub attrs: Vec<XmlAttr<I>>,
    /// token `/>` or `>`
    pub end_token: I,
}

impl<I> Parse<I> for XmlStart<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comment_list, input) = parse_comment_list(input)?;
        let (start_token, input) = token_of("<").parse(input)?;
        let (ident, mut input) = XmlIdent::parse(input)?;

        let mut attrs = vec![];

        loop {
            let attr;

            (attr, input) = XmlAttr::into_parser().ok().parse(input)?;

            if let Some(attr) = attr {
                attrs.push(attr);
            } else {
                break;
            }
        }

        let (end_token, input) = token_of("/>").or(token_of(">")).parse(input)?;

        Ok((
            Self {
                comment_list,
                start_token,
                ident,
                attrs,
                end_token,
            },
            input,
        ))
    }
}

/// Xml end tag: `</xxx>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlEnd<I> {
    /// comment list.
    pub comment_list: Vec<Comment<I>>,
    /// token `</`
    pub start_token: I,
    /// xml start tag name.
    pub ident: XmlIdent<I>,
    /// token `>`
    pub end_token: I,
}

impl<I> Parse<I> for XmlEnd<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (comment_list, input) = parse_comment_list(input)?;
        let (start_token, input) = token_of("</").parse(input)?;
        let (ident, input) = XmlIdent::into_parser().fatal().parse(input)?;
        let (end_token, input) = token_of(">").fatal().parse(input)?;

        Ok((
            Self {
                comment_list,
                start_token,
                ident,
                end_token,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        Lit, LitStr, ParseError, TokenError, TokenStream, XmlEnd, XmlExpr, XmlIdent,
    };

    use super::{XmlAttr, XmlStart};

    #[test]
    fn test_start_tag() {
        assert_eq!(
            XmlStart::parse(TokenStream::from("<hello/>")),
            Ok((
                XmlStart {
                    comment_list: vec![],
                    start_token: TokenStream::from((0, "<")),
                    ident: XmlIdent(TokenStream::from((1, "hello"))),
                    attrs: vec![],
                    end_token: TokenStream::from((6, "/>"))
                },
                TokenStream::from((8, ""))
            ))
        );

        assert_eq!(
            XmlStart::parse(TokenStream::from("< hello >")),
            Ok((
                XmlStart {
                    comment_list: vec![],
                    start_token: TokenStream::from((0, "<")),
                    ident: XmlIdent(TokenStream::from((2, "hello"))),
                    attrs: vec![],
                    end_token: TokenStream::from((8, ">"))
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            XmlStart::parse(TokenStream::from("<hello / >")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Keyword(">"),
                Span { offset: 7, len: 3 }
            )))
        );
    }

    #[test]
    fn test_end_tag() {
        assert_eq!(
            XmlEnd::parse(TokenStream::from("</ hello>")),
            Ok((
                XmlEnd {
                    comment_list: vec![],
                    start_token: TokenStream::from((0, "</")),
                    ident: XmlIdent(TokenStream::from((3, "hello"))),
                    end_token: TokenStream::from((8, ">"))
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            XmlEnd::parse(TokenStream::from("<hello>")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Keyword("</"),
                Span { offset: 0, len: 7 }
            )))
        );

        assert_eq!(
            XmlEnd::parse(TokenStream::from("</>")),
            Err(ControlFlow::Fatal(ParseError::Expect(
                TokenError::XmlIdent,
                Span { offset: 2, len: 1 }
            )))
        );
        assert_eq!(
            XmlEnd::parse(TokenStream::from("</hello")),
            Err(ControlFlow::Fatal(ParseError::Expect(
                TokenError::Keyword(">"),
                Span { offset: 7, len: 0 }
            )))
        );
    }

    #[test]
    fn test_attr() {
        assert_eq!(
            XmlAttr::parse(TokenStream::from(r#"v="hello""#)),
            Ok((
                XmlAttr {
                    comment_list: vec![],
                    name: XmlIdent(TokenStream::from("v")),
                    eq_token: TokenStream::from((1, "=")),
                    value: XmlExpr::Lit(Lit::Str(LitStr(TokenStream::from((3, "hello")))))
                },
                TokenStream::from((9, ""))
            ))
        );
    }
}
