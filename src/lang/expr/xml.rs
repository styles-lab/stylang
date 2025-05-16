//! xml syntax analyser.

use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    lit::Lit,
    meta::MetaList,
    tokens::*,
};

use super::{Block, Expr};

/// value expr for xml attribute.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum XmlAttrValue<I>
where
    I: LangInput,
{
    /// A lit value.
    Lit(Lit<I>),
    /// A code block vlaue.
    Block(Block<I>),
}

/// Xml attribute name/value pair: `xx=...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct XmlAttr<I>
where
    I: LangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// xml attribute name.
    #[key_field]
    pub name: XmlIdent<I>,
    /// equal token: `=`
    pub eq_token: (Option<S<I>>, Eq<I>, Option<S<I>>),
    /// xml attribute value.
    pub value: XmlAttrValue<I>,
}

/// Xml start tag end token: `>` or `/>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum XmlStartDelimiterEnd<I>
where
    I: LangInput,
{
    Empty(SlashGt<I>),
    WithContent(Gt<I>),
}

/// Xml start tag: `<xxx xx=xx ..>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct XmlStart<I>
where
    I: LangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// delimiter start token `<`
    pub delimiter_start: Lt<I>,
    /// xml tag name.
    pub ident: XmlIdent<I>,
    /// optional attribute list.
    pub attrs: Vec<(S<I>, XmlAttr<I>)>,
    /// delimiter end token `/>`
    pub delimiter_end: XmlStartDelimiterEnd<I>,
}

impl<I> Parse<I> for XmlStart<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;

        let span = input.span();

        let (delimiter_start, input) = LtStart::parse(input)?;

        let LtStart::Lt(delimiter_start) = delimiter_start else {
            return Err(parserc::ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("<"),
                span,
            )));
        };

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (ident, input) = XmlIdent::into_parser().fatal().parse(input)?;

        let (attrs, input) = Vec::<(S<I>, XmlAttr<I>)>::into_parser()
            .fatal()
            .parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (delimiter_end, input) = XmlStartDelimiterEnd::into_parser().fatal().parse(input)?;

        Ok((
            Self {
                meta_list,
                delimiter_start,
                ident,
                attrs,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Xml end tag: `</xxx>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct XmlEnd<I>
where
    I: LangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// token `</`
    #[key_field]
    pub delimiter_start: LtSlash<I>,
    /// xml start tag name.
    pub ident: (Option<S<I>>, XmlIdent<I>, Option<S<I>>),
    /// token `>`
    pub delimiter_end: Gt<I>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum XmlChild<I>
where
    I: LangInput,
{
    Xml(ExprXml<I>),
}

impl<I> From<XmlChild<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: XmlChild<I>) -> Self {
        match value {
            XmlChild::Xml(expr_xml) => Self::Xml(expr_xml),
        }
    }
}

/// Declaration a xml document.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprXml<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// start tag.
    pub start_tag: XmlStart<I>,
    /// child exprs.
    pub children: Vec<Expr<I>>,
    /// optional end tag.
    pub end_tag: Option<XmlEnd<I>>,
}

impl<I> Parse<I> for ExprXml<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (start_tag, mut input) = XmlStart::parse(input)?;

        let XmlStartDelimiterEnd::WithContent(_) = &start_tag.delimiter_end else {
            return Ok((
                Self {
                    meta_list,
                    start_tag,
                    children: vec![],
                    end_tag: None,
                },
                input,
            ));
        };

        let mut children = vec![];

        let ident_span = start_tag.ident.0.span();

        loop {
            let child;

            (child, input) = XmlChild::into_parser()
                .map(|v| Expr::from(v))
                .ok()
                .parse(input)?;

            let Some(child) = child else {
                let (end_tag, input) = XmlEnd::into_parser()
                    .map_err(|input: I, _| {
                        LangError::expect(TokenKind::XmlEndTag(ident_span), input.span())
                    })
                    .fatal()
                    .parse(input)?;

                return Ok((
                    Self {
                        meta_list,
                        start_tag,
                        children,
                        end_tag: Some(end_tag),
                    },
                    input,
                ));
            };

            children.push(child);
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{
        errors::TokenKind, expr::Block, inputs::TokenStream, lit::LitStr, stmt::Stmts,
    };

    use super::*;

    #[test]
    fn test_empty_tag() {
        XmlStart::parse(TokenStream::from(
            r#"<text-field text={value} prompt="Donate via ethereum network with a minimum donation of 0.1eth."/>"#,
        )).unwrap();
    }

    #[test]
    fn test_start_tag() {
        assert_eq!(
            XmlStart::parse(TokenStream::from("<hello/>")),
            Ok((
                XmlStart {
                    meta_list: MetaList(vec![]),
                    delimiter_start: Lt(TokenStream::from((0, "<"))),
                    ident: XmlIdent(TokenStream::from((1, "hello"))),
                    attrs: vec![],
                    delimiter_end: XmlStartDelimiterEnd::Empty(SlashGt(TokenStream::from((
                        6, "/>"
                    ))))
                },
                TokenStream::from((8, ""))
            ))
        );

        assert_eq!(
            XmlStart::parse(TokenStream::from("< hello >")),
            Ok((
                XmlStart {
                    meta_list: MetaList(vec![]),
                    delimiter_start: Lt(TokenStream::from((0, "<"))),
                    ident: XmlIdent(TokenStream::from((2, "hello"))),
                    attrs: vec![],
                    delimiter_end: XmlStartDelimiterEnd::WithContent(Gt(TokenStream::from((
                        8, ">"
                    ))))
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            XmlStart::parse(TokenStream::from("<hello / >")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token(">"),
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
                    meta_list: MetaList(vec![]),
                    delimiter_start: LtSlash(TokenStream::from("</")),
                    ident: (
                        Some(S(TokenStream::from((2, " ")))),
                        XmlIdent(TokenStream::from((3, "hello"))),
                        None
                    ),
                    delimiter_end: Gt(TokenStream::from((8, ">")))
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            XmlEnd::parse(TokenStream::from("<hello>")),
            Err(ControlFlow::Recovable(LangError::expect(
                TokenKind::Token("</"),
                Span { offset: 0, len: 7 }
            )))
        );

        assert_eq!(
            XmlEnd::parse(TokenStream::from("</>")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::XmlIdent,
                Span { offset: 2, len: 1 }
            )))
        );
        assert_eq!(
            XmlEnd::parse(TokenStream::from("</hello")),
            Err(ControlFlow::Fatal(LangError::expect(
                TokenKind::Token(">"),
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
                    meta_list: MetaList(vec![]),
                    name: XmlIdent(TokenStream::from("v")),
                    eq_token: (None, Eq(TokenStream::from((1, "="))), None),
                    value: XmlAttrValue::Lit(Lit::String(LitStr(TokenStream::from((3, "hello")))))
                },
                TokenStream::from((9, ""))
            ))
        );

        assert_eq!(
            XmlAttr::parse(TokenStream::from(r#"v={}"#)),
            Ok((
                XmlAttr {
                    meta_list: MetaList(vec![]),
                    name: XmlIdent(TokenStream::from("v")),
                    eq_token: (None, Eq(TokenStream::from((1, "="))), None),
                    value: XmlAttrValue::Block(Block {
                        delimiter_start: LeftCurlyBracket(TokenStream::from((2, "{"))),
                        stmts: Stmts(vec![]),
                        meta_list: MetaList(vec![]),
                        delimiter_end: RightCurlyBracket(TokenStream::from((3, "}")))
                    })
                },
                TokenStream::from((4, ""))
            ))
        );
    }

    #[test]
    fn test_start_tag_with_attr() {
        assert_eq!(
            XmlStart::parse(TokenStream::from(r#"<text font-family="serif" />"#)),
            Ok((
                XmlStart {
                    meta_list: MetaList(vec![]),
                    delimiter_start: Lt(TokenStream::from("<")),
                    ident: XmlIdent(TokenStream::from((1, "text"))),
                    attrs: vec![(
                        S(TokenStream::from((5, " "))),
                        XmlAttr {
                            meta_list: MetaList(vec![]),
                            name: XmlIdent(TokenStream::from((6, "font-family"))),
                            eq_token: (None, Eq(TokenStream::from((17, "="))), None),
                            value: XmlAttrValue::Lit(Lit::String(LitStr(TokenStream::from((
                                19, "serif"
                            )))))
                        }
                    )],
                    delimiter_end: XmlStartDelimiterEnd::Empty(SlashGt(TokenStream::from((
                        26, "/>"
                    ))))
                },
                TokenStream::from((28, ""))
            ))
        );
    }
}
