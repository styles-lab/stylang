//! xml syntax analyser.

use parserc::derive_parse;

use crate::lang::{
    errors::LangError, inputs::LangInput, item::Block, lit::Lit, meta::MetaList, tokens::*,
};

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
    /// a block value.
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
#[derive_parse(error = LangError,input = I)]
pub struct XmlStart<I>
where
    I: LangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// delimiter start token `<`
    #[key_field]
    pub delimiter_start: (Lt<I>, Option<S<I>>),
    /// xml tag name.
    pub ident: XmlIdent<I>,
    /// optional attribute list.
    pub attrs: Vec<(S<I>, XmlAttr<I>)>,
    /// delimiter end token `/>`
    pub delimiter_end: (Option<S<I>>, XmlStartDelimiterEnd<I>),
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

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::{errors::TokenKind, inputs::TokenStream, lit::LitStr, stmt::Stmts};

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
                    delimiter_start: (Lt(TokenStream::from((0, "<"))), None),
                    ident: XmlIdent(TokenStream::from((1, "hello"))),
                    attrs: vec![],
                    delimiter_end: (
                        None,
                        XmlStartDelimiterEnd::Empty(SlashGt(TokenStream::from((6, "/>"))))
                    )
                },
                TokenStream::from((8, ""))
            ))
        );

        assert_eq!(
            XmlStart::parse(TokenStream::from("< hello >")),
            Ok((
                XmlStart {
                    meta_list: MetaList(vec![]),
                    delimiter_start: (
                        Lt(TokenStream::from((0, "<"))),
                        Some(S(TokenStream::from((1, " "))))
                    ),
                    ident: XmlIdent(TokenStream::from((2, "hello"))),
                    attrs: vec![],
                    delimiter_end: (
                        Some(S(TokenStream::from((7, " ")))),
                        XmlStartDelimiterEnd::WithContent(Gt(TokenStream::from((8, ">"))))
                    )
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
                    delimiter_start: (Lt(TokenStream::from("<")), None),
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
                    delimiter_end: (
                        Some(S(TokenStream::from((25, " ")))),
                        XmlStartDelimiterEnd::Empty(SlashGt(TokenStream::from((26, "/>"))))
                    )
                },
                TokenStream::from((28, ""))
            ))
        );
    }
}
