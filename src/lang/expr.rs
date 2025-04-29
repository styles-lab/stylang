use parserc::derive_parse;

use super::{
    Eq, Gt, KeywordLet, Lit, Lt, LtSlash, MetaList, ParseError, Patt, Punctuated, S, SlashGt,
    StylangInput, XmlIdent,
};

/// value expr for xml attribute.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum XmlAttrValue<I>
where
    I: StylangInput,
{
    /// A lit value.
    Lit(Lit<I>),
}

/// Xml attribute name/value pair: `xx=...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct XmlAttr<I>
where
    I: StylangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// xml attribute name.
    pub name: XmlIdent<I>,
    /// equal token: `=`
    pub eq_token: (Option<S<I>>, Eq<I>, Option<S<I>>),
    /// xml attribute value.
    pub value: XmlAttrValue<I>,
}

/// Xml start tag end token: `>` or `/>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum XmlStartDelimiterEnd<I>
where
    I: StylangInput,
{
    Empty(SlashGt<I>),
    WithContent(Gt<I>),
}

/// Xml start tag: `<xxx xx=xx ..>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct XmlStart<I>
where
    I: StylangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// delimiter start token `<`
    pub delimiter_start: (Lt<I>, Option<S<I>>),
    /// xml tag name.
    pub ident: XmlIdent<I>,
    /// optional attribute list.
    pub attrs: Punctuated<XmlAttr<I>, S<I>>,
    /// delimiter end token `/>`
    pub delimiter_end: XmlStartDelimiterEnd<I>,
}

/// Xml end tag: `</xxx>`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct XmlEnd<I>
where
    I: StylangInput,
{
    /// comment list.
    pub meta_list: MetaList<I>,
    /// token `</`
    pub delimiter_start: LtSlash<I>,
    /// xml start tag name.
    pub ident: (Option<S<I>>, XmlIdent<I>, Option<S<I>>),
    /// token `>`
    pub delimiter_end: Gt<I>,
}

/// A local let binding: let x: u64 = 10.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprLet<I>
where
    I: StylangInput,
{
    /// optional metadata list.
    pub meta_list: MetaList<I>,
    /// keyword `let`.
    pub let_token: (KeywordLet<I>, S<I>),
    /// let binding patt.
    pub patt: Patt<I>,
    /// equal token: `=`
    pub eq_token: (Option<S<I>>, Eq<I>, Option<S<I>>),
    /// init expr part.
    pub expr: Box<Expr<I>>,
}

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Expr<I>
where
    I: StylangInput,
{
    Lit(MetaList<I>, Lit<I>),
    Let(ExprLet<I>),
    XmlStart(XmlStart<I>),
    XmlEnd(XmlEnd<I>),
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Parse, span::Span};

    use crate::lang::*;

    #[test]
    fn test_let() {
        assert_eq!(
            Expr::parse(TokenStream::from("let a = 10")),
            Ok((
                Expr::Let(ExprLet {
                    meta_list: MetaList(vec![]),
                    let_token: (
                        KeywordLet(TokenStream::from("let")),
                        S(TokenStream::from((3, " ")))
                    ),
                    patt: Patt::Path(PattPath {
                        meta_list: MetaList(vec![]),
                        path: TypePath {
                            first: Ident(TokenStream::from((4, "a"))),
                            rest: vec![]
                        }
                    }),
                    eq_token: (
                        None,
                        Eq(TokenStream::from((6, "="))),
                        Some(S(TokenStream::from((7, " "))))
                    ),
                    expr: Box::new(Expr::Lit(
                        MetaList(vec![]),
                        crate::lang::Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((8, "10")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ))
                }),
                TokenStream::from((10, ""))
            ))
        );
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
                    attrs: Punctuated {
                        items: vec![],
                        last: None
                    },
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
                    delimiter_start: (
                        Lt(TokenStream::from((0, "<"))),
                        Some(S(TokenStream::from((1, " "))))
                    ),
                    ident: XmlIdent(TokenStream::from((2, "hello"))),
                    attrs: Punctuated {
                        items: vec![],
                        last: None
                    },
                    delimiter_end: XmlStartDelimiterEnd::WithContent(Gt(TokenStream::from((
                        8, ">"
                    ))))
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
            Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Keyword("</"),
                Span { offset: 0, len: 7 }
            )))
        );

        assert_eq!(
            XmlEnd::parse(TokenStream::from("</>")),
            Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::XmlIdent,
                Span { offset: 2, len: 1 }
            )))
        );
        assert_eq!(
            XmlEnd::parse(TokenStream::from("</hello")),
            Err(ControlFlow::Recovable(ParseError::Expect(
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
                    meta_list: MetaList(vec![]),
                    name: XmlIdent(TokenStream::from("v")),
                    eq_token: (None, Eq(TokenStream::from((1, "="))), None),
                    value: XmlAttrValue::Lit(Lit::String(LitStr(TokenStream::from((3, "hello")))))
                },
                TokenStream::from((9, ""))
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
                    attrs: Punctuated {
                        items: vec![],
                        last: Some(Box::new(XmlAttr {
                            meta_list: MetaList(vec![]),
                            name: XmlIdent(TokenStream::from((6, "font-family"))),
                            eq_token: (None, Eq(TokenStream::from((17, "="))), None),
                            value: XmlAttrValue::Lit(Lit::String(LitStr(TokenStream::from((
                                19, "serif"
                            )))))
                        }))
                    },
                    delimiter_end: XmlStartDelimiterEnd::Empty(SlashGt(TokenStream::from((
                        26, "/>"
                    ))))
                },
                TokenStream::from((28, ""))
            ))
        );
    }
}
