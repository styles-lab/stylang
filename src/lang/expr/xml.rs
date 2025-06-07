//! xml syntax analyser.

use parserc::{ControlFlow, Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    expr::ExprIf,
    input::LangInput,
    lit::Lit,
    meta::MetaList,
    stmt::Block,
    token::{SepEq, SepGt, SepLt, SepLtSlash, SepSlashGt, XmlIdent},
};

/// Xml attribute value parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum XmlValue<I>
where
    I: LangInput,
{
    Lit(Lit<I>),
    Block(Block<I>),
}

///  Xml attr/value pair parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct XmlAttr<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// required tag name.
    pub ident: XmlIdent<I>,
    /// eq token: `=`
    pub eq_token: SepEq<I>,
    /// Value expr.
    pub value: XmlValue<I>,
}

/// Delimiter end token: `>` or `/>` for XmlStart.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum XmlStartDelimiterEnd<I>
where
    I: LangInput,
{
    Open(SepGt<I>),
    Closed(SepSlashGt<I>),
}

/// A parser for xml start tag.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct XmlStart<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `<`
    pub delimiter_start: SepLt<I>,
    /// required tag name.
    pub ident: XmlIdent<I>,
    /// value list.
    pub values: Vec<XmlAttr<I>>,
    /// end token: `>` or `/>`
    pub delimiter_end: XmlStartDelimiterEnd<I>,
}

/// A parser for xml start tag.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct XmlEnd<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `</`
    #[fatal]
    pub delimiter_start: SepLtSlash<I>,
    /// required tag name.
    #[fatal]
    pub ident: XmlIdent<I>,
    /// end token: `>`
    #[fatal]
    pub delimiter_end: SepGt<I>,
}

/// Xml child node parser.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum XmlChild<I>
where
    I: LangInput,
{
    If(ExprIf<I>),
    Xml(ExprXml<I>),
}

/// A parser for xml block.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprXml<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// The start tag.
    pub start_tag: XmlStart<I>,
    /// children nodes.
    pub children: Vec<XmlChild<I>>,
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

        let XmlStartDelimiterEnd::Open(_) = &start_tag.delimiter_end else {
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

            (child, input) = XmlChild::into_parser().ok().parse(input)?;

            let Some(child) = child else {
                let (end_tag, input) = XmlEnd::into_parser()
                    .map_err(|input: I, _| {
                        LangError::expect(TokenKind::XmlEndTag(ident_span), input.span())
                    })
                    .fatal()
                    .parse(input)?;

                if start_tag.ident.0.as_str() != end_tag.ident.0.as_str() {
                    return Err(ControlFlow::Fatal(LangError::expect(
                        TokenKind::XmlEndTag(ident_span),
                        input.span(),
                    )));
                }

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
    use parserc::{Delimiter, Parse};

    use crate::lang::{
        expr::{Expr, ExprXml, XmlAttr, XmlStart, XmlStartDelimiterEnd, XmlValue},
        input::TokenStream,
        lit::{Lit, LitStr},
        meta::Meta,
        stmt::{Block, Stmt, Stmts},
        token::{Ident, S, SepEq, SepLeftBrace, SepLt, SepRightBrace, SepSlashGt, XmlIdent},
        ty::TypePath,
    };

    #[test]
    fn xml_tag_text_field() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                r#"<text-field text={value} prompt="Donate via ethereum network with a minimum donation of 0.1eth."/>"#
            )),
            Ok((
                Expr::Xml(ExprXml {
                    meta_list: vec![],
                    start_tag: XmlStart {
                        meta_list: vec![],
                        delimiter_start: SepLt(
                            None,
                            TokenStream {
                                offset: 0,
                                value: "<"
                            },
                            None
                        ),
                        ident: XmlIdent(TokenStream {
                            offset: 1,
                            value: "text-field"
                        }),
                        values: vec![
                            XmlAttr {
                                meta_list: vec![Meta::S(S(TokenStream {
                                    offset: 11,
                                    value: " "
                                }))],
                                ident: XmlIdent(TokenStream {
                                    offset: 12,
                                    value: "text"
                                }),
                                eq_token: SepEq(
                                    None,
                                    TokenStream {
                                        offset: 16,
                                        value: "="
                                    },
                                    None
                                ),
                                value: XmlValue::Block(Block(Delimiter {
                                    delimiter_start: SepLeftBrace(
                                        None,
                                        TokenStream {
                                            offset: 17,
                                            value: "{"
                                        },
                                        None
                                    ),
                                    body: Stmts(vec![Stmt::Expr(
                                        Expr::TypePath(
                                            Default::default(),
                                            TypePath {
                                                first: Ident(TokenStream {
                                                    offset: 18,
                                                    value: "value"
                                                }),
                                                rest: vec![]
                                            }
                                        ),
                                        None
                                    )]),
                                    delimiter_end: SepRightBrace(
                                        None,
                                        TokenStream {
                                            offset: 23,
                                            value: "}"
                                        },
                                        Some(S(TokenStream {
                                            offset: 24,
                                            value: " "
                                        }))
                                    )
                                }))
                            },
                            XmlAttr {
                                meta_list: vec![],
                                ident: XmlIdent(TokenStream {
                                    offset: 25,
                                    value: "prompt"
                                }),
                                eq_token: SepEq(
                                    None,
                                    TokenStream {
                                        offset: 31,
                                        value: "="
                                    },
                                    None
                                ),
                                value: XmlValue::Lit(Lit::String(LitStr(TokenStream {
                                    offset: 33,
                                    value: "Donate via ethereum network with a minimum donation of 0.1eth."
                                })))
                            }
                        ],
                        delimiter_end: XmlStartDelimiterEnd::Closed(SepSlashGt(
                            None,
                            TokenStream {
                                offset: 96,
                                value: "/>"
                            },
                            None
                        ))
                    },
                    children: vec![],
                    end_tag: None
                }),
                TokenStream {
                    offset: 98,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn xml_attr() {
        assert_eq!(
            XmlAttr::parse(TokenStream::from(
                r#"prompt="Donate via ethereum network with a minimum donation of 0.1eth.""#
            )),
            Ok((
                XmlAttr {
                    meta_list: vec![],
                    ident: XmlIdent(TokenStream {
                        offset: 0,
                        value: "prompt"
                    }),
                    eq_token: SepEq(
                        None,
                        TokenStream {
                            offset: 6,
                            value: "="
                        },
                        None
                    ),
                    value: XmlValue::Lit(Lit::String(LitStr(TokenStream {
                        offset: 8,
                        value: "Donate via ethereum network with a minimum donation of 0.1eth."
                    })))
                },
                TokenStream {
                    offset: 71,
                    value: ""
                }
            ))
        );
        assert_eq!(
            XmlAttr::parse(TokenStream::from("text={value}")),
            Ok((
                XmlAttr {
                    meta_list: vec![],
                    ident: XmlIdent(TokenStream {
                        offset: 0,
                        value: "text"
                    }),
                    eq_token: SepEq(
                        None,
                        TokenStream {
                            offset: 4,
                            value: "="
                        },
                        None
                    ),
                    value: XmlValue::Block(Block(Delimiter {
                        delimiter_start: SepLeftBrace(
                            None,
                            TokenStream {
                                offset: 5,
                                value: "{"
                            },
                            None
                        ),
                        body: Stmts(vec![Stmt::Expr(
                            Expr::TypePath(
                                Default::default(),
                                TypePath {
                                    first: Ident(TokenStream {
                                        offset: 6,
                                        value: "value"
                                    }),
                                    rest: vec![]
                                }
                            ),
                            None
                        )]),
                        delimiter_end: SepRightBrace(
                            None,
                            TokenStream {
                                offset: 11,
                                value: "}"
                            },
                            None
                        )
                    }))
                },
                TokenStream {
                    offset: 12,
                    value: ""
                }
            ))
        );
    }
}
