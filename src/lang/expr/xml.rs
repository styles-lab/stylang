//! xml syntax analyser.

use parserc::{
    errors::ControlFlow,
    inputs::{SpanJoin, lang::LangInput},
    parser::Parser,
    syntax::{AsSpan, Syntax},
};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    expr::ExprIf,
    lit::Lit,
    meta::MetaList,
    stmt::Block,
    token::*,
};

/// Xml attribute value parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum XmlValue<I>
where
    I: LangInput,
{
    Lit(Lit<I>),
    Block(Block<I>),
}

///  Xml attr/value pair parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct XmlAttr<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// required tag name.
    pub ident: XmlIdent<I>,
    /// eq token: `=`
    pub eq_token: (Option<S<I>>, TokenEq<I>, Option<S<I>>),
    /// Value expr.
    pub value: XmlValue<I>,
}

/// Delimiter end token: `>` or `/>` for XmlStart.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum XmlStartDelimiterEnd<I>
where
    I: LangInput,
{
    Open((Option<S<I>>, TokenGt<I>)),
    Closed((Option<S<I>>, TokenSlashGt<I>)),
}

/// A parser for xml start tag.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct XmlStart<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `<`
    pub delimiter_start: (Option<S<I>>, TokenLt<I>, Option<S<I>>),
    /// required tag name.
    pub ident: XmlIdent<I>,
    /// value list.
    pub values: Vec<XmlAttr<I>>,
    /// end token: `>` or `/>`
    pub delimiter_end: XmlStartDelimiterEnd<I>,
}

/// A parser for xml start tag.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct XmlEnd<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// token `</`
    #[fatal]
    pub delimiter_start: (Option<S<I>>, TokenLtSlash<I>, Option<S<I>>),
    /// required tag name.
    #[fatal]
    pub ident: XmlIdent<I>,
    /// end token: `>`
    #[fatal]
    pub delimiter_end: (Option<S<I>>, TokenGt<I>),
}

/// Xml child node parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
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

impl<I> AsSpan for ExprXml<I>
where
    I: LangInput,
{
    fn as_span(&self) -> Option<parserc::inputs::Span> {
        self.meta_list
            .as_span()
            .join(self.start_tag.as_span())
            .join(self.children.as_span())
            .join(self.end_tag.as_span())
    }
}

impl<I> Syntax<I, LangError> for ExprXml<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
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

        let ident_span = start_tag.ident.as_span().unwrap();

        loop {
            let child;

            (child, input) = XmlChild::into_parser().ok().parse(input)?;

            let span = input.as_span().unwrap();

            let Some(child) = child else {
                let (end_tag, input) = XmlEnd::into_parser()
                    .map_err(|_| LangError::expect(SyntaxKind::XmlEndTag(ident_span), span))
                    .fatal()
                    .parse(input)?;

                if start_tag.ident.0.as_str() != end_tag.ident.0.as_str() {
                    return Err(ControlFlow::Fatal(LangError::expect(
                        SyntaxKind::XmlEndTag(ident_span),
                        input.as_span().unwrap(),
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
    use parserc::{inputs::lang::TokenStream, syntax::Delimiter};

    use crate::lang::{
        expr::Expr,
        lit::LitStr,
        meta::Meta,
        stmt::{Block, Stmt, Stmts},
    };

    use super::*;

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
                        delimiter_start: (
                            None,
                            TokenLt(TokenStream {
                                offset: 0,
                                value: "<"
                            }),
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
                                eq_token: (
                                    None,
                                    TokenEq(TokenStream {
                                        offset: 16,
                                        value: "="
                                    }),
                                    None
                                ),
                                value: XmlValue::Block(Block(Delimiter {
                                    start: (
                                        None,
                                        TokenLeftBrace(TokenStream {
                                            offset: 17,
                                            value: "{"
                                        }),
                                        None
                                    ),
                                    body: Stmts(vec![Stmt::Expr(
                                        Expr::Ident(
                                            Default::default(),
                                            Ident(TokenStream {
                                                offset: 18,
                                                value: "value"
                                            }),
                                        ),
                                        None,
                                        None,
                                        None
                                    )]),
                                    end: (
                                        None,
                                        TokenRightBrace(TokenStream {
                                            offset: 23,
                                            value: "}"
                                        }),
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
                                eq_token: (
                                    None,
                                    TokenEq(TokenStream {
                                        offset: 31,
                                        value: "="
                                    }),
                                    None
                                ),
                                value: XmlValue::Lit(Lit::String(LitStr(TokenStream {
                                    offset: 33,
                                    value: "Donate via ethereum network with a minimum donation of 0.1eth."
                                })))
                            }
                        ],
                        delimiter_end: XmlStartDelimiterEnd::Closed((
                            None,
                            TokenSlashGt(TokenStream {
                                offset: 96,
                                value: "/>"
                            }),
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
                    eq_token: (
                        None,
                        TokenEq(TokenStream {
                            offset: 6,
                            value: "="
                        }),
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
                    eq_token: (
                        None,
                        TokenEq(TokenStream {
                            offset: 4,
                            value: "="
                        }),
                        None
                    ),
                    value: XmlValue::Block(Block(Delimiter {
                        start: (
                            None,
                            TokenLeftBrace(TokenStream {
                                offset: 5,
                                value: "{"
                            }),
                            None
                        ),
                        body: Stmts(vec![Stmt::Expr(
                            Expr::Ident(
                                Default::default(),
                                Ident(TokenStream {
                                    offset: 6,
                                    value: "value"
                                }),
                            ),
                            None,
                            None,
                            None
                        )]),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 11,
                                value: "}"
                            }),
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

    #[test]
    fn distinguish_between_lt_and_xml() {
        assert_eq!(
            Expr::parse(TokenStream::from("a <hello/>")),
            Ok((
                Expr::Ident(
                    Default::default(),
                    Ident(TokenStream {
                        offset: 0,
                        value: "a"
                    })
                ),
                TokenStream {
                    offset: 1,
                    value: " <hello/>"
                }
            ))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("a < b > a")),
            Ok((
                Expr::Ident(
                    vec![],
                    Ident(TokenStream {
                        offset: 0,
                        value: "a"
                    })
                ),
                TokenStream {
                    offset: 1,
                    value: " < b > a"
                }
            ))
        );
    }

    #[test]
    fn test_xml() {
        println!(
            "{:?}",
            ExprXml::parse(TokenStream::from(
                r#"<label class="header" text="Sponsor styles-lab"/>"#
            ))
        )
    }
}
