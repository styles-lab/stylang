use parserc::{
    errors::{ControlFlow, MapFatal as _},
    lang::LangInput,
    span::ToSpan,
    syntax::{Or, PartialSyntax, Syntax},
};

use crate::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    lit::Lit,
    meta::MetaList,
    stmt::Block,
    token::*,
};

/// Xml attribute value parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum XmlValue<I>
where
    I: LangInput,
{
    Lit(MetaList<I>, Lit<I>),
    Block(MetaList<I>, Block<I>),
}

impl<I> From<XmlValue<I>> for Box<Expr<I>>
where
    I: LangInput,
{
    fn from(value: XmlValue<I>) -> Self {
        match value {
            XmlValue::Lit(metas, lit) => Box::new(Expr::Lit(metas, lit)),
            XmlValue::Block(metas, block) => Box::new(Expr::Block(metas, block)),
        }
    }
}

///  Xml attr/value pair parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct XmlAttr<I>
where
    I: LangInput,
{
    /// required tag name.
    pub ident: (Option<S<I>>, XmlIdent<I>),
    /// eq token: `=`
    #[fatal]
    pub eq_token: (Option<S<I>>, TokenEq<I>, Option<S<I>>),
    /// Value expr.
    #[fatal]
    #[from(XmlValue<_>)]
    pub value: Box<Expr<I>>,
}

/// Xml start tag: `<a xxx="..." ...>`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct XmlStart<I>
where
    I: LangInput,
{
    /// token `<`
    pub delimiter_start: (TokenLt<I>, Option<S<I>>),
    /// required tag name.
    #[fatal]
    pub ident: XmlIdent<I>,
    /// value list.
    pub values: Vec<XmlAttr<I>>,
    /// token `>` or `/>`
    #[fatal]
    pub delimiter_end: (Option<S<I>>, Or<TokenSlashGt<I>, TokenGt<I>>),
}

impl<I> PartialSyntax<I, LangError, TokenLt<I>> for XmlStart<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        delimiter_start: TokenLt<I>,
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = input.parse()?;
        let delimiter_start = (delimiter_start, s);
        let (ident, input) = input.parse()?;
        let (values, input) = input.parse()?;
        let (delimiter_end, input) = input.parse().fatal()?;

        Ok((
            Self {
                delimiter_start,
                ident,
                values,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Xml end tag: `</xxx>`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct XmlEnd<I>
where
    I: LangInput,
{
    /// token `</`
    pub delimiter_start: (Option<S<I>>, TokenLtSlash<I>, Option<S<I>>),
    /// required tag name.
    #[fatal]
    pub ident: XmlIdent<I>,
    /// token `>` or `/>`
    #[fatal]
    pub delimiter_end: (Option<S<I>>, TokenGt<I>),
}

/// A tuple expression parser: `(expr)`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprXml<I>
where
    I: LangInput,
{
    /// The optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// the start tag.
    pub start: XmlStart<I>,
    /// the end tag.
    pub end: Option<(Vec<Expr<I>>, XmlEnd<I>)>,
}

impl<I> ToSpan<usize> for ExprXml<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::span::Span<usize> {
        self.meta_list.to_span() ^ self.start.to_span() ^ self.end.to_span()
    }
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, TokenLt<I>)> for ExprXml<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, token_lt): (MetaList<I>, TokenLt<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (start, input) = XmlStart::parse_with_prefix(token_lt, input)?;

        if let Or::First(_) = &start.delimiter_end.1 {
            return Ok((
                Self {
                    meta_list,
                    start,
                    end: None,
                },
                input,
            ));
        }

        let (children, input): (Vec<Expr<I>>, I) = input.parse().fatal()?;

        for node in children.iter() {
            match node {
                Expr::Xml(_) | Expr::If(_) => {}
                _ => {
                    println!("{:?}", children);
                    return Err(ControlFlow::Fatal(LangError::invalid(
                        SyntaxKind::XmlNode,
                        node.to_span(),
                    )));
                }
            }
        }

        let span = input.to_span();
        let (end, input): (XmlEnd<I>, I) = input.parse().fatal()?;

        if end.ident.0.as_str() != start.ident.0.as_str() {
            return Err(ControlFlow::Fatal(LangError::expect(
                SyntaxKind::XmlEndTag(start.ident.to_span()),
                span,
            )));
        }

        Ok((
            Self {
                meta_list,
                start,
                end: Some((children, end)),
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        lang::{Span, TokenStream},
        syntax::{Delimiter, Syntax},
    };

    use crate::{
        expr::ExprIf,
        lit::LitStr,
        meta::Meta,
        stmt::{Stmt, Stmts},
    };

    use super::*;

    #[test]
    fn test_xml() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                r#"
                <a
                    hello="world"
                >

                <b/>

                if a {
                    <c/>
                } else {
                    <d/>
                }
                </a>
                "#
            )),
            Ok((
                Expr::Xml(ExprXml {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n                "
                    }))],
                    start: XmlStart {
                        delimiter_start: (
                            TokenLt(TokenStream {
                                offset: 17,
                                value: "<"
                            }),
                            None
                        ),
                        ident: XmlIdent(TokenStream {
                            offset: 18,
                            value: "a"
                        }),
                        values: vec![XmlAttr {
                            ident: (
                                Some(S(TokenStream {
                                    offset: 19,
                                    value: "\n                    "
                                })),
                                XmlIdent(TokenStream {
                                    offset: 40,
                                    value: "hello"
                                })
                            ),
                            eq_token: (
                                None,
                                TokenEq(TokenStream {
                                    offset: 45,
                                    value: "="
                                }),
                                None
                            ),
                            value: Box::new(Expr::Lit(
                                vec![],
                                Lit::String(LitStr(TokenStream {
                                    offset: 47,
                                    value: "world"
                                }))
                            ))
                        }],
                        delimiter_end: (
                            Some(S(TokenStream {
                                offset: 53,
                                value: "\n                "
                            })),
                            Or::Second(TokenGt(TokenStream {
                                offset: 70,
                                value: ">"
                            }))
                        )
                    },
                    end: Some((
                        vec![
                            Expr::Xml(ExprXml {
                                meta_list: vec![Meta::S(S(TokenStream {
                                    offset: 71,
                                    value: "\n\n                "
                                }))],
                                start: XmlStart {
                                    delimiter_start: (
                                        TokenLt(TokenStream {
                                            offset: 89,
                                            value: "<"
                                        }),
                                        None
                                    ),
                                    ident: XmlIdent(TokenStream {
                                        offset: 90,
                                        value: "b"
                                    }),
                                    values: vec![],
                                    delimiter_end: (
                                        None,
                                        Or::First(TokenSlashGt(TokenStream {
                                            offset: 91,
                                            value: "/>"
                                        }))
                                    )
                                },
                                end: None
                            }),
                            Expr::If(ExprIf {
                                meta_list: vec![Meta::S(S(TokenStream {
                                    offset: 93,
                                    value: "\n\n                "
                                }))],
                                keyword: (
                                    KeywordIf(TokenStream {
                                        offset: 111,
                                        value: "if"
                                    }),
                                    S(TokenStream {
                                        offset: 113,
                                        value: " "
                                    })
                                ),
                                expr: Box::new(Expr::Ident(
                                    vec![],
                                    Ident(TokenStream {
                                        offset: 114,
                                        value: "a"
                                    })
                                )),
                                if_branch: Block(Delimiter {
                                    start: (
                                        Some(S(TokenStream {
                                            offset: 115,
                                            value: " "
                                        })),
                                        TokenLeftBrace(TokenStream {
                                            offset: 116,
                                            value: "{"
                                        }),
                                        Some(S(TokenStream {
                                            offset: 117,
                                            value: "\n                    "
                                        }))
                                    ),
                                    end: (
                                        None,
                                        TokenRightBrace(TokenStream {
                                            offset: 159,
                                            value: "}"
                                        }),
                                        Some(S(TokenStream {
                                            offset: 160,
                                            value: " "
                                        }))
                                    ),
                                    body: Stmts(vec![Stmt::Expr(
                                        Expr::Xml(ExprXml {
                                            meta_list: vec![],
                                            start: XmlStart {
                                                delimiter_start: (
                                                    TokenLt(TokenStream {
                                                        offset: 138,
                                                        value: "<"
                                                    }),
                                                    None
                                                ),
                                                ident: XmlIdent(TokenStream {
                                                    offset: 139,
                                                    value: "c"
                                                }),
                                                values: vec![],
                                                delimiter_end: (
                                                    None,
                                                    Or::First(TokenSlashGt(TokenStream {
                                                        offset: 140,
                                                        value: "/>"
                                                    }))
                                                )
                                            },
                                            end: None
                                        }),
                                        Some(S(TokenStream {
                                            offset: 142,
                                            value: "\n                "
                                        })),
                                        None,
                                        None
                                    )])
                                }),
                                else_branch: Some((
                                    KeywordElse(TokenStream {
                                        offset: 161,
                                        value: "else"
                                    }),
                                    S(TokenStream {
                                        offset: 165,
                                        value: " "
                                    }),
                                    Box::new(Expr::Block(
                                        vec![],
                                        Block(Delimiter {
                                            start: (
                                                None,
                                                TokenLeftBrace(TokenStream {
                                                    offset: 166,
                                                    value: "{"
                                                }),
                                                Some(S(TokenStream {
                                                    offset: 167,
                                                    value: "\n                    "
                                                }))
                                            ),
                                            end: (
                                                None,
                                                TokenRightBrace(TokenStream {
                                                    offset: 209,
                                                    value: "}"
                                                }),
                                                Some(S(TokenStream {
                                                    offset: 210,
                                                    value: "\n                "
                                                }))
                                            ),
                                            body: Stmts(vec![Stmt::Expr(
                                                Expr::Xml(ExprXml {
                                                    meta_list: vec![],
                                                    start: XmlStart {
                                                        delimiter_start: (
                                                            TokenLt(TokenStream {
                                                                offset: 188,
                                                                value: "<"
                                                            }),
                                                            None
                                                        ),
                                                        ident: XmlIdent(TokenStream {
                                                            offset: 189,
                                                            value: "d"
                                                        }),
                                                        values: vec![],
                                                        delimiter_end: (
                                                            None,
                                                            Or::First(TokenSlashGt(TokenStream {
                                                                offset: 190,
                                                                value: "/>"
                                                            }))
                                                        )
                                                    },
                                                    end: None
                                                }),
                                                Some(S(TokenStream {
                                                    offset: 192,
                                                    value: "\n                "
                                                })),
                                                None,
                                                None
                                            )])
                                        })
                                    ))
                                ))
                            })
                        ],
                        XmlEnd {
                            delimiter_start: (
                                None,
                                TokenLtSlash(TokenStream {
                                    offset: 227,
                                    value: "</"
                                }),
                                None
                            ),
                            ident: XmlIdent(TokenStream {
                                offset: 229,
                                value: "a"
                            }),
                            delimiter_end: (
                                None,
                                TokenGt(TokenStream {
                                    offset: 230,
                                    value: ">"
                                })
                            )
                        }
                    ))
                }),
                TokenStream {
                    offset: 231,
                    value: "\n                "
                }
            ))
        );
    }

    #[test]
    fn miss_end_tag() {
        assert_eq!(
            Expr::parse(TokenStream::from("<a>")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: SyntaxKind::Token("</"),
                span: Span::Some { start: 3, end: 3 },
                item: None
            }))
        );
    }

    #[test]
    fn invalid_node() {
        assert_eq!(
            Expr::parse(TokenStream::from("<a>value</a>")),
            Err(ControlFlow::Fatal(LangError::Invalid {
                kind: SyntaxKind::XmlNode,
                span: Span::Some { start: 3, end: 8 },
                item: None
            }))
        );
    }
}
