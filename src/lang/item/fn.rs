use parserc::{
    lang::LangInput,
    syntax::{Punctuated, Syntax},
};

use crate::lang::{
    errors::LangError, item::Visibility, meta::MetaList, patt::PattType, stmt::Block, token::*,
    ty::Type,
};

/// Formal parameter for [`ItemFn`]
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct Param<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// field type ascription pattern.
    pub patt: PattType<I>,
}

/// Body block for [`ItemFn`]
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Body<I>
where
    I: LangInput,
{
    /// a stmts block.
    Block(Block<I>),
    /// Virtual function body.
    SemiColon(Option<S<I>>, TokenSemiColon<I>, Option<S<I>>),
}

/// Item parser for `free function` item.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ReturnType<I>
where
    I: LangInput,
{
    pub token: (Option<S<I>>, TokenArrowRight<I>, Option<S<I>>),
    #[fatal]
    pub ty: Box<Type<I>>,
}

/// Item parser for `free function` item.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ItemFn<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// optional `extern` keyword.
    pub extern_keyword: Option<(KeywordExtern<I>, S<I>)>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `fn[s]+`
    pub fn_keyword: (KeywordFn<I>, S<I>),
    /// function name.
    #[fatal]
    pub ident: Ident<I>,
    /// The formal parameter list.
    #[fatal]
    pub params: Paren<I, Punctuated<Param<I>, SepComma<I>>>,
    /// optional return type clause.
    pub return_ty: Option<ReturnType<I>>,
    /// function body clause.
    #[fatal]
    pub body: Body<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{lang::TokenStream, syntax::Delimiter};

    use crate::lang::{
        meta::{Attr, Comment, Meta, OuterLineDoc},
        stmt::Stmts,
        ty::TypePath,
    };

    use super::*;

    #[test]
    fn virtual_fn() {
        assert_eq!(
            ItemFn::parse(TokenStream::from(
                r#"
                /// A platform provided view function to display a text characters
                @platform
                extern fn label(label: Label, layout: TextLayout, fill: Fill) -> view;
            "#
            )),
            Ok((
                ItemFn {
                    meta_list: vec![
                        Meta::Comment(Comment::Outerline(OuterLineDoc {
                            delimiter_start: (
                                Some(S(TokenStream {
                                    offset: 0,
                                    value: "\n                "
                                })),
                                TokenOuterline(TokenStream {
                                    offset: 17,
                                    value: "///"
                                }),
                                Some(S(TokenStream {
                                    offset: 20,
                                    value: " "
                                }))
                            ),
                            inline: TokenStream {
                                offset: 21,
                                value: "A platform provided view function to display a text characters"
                            },
                            delimiter_end: Some(TokenNewLine(TokenStream {
                                offset: 83,
                                value: "\n"
                            }))
                        })),
                        Meta::Attr(Attr {
                            token: (
                                Some(S(TokenStream {
                                    offset: 84,
                                    value: "                "
                                })),
                                TokenAt(TokenStream {
                                    offset: 100,
                                    value: "@"
                                })
                            ),
                            ident: Ident(TokenStream {
                                offset: 101,
                                value: "platform"
                            }),
                            params: None
                        }),
                        Meta::S(S(TokenStream {
                            offset: 109,
                            value: "\n                "
                        }))
                    ],
                    extern_keyword: Some((
                        KeywordExtern(TokenStream {
                            offset: 126,
                            value: "extern"
                        }),
                        S(TokenStream {
                            offset: 132,
                            value: " "
                        })
                    )),
                    vs: None,
                    fn_keyword: (
                        KeywordFn(TokenStream {
                            offset: 133,
                            value: "fn"
                        }),
                        S(TokenStream {
                            offset: 135,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 136,
                        value: "label"
                    }),
                    params: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 141,
                                value: "("
                            }),
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![
                                (
                                    Param {
                                        meta_list: Default::default(),
                                        patt: PattType {
                                            ident: Ident(TokenStream {
                                                offset: 142,
                                                value: "label"
                                            }),
                                            sep: (
                                                None,
                                                TokenColon(TokenStream {
                                                    offset: 147,
                                                    value: ":"
                                                }),
                                                Some(S(TokenStream {
                                                    offset: 148,
                                                    value: " "
                                                }))
                                            ),
                                            ty: Type::Path(TypePath {
                                                first: Ident(TokenStream {
                                                    offset: 149,
                                                    value: "Label"
                                                }),
                                                rest: vec![]
                                            }),
                                        }
                                    },
                                    (
                                        None,
                                        TokenComma(TokenStream {
                                            offset: 154,
                                            value: ","
                                        }),
                                        Some(S(TokenStream {
                                            offset: 155,
                                            value: " "
                                        }))
                                    )
                                ),
                                (
                                    Param {
                                        meta_list: Default::default(),
                                        patt: PattType {
                                            ident: Ident(TokenStream {
                                                offset: 156,
                                                value: "layout"
                                            }),
                                            sep: (
                                                None,
                                                TokenColon(TokenStream {
                                                    offset: 162,
                                                    value: ":"
                                                }),
                                                Some(S(TokenStream {
                                                    offset: 163,
                                                    value: " "
                                                }))
                                            ),
                                            ty: Type::Path(TypePath {
                                                first: Ident(TokenStream {
                                                    offset: 164,
                                                    value: "TextLayout"
                                                }),
                                                rest: vec![]
                                            })
                                        }
                                    },
                                    (
                                        None,
                                        TokenComma(TokenStream {
                                            offset: 174,
                                            value: ","
                                        }),
                                        Some(S(TokenStream {
                                            offset: 175,
                                            value: " "
                                        }))
                                    )
                                )
                            ],
                            tail: Some(Box::new(Param {
                                meta_list: Default::default(),
                                patt: PattType {
                                    ident: Ident(TokenStream {
                                        offset: 176,
                                        value: "fill"
                                    }),
                                    sep: (
                                        None,
                                        TokenColon(TokenStream {
                                            offset: 180,
                                            value: ":"
                                        }),
                                        Some(S(TokenStream {
                                            offset: 181,
                                            value: " "
                                        }))
                                    ),
                                    ty: Type::Path(TypePath {
                                        first: Ident(TokenStream {
                                            offset: 182,
                                            value: "Fill"
                                        }),
                                        rest: vec![]
                                    })
                                }
                            }))
                        },
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 186,
                                value: ")"
                            }),
                            Some(S(TokenStream {
                                offset: 187,
                                value: " "
                            }))
                        )
                    },
                    return_ty: Some(ReturnType {
                        token: (
                            None,
                            TokenArrowRight(TokenStream {
                                offset: 188,
                                value: "->"
                            }),
                            Some(S(TokenStream {
                                offset: 190,
                                value: " "
                            }))
                        ),
                        ty: Box::new(Type::View(KeywordView(TokenStream {
                            offset: 191,
                            value: "view"
                        })))
                    }),
                    body: Body::SemiColon(
                        None,
                        TokenSemiColon(TokenStream {
                            offset: 195,
                            value: ";"
                        }),
                        Some(S(TokenStream {
                            offset: 196,
                            value: "\n            "
                        }))
                    )
                },
                TokenStream {
                    offset: 209,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn empty_fn() {
        assert_eq!(
            ItemFn::parse(TokenStream::from("fn test() {}")),
            Ok((
                ItemFn {
                    meta_list: vec![],
                    extern_keyword: None,
                    vs: None,
                    fn_keyword: (
                        KeywordFn(TokenStream {
                            offset: 0,
                            value: "fn"
                        }),
                        S(TokenStream {
                            offset: 2,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 3,
                        value: "test"
                    }),
                    params: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 7,
                                value: "("
                            }),
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: None
                        },
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 8,
                                value: ")"
                            }),
                            Some(S(TokenStream {
                                offset: 9,
                                value: " "
                            }))
                        )
                    },
                    return_ty: None,
                    body: Body::Block(Block(Delimiter {
                        start: (
                            None,
                            TokenLeftBrace(TokenStream {
                                offset: 10,
                                value: "{"
                            }),
                            None
                        ),
                        body: Stmts(vec![]),
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
}
