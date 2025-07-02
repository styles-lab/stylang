//! The types for parsing meta-data(comments or attrs).

use parserc::{
    lang::LangInput,
    parser::{Parser, take_till, take_until},
    span::ToSpan,
    syntax::{Punctuated, Syntax},
};

use crate::{errors::LangError, lit::Lit, token::*};

/// Parsing attribute metadata: `@platform`, `@sol("./erc20.json")`,...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct Attr<I>
where
    I: LangInput,
{
    /// token `@`
    pub token: (Option<S<I>>, TokenAt<I>),
    /// attribute name.
    pub ident: Ident<I>,
    /// optional parameter list.
    pub params: Option<Paren<I, Punctuated<Lit<I>, SepComma<I>>>>,
}

/// Outer line doc: `/// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterLineDoc<I>
where
    I: LangInput,
{
    /// Delimiter start token: `///`
    pub delimiter_start: (Option<S<I>>, TokenOuterline<I>, Option<S<I>>),
    /// Inline docuement content.
    pub inline: I,
    /// newline token.
    pub delimiter_end: Option<TokenNewLine<I>>,
}

impl<I> Syntax<I, LangError> for OuterLineDoc<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (delimiter_start, input) = input.parse()?;
        let (inline, input) = take_till(|c| c == b'\n').parse(input)?;
        let (delimiter_end, input) = TokenNewLine::into_parser().ok().parse(input)?;

        Ok((
            Self {
                delimiter_start,
                inline,
                delimiter_end,
            },
            input,
        ))
    }
}

impl<I> ToSpan<usize> for OuterLineDoc<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::lang::Span {
        self.delimiter_start.to_span() ^ self.delimiter_end.to_span()
    }
}

/// Inline doc: `// ...`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct InlineComment<I>
where
    I: LangInput,
{
    /// Delimiter start token: `///`
    pub delimiter_start: (Option<S<I>>, TokenInline<I>, Option<S<I>>),
    /// Inline docuement content.
    pub inline: I,
    /// newline token.
    pub delimiter_end: Option<TokenNewLine<I>>,
}

impl<I> ToSpan<usize> for InlineComment<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::lang::Span {
        self.delimiter_start.to_span() ^ self.delimiter_end.to_span()
    }
}

impl<I> Syntax<I, LangError> for InlineComment<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (delimiter_start, input) = input.parse()?;
        let (inline, input) = take_till(|c| c == b'\n').parse(input)?;
        let (delimiter_end, input) = TokenNewLine::into_parser().ok().parse(input)?;

        Ok((
            Self {
                delimiter_start,
                inline,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Outer block doc: `/** ...*/`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct OuterBlockDoc<I>
where
    I: LangInput,
{
    /// Delimiter start token: `/**`
    pub delimiter_start: (Option<S<I>>, TokenOuterBlockStart<I>, Option<S<I>>),
    /// Inline docuement content.
    pub inline: I,
    /// Delimiter end token: `*/`
    pub delimiter_end: TokenBlockEnd<I>,
}

impl<I> ToSpan<usize> for OuterBlockDoc<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::lang::Span {
        self.delimiter_start.to_span() ^ self.delimiter_end.to_span()
    }
}

impl<I> Syntax<I, LangError> for OuterBlockDoc<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (delimiter_start, input) = input.parse()?;
        let (inline, input) = take_until("*/").parse(input)?;
        let (delimiter_end, input) = TokenBlockEnd::into_parser().fatal().parse(input)?;

        Ok((
            Self {
                delimiter_start,
                inline,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Block doc: `/* ...*/`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockComment<I>
where
    I: LangInput,
{
    /// Delimiter start token: `/*`
    pub delimiter_start: (Option<S<I>>, TokenBlockStart<I>, Option<S<I>>),
    /// Inline docuement content.
    pub inline: I,
    /// Delimiter end token: `*/`
    pub delimiter_end: TokenBlockEnd<I>,
}

impl<I> ToSpan<usize> for BlockComment<I>
where
    I: LangInput,
{
    fn to_span(&self) -> parserc::lang::Span {
        self.delimiter_start.to_span() ^ self.delimiter_end.to_span()
    }
}

impl<I> Syntax<I, LangError> for BlockComment<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (delimiter_start, input) = input.parse()?;
        let (inline, input) = take_until("*/").parse(input)?;
        let (delimiter_end, input) = TokenBlockEnd::into_parser().fatal().parse(input)?;

        Ok((
            Self {
                delimiter_start,
                inline,
                delimiter_end,
            },
            input,
        ))
    }
}

/// Parser for comments and docs.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum Comment<I>
where
    I: LangInput,
{
    Outerline(OuterLineDoc<I>),
    Inline(InlineComment<I>),
    OuterBlock(OuterBlockDoc<I>),
    Block(BlockComment<I>),
}

/// Comments list.
pub type Comments<I> = Vec<Comment<I>>;

/// Metadata parser for comment/doc/attr/...
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum Meta<I>
where
    I: LangInput,
{
    Attr(Attr<I>),
    Comment(Comment<I>),
    S(S<I>),
}

/// Metadata list.
pub type MetaList<I> = Vec<Meta<I>>;

#[cfg(test)]
mod tests {

    use parserc::{lang::TokenStream, syntax::Delimiter};

    use crate::lit::LitStr;

    use super::*;

    #[test]
    fn attr_with_params() {
        assert_eq!(
            Attr::parse(TokenStream::from(r#"  @sol("erc20.json")"#)),
            Ok((
                Attr {
                    token: (
                        Some(S(TokenStream {
                            offset: 0,
                            value: "  "
                        })),
                        TokenAt(TokenStream {
                            offset: 2,
                            value: "@"
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 3,
                        value: "sol"
                    }),
                    params: Some(Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 6,
                                value: "("
                            }),
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![],
                            tail: Some(Box::new(Lit::String(LitStr(TokenStream {
                                offset: 8,
                                value: "erc20.json"
                            }))))
                        },
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 19,
                                value: ")"
                            }),
                            None
                        )
                    })
                },
                TokenStream {
                    offset: 20,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn attr_without_params() {
        assert_eq!(
            Attr::parse(TokenStream::from(r#"  @platform"#)),
            Ok((
                Attr {
                    token: (
                        Some(S(TokenStream {
                            offset: 0,
                            value: "  "
                        })),
                        TokenAt(TokenStream {
                            offset: 2,
                            value: "@"
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 3,
                        value: "platform"
                    }),
                    params: None
                },
                TokenStream {
                    offset: 11,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn meta_list() {
        assert_eq!(
            MetaList::parse(TokenStream::from(
                r#"
        /// The ‘writing-mode’ property specifies whether the initial inline-progression-direction for a ‘text’ element shall be
        /// left-to-right, right-to-left, or top-to-bottom. The ‘writing-mode’ property applies only to ‘text’ elements;
        /// the property is ignored for ‘tspan’, ‘tref’, ‘altGlyph’ and ‘textPath’ sub-elements. (Note that the inline-progression-direction
        /// can change within a ‘text’ element due to the Unicode bidirectional algorithm and properties ‘direction’ and ‘unicode-bidi’.
        @target("wasm")
        /// For more on bidirectional text, see Relationship with bidirectionality.)
        @platform
        "#,
            )),
            Ok((
                vec![
                    Meta::Comment(Comment::Outerline(OuterLineDoc {
                        delimiter_start: (
                            Some(S(TokenStream {
                                offset: 0,
                                value: "\n        "
                            })),
                            TokenOuterline(TokenStream {
                                offset: 9,
                                value: "///"
                            }),
                            Some(S(TokenStream {
                                offset: 12,
                                value: " "
                            }))
                        ),
                        inline: TokenStream {
                            offset: 13,
                            value: "The ‘writing-mode’ property specifies whether the initial inline-progression-direction for a ‘text’ element shall be"
                        },
                        delimiter_end: Some(TokenNewLine(TokenStream {
                            offset: 137,
                            value: "\n"
                        }))
                    })),
                    Meta::Comment(Comment::Outerline(OuterLineDoc {
                        delimiter_start: (
                            Some(S(TokenStream {
                                offset: 138,
                                value: "        "
                            })),
                            TokenOuterline(TokenStream {
                                offset: 146,
                                value: "///"
                            }),
                            Some(S(TokenStream {
                                offset: 149,
                                value: " "
                            }))
                        ),
                        inline: TokenStream {
                            offset: 150,
                            value: "left-to-right, right-to-left, or top-to-bottom. The ‘writing-mode’ property applies only to ‘text’ elements;"
                        },
                        delimiter_end: Some(TokenNewLine(TokenStream {
                            offset: 266,
                            value: "\n"
                        }))
                    })),
                    Meta::Comment(Comment::Outerline(OuterLineDoc {
                        delimiter_start: (
                            Some(S(TokenStream {
                                offset: 267,
                                value: "        "
                            })),
                            TokenOuterline(TokenStream {
                                offset: 275,
                                value: "///"
                            }),
                            Some(S(TokenStream {
                                offset: 278,
                                value: " "
                            }))
                        ),
                        inline: TokenStream {
                            offset: 279,
                            value: "the property is ignored for ‘tspan’, ‘tref’, ‘altGlyph’ and ‘textPath’ sub-elements. (Note that the inline-progression-direction"
                        },
                        delimiter_end: Some(TokenNewLine(TokenStream {
                            offset: 423,
                            value: "\n"
                        }))
                    })),
                    Meta::Comment(Comment::Outerline(OuterLineDoc {
                        delimiter_start: (
                            Some(S(TokenStream {
                                offset: 424,
                                value: "        "
                            })),
                            TokenOuterline(TokenStream {
                                offset: 432,
                                value: "///"
                            }),
                            Some(S(TokenStream {
                                offset: 435,
                                value: " "
                            }))
                        ),
                        inline: TokenStream {
                            offset: 436,
                            value: "can change within a ‘text’ element due to the Unicode bidirectional algorithm and properties ‘direction’ and ‘unicode-bidi’."
                        },
                        delimiter_end: Some(TokenNewLine(TokenStream {
                            offset: 572,
                            value: "\n"
                        }))
                    })),
                    Meta::Attr(Attr {
                        token: (
                            Some(S(TokenStream {
                                offset: 573,
                                value: "        "
                            })),
                            TokenAt(TokenStream {
                                offset: 581,
                                value: "@"
                            })
                        ),
                        ident: Ident(TokenStream {
                            offset: 582,
                            value: "target"
                        }),
                        params: Some(Delimiter {
                            start: (
                                None,
                                TokenLeftParen(TokenStream {
                                    offset: 588,
                                    value: "("
                                }),
                                None
                            ),
                            body: Punctuated {
                                pairs: vec![],
                                tail: Some(Box::new(Lit::String(LitStr(TokenStream {
                                    offset: 590,
                                    value: "wasm"
                                }))))
                            },
                            end: (
                                None,
                                TokenRightParen(TokenStream {
                                    offset: 595,
                                    value: ")"
                                }),
                                Some(S(TokenStream {
                                    offset: 596,
                                    value: "\n        "
                                }))
                            )
                        })
                    }),
                    Meta::Comment(Comment::Outerline(OuterLineDoc {
                        delimiter_start: (
                            None,
                            TokenOuterline(TokenStream {
                                offset: 605,
                                value: "///"
                            }),
                            Some(S(TokenStream {
                                offset: 608,
                                value: " "
                            }))
                        ),
                        inline: TokenStream {
                            offset: 609,
                            value: "For more on bidirectional text, see Relationship with bidirectionality.)"
                        },
                        delimiter_end: Some(TokenNewLine(TokenStream {
                            offset: 681,
                            value: "\n"
                        }))
                    })),
                    Meta::Attr(Attr {
                        token: (
                            Some(S(TokenStream {
                                offset: 682,
                                value: "        "
                            })),
                            TokenAt(TokenStream {
                                offset: 690,
                                value: "@"
                            })
                        ),
                        ident: Ident(TokenStream {
                            offset: 691,
                            value: "platform"
                        }),
                        params: None
                    }),
                    Meta::S(S(TokenStream {
                        offset: 699,
                        value: "\n        "
                    }))
                ],
                TokenStream {
                    offset: 708,
                    value: ""
                }
            ))
        );
    }
}
