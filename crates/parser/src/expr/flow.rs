use parserc::{
    errors::{ControlFlow, MapFatal as _},
    lang::LangInput,
    parser::Parser,
    span::ToSpan,
    syntax::{PartialSyntax, Syntax},
};

use crate::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    meta::MetaList,
    patt::Patt,
    stmt::Block,
    token::{
        Brace, KeywordElse, KeywordIf, KeywordLoop, KeywordMatch, KeywordWhile, S, SepComma,
        TokenFatArrow,
    },
};

/// expression `loop {...}`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub keyword: (KeywordLoop<I>, Option<S<I>>),
    /// loop body block.
    pub body: Block<I>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordLoop<I>)> for ExprLoop<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordLoop<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = input.parse()?;
        let keyword = (keyword, s);
        let (body, input) = Block::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        Ok((
            Self {
                meta_list,
                keyword,
                body,
            },
            input,
        ))
    }
}

/// expression `loop {...}`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprWhile<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `while`
    pub keyword: (KeywordWhile<I>, S<I>),
    /// condition expression: `a < b`, `let Some(a) = b.next()`,...
    pub expr: Box<Expr<I>>,
    /// loop body block.
    pub body: Block<I>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordWhile<I>)> for ExprWhile<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordWhile<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = input.parse()?;
        let keyword = (keyword, s);
        let (expr, input) = Expr::into_parser()
            .boxed()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        let (body, input) = Block::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        Ok((
            Self {
                meta_list,
                keyword,
                expr,
                body,
            },
            input,
        ))
    }
}

/// expression `if $cond {...} else ...`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `if`
    pub keyword: (KeywordIf<I>, S<I>),
    /// condition expression: `a < b`, `let Some(a) = b.next()`,...
    pub expr: Box<Expr<I>>,
    /// if branch.
    pub if_branch: Block<I>,
    /// else branch.
    pub else_branch: Option<(KeywordElse<I>, S<I>, Box<Expr<I>>)>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordIf<I>)> for ExprIf<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordIf<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = input.parse().fatal()?;
        let keyword = (keyword, s);
        let (expr, input) = Expr::into_parser()
            .boxed()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        let (if_branch, input) = Block::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::RightOperand, input.to_span()))
            .fatal()
            .parse(input.clone())?;

        let (keyword_else, input) = KeywordElse::into_parser().ok().parse(input)?;

        let Some(keyword_else) = keyword_else else {
            return Ok((
                Self {
                    meta_list,
                    keyword,
                    expr,
                    if_branch,
                    else_branch: None,
                },
                input,
            ));
        };

        let (s, input) = input.parse().fatal()?;

        let span = input.to_span();

        let (else_branch, input) = Expr::parse(input)?;

        match &else_branch {
            Expr::Block(_, _) | Expr::If(_) => {}
            _ => {
                return Err(ControlFlow::Fatal(LangError::invalid(
                    SyntaxKind::ElseBranch,
                    span,
                )));
            }
        }

        Ok((
            Self {
                meta_list,
                keyword,
                expr,
                if_branch,
                else_branch: Some((keyword_else, s, Box::new(else_branch))),
            },
            input,
        ))
    }
}

fn check_arm_patt<I>(patt: Patt<I>) -> Result<Patt<I>, ControlFlow<LangError>>
where
    I: LangInput,
{
    match &patt {
        Patt::Type(_) | Patt::Path(_) => Err(ControlFlow::Fatal(LangError::unexpect(
            SyntaxKind::ArmPatt,
            patt.to_span(),
        ))),
        _ => Ok(patt),
    }
}

/// One arm of a match expression: `0..=10 => { return true; }`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct Arm<I>
where
    I: LangInput,
{
    /// meta-data list.
    pub meta_list: MetaList<I>,
    /// arm pattern.
    #[try_filter(check_arm_patt)]
    pub patt: Patt<I>,
    /// token `=>`
    pub fat_arrow_token: (Option<S<I>>, TokenFatArrow<I>, Option<S<I>>),
    /// arm body,
    #[fatal]
    #[map_err(|err: LangError| LangError::expect(SyntaxKind::ArmBody, err.to_span()))]
    pub body: Box<Expr<I>>,
    /// optional comma separator.
    pub comma: Option<SepComma<I>>,
}

/// expression `match $cond { $case => .., $case => ...} `
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub struct ExprMatch<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `if`
    pub keyword: (KeywordMatch<I>, S<I>),
    /// condition expression: `a < b`,...
    pub expr: Box<Expr<I>>,
    /// arms delimited by a group of braces: `{ xxx => xxx, ...}`
    pub arms: Brace<I, Vec<Arm<I>>>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, KeywordMatch<I>)> for ExprMatch<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, keyword): (MetaList<I>, KeywordMatch<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = input.parse().fatal()?;
        let keyword = (keyword, s);
        let (expr, input) = input.parse().fatal()?;
        let (arms, input) = input.parse().fatal()?;

        Ok((
            Self {
                meta_list,
                keyword,
                expr,
                arms,
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
        lit::{Lit, LitBool, LitStr},
        stmt::{Stmt, Stmts},
        token::{Ident, TokenLeftBrace, TokenRightBrace, TokenTrue},
    };

    use super::*;

    #[test]
    fn invalid_else_branch() {
        assert_eq!(
            Expr::parse(TokenStream::from("if a {} else b")),
            Err(ControlFlow::Fatal(LangError::Invalid {
                kind: SyntaxKind::ElseBranch,
                span: Span::Some { start: 13, end: 14 },
                item: None
            }))
        );
    }

    #[test]
    fn nest_if() {
        assert_eq!(
            Expr::parse(TokenStream::from("if if a { b } else {c} {}")),
            Ok((
                Expr::If(ExprIf {
                    meta_list: vec![],
                    keyword: (
                        KeywordIf(TokenStream {
                            offset: 0,
                            value: "if"
                        }),
                        S(TokenStream {
                            offset: 2,
                            value: " "
                        })
                    ),
                    expr: Box::new(Expr::If(ExprIf {
                        meta_list: vec![],
                        keyword: (
                            KeywordIf(TokenStream {
                                offset: 3,
                                value: "if"
                            }),
                            S(TokenStream {
                                offset: 5,
                                value: " "
                            })
                        ),
                        expr: Box::new(Expr::Ident(
                            vec![],
                            Ident(TokenStream {
                                offset: 6,
                                value: "a"
                            })
                        )),
                        if_branch: Block(Delimiter {
                            start: (
                                Some(S(TokenStream {
                                    offset: 7,
                                    value: " "
                                })),
                                TokenLeftBrace(TokenStream {
                                    offset: 8,
                                    value: "{"
                                }),
                                Some(S(TokenStream {
                                    offset: 9,
                                    value: " "
                                }))
                            ),
                            end: (
                                None,
                                TokenRightBrace(TokenStream {
                                    offset: 12,
                                    value: "}"
                                }),
                                Some(S(TokenStream {
                                    offset: 13,
                                    value: " "
                                }))
                            ),
                            body: Stmts(vec![Stmt::Expr(
                                Expr::Ident(
                                    vec![],
                                    Ident(TokenStream {
                                        offset: 10,
                                        value: "b"
                                    })
                                ),
                                Some(S(TokenStream {
                                    offset: 11,
                                    value: " "
                                })),
                                None,
                                None
                            )])
                        }),
                        else_branch: Some((
                            KeywordElse(TokenStream {
                                offset: 14,
                                value: "else"
                            }),
                            S(TokenStream {
                                offset: 18,
                                value: " "
                            }),
                            Box::new(Expr::Block(
                                vec![],
                                Block(Delimiter {
                                    start: (
                                        None,
                                        TokenLeftBrace(TokenStream {
                                            offset: 19,
                                            value: "{"
                                        }),
                                        None
                                    ),
                                    end: (
                                        None,
                                        TokenRightBrace(TokenStream {
                                            offset: 21,
                                            value: "}"
                                        }),
                                        Some(S(TokenStream {
                                            offset: 22,
                                            value: " "
                                        }))
                                    ),
                                    body: Stmts(vec![Stmt::Expr(
                                        Expr::Ident(
                                            vec![],
                                            Ident(TokenStream {
                                                offset: 20,
                                                value: "c"
                                            })
                                        ),
                                        None,
                                        None,
                                        None
                                    )])
                                })
                            ))
                        ))
                    })),
                    if_branch: Block(Delimiter {
                        start: (
                            None,
                            TokenLeftBrace(TokenStream {
                                offset: 23,
                                value: "{"
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 24,
                                value: "}"
                            }),
                            None
                        ),
                        body: Stmts(vec![])
                    }),
                    else_branch: None
                }),
                TokenStream {
                    offset: 25,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_if() {
        assert_eq!(
            Expr::parse(TokenStream::from("if a {} else if b {} else {}")),
            Ok((
                Expr::If(ExprIf {
                    meta_list: vec![],
                    keyword: (
                        KeywordIf(TokenStream {
                            offset: 0,
                            value: "if"
                        }),
                        S(TokenStream {
                            offset: 2,
                            value: " "
                        })
                    ),
                    expr: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 3,
                            value: "a"
                        })
                    )),
                    if_branch: Block(Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 4,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 5,
                                value: "{"
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 6,
                                value: "}"
                            }),
                            Some(S(TokenStream {
                                offset: 7,
                                value: " "
                            }))
                        ),
                        body: Stmts(vec![])
                    }),
                    else_branch: Some((
                        KeywordElse(TokenStream {
                            offset: 8,
                            value: "else"
                        }),
                        S(TokenStream {
                            offset: 12,
                            value: " "
                        }),
                        Box::new(Expr::If(ExprIf {
                            meta_list: vec![],
                            keyword: (
                                KeywordIf(TokenStream {
                                    offset: 13,
                                    value: "if"
                                }),
                                S(TokenStream {
                                    offset: 15,
                                    value: " "
                                })
                            ),
                            expr: Box::new(Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 16,
                                    value: "b"
                                })
                            )),
                            if_branch: Block(Delimiter {
                                start: (
                                    Some(S(TokenStream {
                                        offset: 17,
                                        value: " "
                                    })),
                                    TokenLeftBrace(TokenStream {
                                        offset: 18,
                                        value: "{"
                                    }),
                                    None
                                ),
                                end: (
                                    None,
                                    TokenRightBrace(TokenStream {
                                        offset: 19,
                                        value: "}"
                                    }),
                                    Some(S(TokenStream {
                                        offset: 20,
                                        value: " "
                                    }))
                                ),
                                body: Stmts(vec![])
                            }),
                            else_branch: Some((
                                KeywordElse(TokenStream {
                                    offset: 21,
                                    value: "else"
                                }),
                                S(TokenStream {
                                    offset: 25,
                                    value: " "
                                }),
                                Box::new(Expr::Block(
                                    vec![],
                                    Block(Delimiter {
                                        start: (
                                            None,
                                            TokenLeftBrace(TokenStream {
                                                offset: 26,
                                                value: "{"
                                            }),
                                            None
                                        ),
                                        end: (
                                            None,
                                            TokenRightBrace(TokenStream {
                                                offset: 27,
                                                value: "}"
                                            }),
                                            None
                                        ),
                                        body: Stmts(vec![])
                                    })
                                ))
                            ))
                        }))
                    ))
                }),
                TokenStream {
                    offset: 28,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_loop() {
        assert_eq!(
            Expr::parse(TokenStream::from("loop { value }")),
            Ok((
                Expr::Loop(ExprLoop {
                    meta_list: vec![],
                    keyword: (
                        KeywordLoop(TokenStream {
                            offset: 0,
                            value: "loop"
                        }),
                        Some(S(TokenStream {
                            offset: 4,
                            value: " "
                        }))
                    ),
                    body: Block(Delimiter {
                        start: (
                            None,
                            TokenLeftBrace(TokenStream {
                                offset: 5,
                                value: "{"
                            }),
                            Some(S(TokenStream {
                                offset: 6,
                                value: " "
                            }))
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 13,
                                value: "}"
                            }),
                            None
                        ),
                        body: Stmts(vec![Stmt::Expr(
                            Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 7,
                                    value: "value"
                                })
                            ),
                            Some(S(TokenStream {
                                offset: 12,
                                value: " "
                            })),
                            None,
                            None
                        )])
                    })
                }),
                TokenStream {
                    offset: 14,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_while() {
        assert_eq!(
            Expr::parse(TokenStream::from("while true { value }")),
            Ok((
                Expr::While(ExprWhile {
                    meta_list: vec![],
                    keyword: (
                        KeywordWhile(TokenStream {
                            offset: 0,
                            value: "while"
                        }),
                        S(TokenStream {
                            offset: 5,
                            value: " "
                        })
                    ),
                    expr: Box::new(Expr::Lit(
                        vec![],
                        Lit::Bool(LitBool::True(TokenTrue(TokenStream {
                            offset: 6,
                            value: "true"
                        })))
                    )),
                    body: Block(Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 10,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 11,
                                value: "{"
                            }),
                            Some(S(TokenStream {
                                offset: 12,
                                value: " "
                            }))
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 19,
                                value: "}"
                            }),
                            None
                        ),
                        body: Stmts(vec![Stmt::Expr(
                            Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 13,
                                    value: "value"
                                })
                            ),
                            Some(S(TokenStream {
                                offset: 18,
                                value: " "
                            })),
                            None,
                            None
                        )])
                    })
                }),
                TokenStream {
                    offset: 20,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_match() {
        assert_eq!(
            Expr::parse(TokenStream::from(r#"match a { "a" => {} }"#)),
            Ok((
                Expr::Match(ExprMatch {
                    meta_list: vec![],
                    keyword: (
                        KeywordMatch(TokenStream {
                            offset: 0,
                            value: "match"
                        }),
                        S(TokenStream {
                            offset: 5,
                            value: " "
                        })
                    ),
                    expr: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 6,
                            value: "a"
                        })
                    )),
                    arms: Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 7,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 8,
                                value: "{"
                            }),
                            Some(S(TokenStream {
                                offset: 9,
                                value: " "
                            }))
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 20,
                                value: "}"
                            }),
                            None
                        ),
                        body: vec![Arm {
                            meta_list: vec![],
                            patt: Patt::Lit(Lit::String(LitStr(TokenStream {
                                offset: 11,
                                value: "a"
                            }))),
                            fat_arrow_token: (
                                Some(S(TokenStream {
                                    offset: 13,
                                    value: " "
                                })),
                                TokenFatArrow(TokenStream {
                                    offset: 14,
                                    value: "=>"
                                }),
                                Some(S(TokenStream {
                                    offset: 16,
                                    value: " "
                                }))
                            ),
                            body: Box::new(Expr::Block(
                                vec![],
                                Block(Delimiter {
                                    start: (
                                        None,
                                        TokenLeftBrace(TokenStream {
                                            offset: 17,
                                            value: "{"
                                        }),
                                        None
                                    ),
                                    end: (
                                        None,
                                        TokenRightBrace(TokenStream {
                                            offset: 18,
                                            value: "}"
                                        }),
                                        Some(S(TokenStream {
                                            offset: 19,
                                            value: " "
                                        }))
                                    ),
                                    body: Stmts(vec![])
                                })
                            )),
                            comma: None
                        }]
                    }
                }),
                TokenStream {
                    offset: 21,
                    value: ""
                }
            ))
        );
    }
}
