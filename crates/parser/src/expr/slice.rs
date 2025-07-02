use parserc::{
    errors::{ControlFlow, MapFatal as _},
    lang::LangInput,
    parser::{Parser, take_till},
    span::ToSpan,
    syntax::{PartialSyntax, Punctuated, Syntax},
};

use crate::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    meta::MetaList,
    token::*,
};

/// A slice initialization expression: `[expr:num]` or `[expr1,expr2,..]`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[syntax(error = LangError)]
pub enum ExprSlice<I>
where
    I: LangInput,
{
    /// repeat expr: `[a;num]`
    Repeat {
        /// optional meta-data list.
        meta_list: MetaList<I>,
        /// delimited [expr:num]
        bracket: Bracket<I, (Box<Expr<I>>, SepSemiColon<I>, Digits<I>)>,
    },
    InitList {
        /// optional meta-data list.
        meta_list: MetaList<I>,
        /// delimited [expr,expr,...]
        bracket: Bracket<I, Punctuated<Expr<I>, SepComma<I>>>,
    },
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, TokenLeftBracket<I>)> for ExprSlice<I>
where
    I: LangInput,
{
    fn parse_with_prefix(
        (meta_list, token_left_bracket): (MetaList<I>, TokenLeftBracket<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        let (s, input) = S::into_parser().ok().parse(input)?;
        let start = (None, token_left_bracket.clone(), s);

        let (_, lookahead) = take_till(|c: u8| c == b';' || c == b',').parse(input.clone())?;

        match lookahead.iter().next() {
            Some(b';') => {
                let (body, input) = input.parse().fatal()?;
                let (end, input) = input.parse().fatal()?;
                Ok((
                    Self::Repeat {
                        meta_list,
                        bracket: Bracket { start, end, body },
                    },
                    input,
                ))
            }
            Some(b',') => {
                let (body, input) = input.parse().fatal()?;
                let (end, input) = input.parse().fatal()?;
                Ok((
                    Self::InitList {
                        meta_list,
                        bracket: Bracket { start, end, body },
                    },
                    input,
                ))
            }
            _ => Err(ControlFlow::Fatal(LangError::invalid(
                SyntaxKind::ExprSlice,
                meta_list.to_span() ^ token_left_bracket.to_span() ^ input.to_span(),
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        lang::{Span, TokenStream},
        syntax::{Delimiter, Syntax},
    };

    use crate::lit::{Lit, LitNum};

    use super::*;

    #[test]
    fn test_repeat() {
        assert_eq!(
            Expr::parse(TokenStream::from("[10;20]")),
            Ok((
                Expr::Slice(ExprSlice::Repeat {
                    meta_list: vec![],
                    bracket: Delimiter {
                        start: (
                            None,
                            TokenLeftBracket(TokenStream {
                                offset: 0,
                                value: "["
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBracket(TokenStream {
                                offset: 6,
                                value: "]"
                            }),
                            None
                        ),
                        body: (
                            Box::new(Expr::Lit(
                                vec![],
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 1,
                                        value: "10"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            )),
                            (
                                None,
                                TokenSemiColon(TokenStream {
                                    offset: 3,
                                    value: ";"
                                }),
                                None
                            ),
                            Digits(TokenStream {
                                offset: 4,
                                value: "20"
                            })
                        )
                    }
                }),
                TokenStream {
                    offset: 7,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_init_list() {
        assert_eq!(
            Expr::parse(TokenStream::from("[1,2,3,4]")),
            Ok((
                Expr::Slice(ExprSlice::InitList {
                    meta_list: vec![],
                    bracket: Delimiter {
                        start: (
                            None,
                            TokenLeftBracket(TokenStream {
                                offset: 0,
                                value: "["
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBracket(TokenStream {
                                offset: 8,
                                value: "]"
                            }),
                            None
                        ),
                        body: Punctuated {
                            pairs: vec![
                                (
                                    Expr::Lit(
                                        vec![],
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 1,
                                                value: "1"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ),
                                    (
                                        None,
                                        TokenComma(TokenStream {
                                            offset: 2,
                                            value: ","
                                        }),
                                        None
                                    )
                                ),
                                (
                                    Expr::Lit(
                                        vec![],
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 3,
                                                value: "2"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ),
                                    (
                                        None,
                                        TokenComma(TokenStream {
                                            offset: 4,
                                            value: ","
                                        }),
                                        None
                                    )
                                ),
                                (
                                    Expr::Lit(
                                        vec![],
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 5,
                                                value: "3"
                                            })),
                                            dot: None,
                                            fract: None,
                                            exp: None,
                                            unit: None
                                        })
                                    ),
                                    (
                                        None,
                                        TokenComma(TokenStream {
                                            offset: 6,
                                            value: ","
                                        }),
                                        None
                                    )
                                )
                            ],
                            tail: Some(Box::new(Expr::Lit(
                                vec![],
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 7,
                                        value: "4"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            )))
                        }
                    }
                }),
                TokenStream {
                    offset: 9,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn error_detect() {
        assert_eq!(
            Expr::parse(TokenStream::from("[10]")),
            Err(ControlFlow::Fatal(LangError::Invalid {
                kind: SyntaxKind::ExprSlice,
                span: Span::Some { start: 0, end: 4 },
                item: None
            }))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("[10,10;10]")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: SyntaxKind::Token("]"),
                span: Span::Some { start: 6, end: 10 },
                item: None
            }))
        );

        assert_eq!(
            Expr::parse(TokenStream::from("[10;10,19]")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: SyntaxKind::Token("]"),
                span: Span::Some { start: 6, end: 10 },
                item: None
            }))
        );
    }
}
