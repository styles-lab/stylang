use parserc::{
    inputs::{SpanJoin, lang::LangInput},
    parser::Parser,
    syntax::{AsSpan, Syntax},
};

use crate::lang::{
    errors::{LangError, SyntaxKind},
    expr::Expr,
    meta::MetaList,
    patt::Patt,
    token::*,
};

/// One arm of a match expression: 0..=10 => { return true; }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Arm<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// arm pattern.
    pub patt: Patt<I>,
    /// token `=>`
    pub fat_arrow_token: (Option<S<I>>, TokenFatArrow<I>, Option<S<I>>),
    /// arm body,
    pub body: Box<Expr<I>>,
    /// optional comma separator.
    pub comma: Option<SepComma<I>>,
}

impl<I> AsSpan for Arm<I>
where
    I: LangInput,
{
    fn as_span(&self) -> Option<parserc::inputs::Span> {
        self.meta_list
            .as_span()
            .join(self.patt.as_span())
            .join(self.body.as_span())
            .join(self.comma.as_span())
    }
}

impl<I> Syntax<I, LangError> for Arm<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;
        let (meta_list, input) = MetaList::parse(input)?;
        let (patt, input) = Patt::parse(input)?;
        let (fat_arrow_token, input) = input.parse()?;

        let span = input.as_span().unwrap();

        let (body, input) = Expr::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::ArmBody, span))
            .fatal()
            .boxed()
            .parse(input)?;

        let (comma, input) = input.parse()?;

        Ok((
            Self {
                meta_list,
                patt,
                fat_arrow_token,
                body,
                comma,
            },
            input,
        ))
    }
}

/// A `match` pattern expr.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprMatch<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `match`.
    pub keyword: (KeywordMatch<I>, S<I>),
    /// target expr.
    pub expr: Box<Expr<I>>,
    /// arms delimited by a group of braces: `{ xxx => xxx, ...}`
    pub arms: Brace<I, Vec<Arm<I>>>,
}

impl<I> AsSpan for ExprMatch<I>
where
    I: LangInput,
{
    fn as_span(&self) -> Option<parserc::inputs::Span> {
        self.meta_list
            .as_span()
            .join(self.keyword.as_span())
            .join(self.arms.as_span())
    }
}

impl<I> Syntax<I, LangError> for ExprMatch<I>
where
    I: LangInput,
{
    fn parse(input: I) -> parserc::errors::Result<Self, I, LangError> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (keyword, input) = <(KeywordMatch<_>, S<_>)>::parse(input)?;
        let span = input.as_span().unwrap();
        let mut expr_input = input.clone();
        let (expr, mut input) = Expr::into_parser()
            .map(|v| Expr::from(v))
            .map_err(|_| LangError::expect(SyntaxKind::MatchExpr, span))
            .fatal()
            .parse(input)?;

        let expr = match expr {
            Expr::Struct(expr) => {
                input = expr_input.split_off(expr.ty.as_span().unwrap().len);
                expr.ty
            }
            expr => Box::new(expr),
        };

        let span = input.as_span().unwrap();
        let (arms, input) = Brace::into_parser()
            .map_err(|_| LangError::expect(SyntaxKind::MatchArms, span))
            .fatal()
            .parse(input)?;

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
        inputs::lang::TokenStream,
        syntax::{Delimiter, Punctuated, Syntax},
    };

    use crate::lang::{
        expr::{Expr, ExprPath, PathCall, PathField, PathSegment},
        meta::Meta,
        ty::TypePath,
    };

    use super::*;

    #[test]
    fn test_match2() {
        assert_eq!(
            Expr::parse(TokenStream::from(r#"match A::b().1 {}"#)),
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
                    expr: Box::new(Expr::Path(ExprPath {
                        first: Box::new(Expr::TypePath(
                            vec![],
                            TypePath {
                                first: Ident(TokenStream {
                                    offset: 6,
                                    value: "A"
                                }),
                                rest: vec![(
                                    None,
                                    TokenColonColon(TokenStream {
                                        offset: 7,
                                        value: "::"
                                    }),
                                    None,
                                    Ident(TokenStream {
                                        offset: 9,
                                        value: "b"
                                    })
                                )]
                            }
                        )),
                        rest: vec![
                            PathSegment::Call(PathCall(Delimiter {
                                start: (
                                    None,
                                    TokenLeftParen(TokenStream {
                                        offset: 10,
                                        value: "("
                                    }),
                                    None
                                ),
                                end: (
                                    None,
                                    TokenRightParen(TokenStream {
                                        offset: 11,
                                        value: ")"
                                    }),
                                    None
                                ),
                                body: Punctuated {
                                    pairs: vec![],
                                    tail: None
                                }
                            })),
                            PathSegment::Field {
                                dot_sep: (
                                    None,
                                    TokenDot(TokenStream {
                                        offset: 12,
                                        value: "."
                                    }),
                                    None
                                ),
                                field: PathField::Uname(Digits(TokenStream {
                                    offset: 13,
                                    value: "1"
                                }))
                            }
                        ]
                    })),
                    arms: Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 14,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 15,
                                value: "{"
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 16,
                                value: "}"
                            }),
                            None
                        ),
                        body: vec![]
                    }
                }),
                TokenStream {
                    offset: 17,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn test_match() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                r#"
                match a {
                }
                "#
            )),
            Ok((
                Expr::Match(ExprMatch {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n                "
                    }))],
                    keyword: (
                        KeywordMatch(TokenStream {
                            offset: 17,
                            value: "match"
                        }),
                        S(TokenStream {
                            offset: 22,
                            value: " "
                        })
                    ),
                    expr: Box::new(Expr::Ident(
                        vec![],
                        Ident(TokenStream {
                            offset: 23,
                            value: "a"
                        })
                    )),
                    arms: Delimiter {
                        start: (
                            Some(S(TokenStream {
                                offset: 24,
                                value: " "
                            })),
                            TokenLeftBrace(TokenStream {
                                offset: 25,
                                value: "{"
                            }),
                            Some(S(TokenStream {
                                offset: 26,
                                value: "\n                "
                            }))
                        ),
                        end: (
                            None,
                            TokenRightBrace(TokenStream {
                                offset: 43,
                                value: "}"
                            }),
                            Some(S(TokenStream {
                                offset: 44,
                                value: "\n                "
                            }))
                        ),
                        body: vec![]
                    }
                }),
                TokenStream {
                    offset: 61,
                    value: ""
                }
            ))
        );
    }
}
