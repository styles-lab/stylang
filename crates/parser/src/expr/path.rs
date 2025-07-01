use parserc::{
    lang::LangInput,
    syntax::{Punctuated, Syntax},
};

use crate::{errors::LangError, expr::Expr, token::*};

/// `Path` call segement parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PathCall<I>(pub Paren<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// index parser for `Path` expr.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct PathIndex<I>(pub Bracket<I, Punctuated<Expr<I>, SepComma<I>>>)
where
    I: LangInput;

/// `Path` field index parser.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum PathField<I>
where
    I: LangInput,
{
    Name {
        /// `.`
        dot_sep: (Option<S<I>>, TokenDot<I>, Option<S<I>>),
        /// a,b,...
        name: Ident<I>,
    },
    Uname {
        /// `.`
        dot_sep: (Option<S<I>>, TokenDot<I>, Option<S<I>>),
        /// 1,10,..
        index: Digits<I>,
    },
}

/// The rest element parser for expr `Path`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum PathSegment<I>
where
    I: LangInput,
{
    Field(PathField<I>),
    Call(PathCall<I>),
    Index(PathIndex<I>),
}

/// The rest element parser for expr `Path`.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprPath<I>
where
    I: LangInput,
{
    /// leading expression.
    pub expr: Box<Expr<I>>,
    /// path semgents.
    pub segment: PathSegment<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{
        lang::TokenStream,
        syntax::{Delimiter, Syntax},
    };

    use crate::{
        expr::{Expr, ExprTerm, TermOp},
        lit::{Lit, LitNum},
    };

    use super::*;

    #[test]
    fn test_path() {
        assert_eq!(
            Expr::parse(TokenStream::from("a(1,2) + 2")),
            Ok((
                Expr::Term(ExprTerm {
                    left: Box::new(Expr::Path(ExprPath {
                        expr: Box::new(Expr::Ident(
                            vec![],
                            Ident(TokenStream {
                                offset: 0,
                                value: "a"
                            })
                        )),
                        segment: PathSegment::Call(PathCall(Delimiter {
                            start: (
                                None,
                                TokenLeftParen(TokenStream {
                                    offset: 1,
                                    value: "("
                                }),
                                None
                            ),
                            end: (
                                None,
                                TokenRightParen(TokenStream {
                                    offset: 5,
                                    value: ")"
                                }),
                                Some(S(TokenStream {
                                    offset: 6,
                                    value: " "
                                }))
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    Expr::Lit(
                                        vec![],
                                        Lit::Num(LitNum {
                                            sign: None,
                                            trunc: Some(Digits(TokenStream {
                                                offset: 2,
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
                                            offset: 3,
                                            value: ","
                                        }),
                                        None
                                    )
                                )],
                                tail: Some(Box::new(Expr::Lit(
                                    vec![],
                                    Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream {
                                            offset: 4,
                                            value: "2"
                                        })),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: None
                                    })
                                )))
                            }
                        }))
                    })),
                    op: TermOp::Add(
                        None,
                        TokenPlus(TokenStream {
                            offset: 7,
                            value: "+"
                        }),
                        Some(S(TokenStream {
                            offset: 8,
                            value: " "
                        }))
                    ),
                    right: Box::new(Expr::Lit(
                        vec![],
                        Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream {
                                offset: 9,
                                value: "2"
                            })),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ))
                }),
                TokenStream {
                    offset: 10,
                    value: ""
                }
            ))
        );
    }
}
