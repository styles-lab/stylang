use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    expr::{Expr, ExprBlock},
    input::LangInput,
    meta::MetaList,
    token::{KeywordElse, KeywordIf, KeywordLoop, KeywordWhile},
};

/// An if expression with an optional else block: if expr { ... } else { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `if`
    pub keyword: KeywordIf<I>,
    /// condition expr.
    #[fatal]
    pub cond: Box<Expr<I>>,
    /// then branch.
    #[fatal]
    pub then_branch: ExprBlock<I>,
    /// else branch.
    pub else_branch: Option<(KeywordElse<I>, Box<Expr<I>>)>,
}

/// Conditionless loop: loop { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub keyword: KeywordLoop<I>,
    /// A group of stmts as the body of the loop.
    #[fatal]
    pub body: ExprBlock<I>,
}

/// Condition loop: while $expr { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprWhile<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub keyword: KeywordWhile<I>,
    /// condition expr.
    #[fatal]
    pub cond: Box<Expr<I>>,
    /// A group of stmts as the body of the loop.
    #[fatal]
    pub body: ExprBlock<I>,
}

#[cfg(test)]
mod tests {
    use parserc::{Delimiter, Parse, Punctuated};

    use crate::lang::{
        expr::{
            BitsOp, CompOp, Expr, ExprBits, ExprBlock, ExprComp, ExprIf, ExprPath, ExprWhile,
            PathCall, PathSegment, PathStart,
        },
        input::TokenStream,
        lit::{Lit, LitNum},
        meta::Meta,
        stmt::{Block, Stmts},
        token::*,
        ty::TypePath,
    };

    #[test]
    fn expr_while() {
        assert_eq!(
            Expr::parse(TokenStream::from("while a ^ 4 != 0 {}")),
            Ok((
                Expr::While(ExprWhile {
                    meta_list: vec![],
                    keyword: KeywordWhile(
                        TokenStream {
                            offset: 0,
                            value: "while"
                        },
                        S(TokenStream {
                            offset: 5,
                            value: " "
                        })
                    ),
                    cond: Box::new(Expr::Comp(ExprComp {
                        meta_list: vec![],
                        left_operand: Box::new(Expr::Bits(ExprBits {
                            meta_list: vec![],
                            left_operand: Box::new(Expr::Ident(
                                vec![],
                                Ident(TokenStream {
                                    offset: 6,
                                    value: "a"
                                })
                            )),
                            op: BitsOp::BitXor(SepCaret(
                                Some(S(TokenStream {
                                    offset: 7,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 8,
                                    value: "^"
                                },
                                Some(S(TokenStream {
                                    offset: 9,
                                    value: " "
                                }))
                            )),
                            right_operand: Box::new(Expr::Lit(
                                vec![],
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 10,
                                        value: "4"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            ))
                        })),
                        op: CompOp::Ne(SepNe(
                            Some(S(TokenStream {
                                offset: 11,
                                value: " "
                            })),
                            TokenStream {
                                offset: 12,
                                value: "!="
                            },
                            Some(S(TokenStream {
                                offset: 14,
                                value: " "
                            }))
                        )),
                        right_operand: Box::new(Expr::Lit(
                            vec![],
                            Lit::Num(LitNum {
                                sign: None,
                                trunc: Some(Digits(TokenStream {
                                    offset: 15,
                                    value: "0"
                                })),
                                dot: None,
                                fract: None,
                                exp: None,
                                unit: None
                            })
                        ))
                    })),
                    body: ExprBlock {
                        meta_list: vec![Meta::S(S(TokenStream {
                            offset: 16,
                            value: " "
                        }))],
                        block: Block(Delimiter {
                            delimiter_start: SepLeftBrace(
                                None,
                                TokenStream {
                                    offset: 17,
                                    value: "{"
                                },
                                None
                            ),
                            body: Stmts(vec![]),
                            delimiter_end: SepRightBrace(
                                None,
                                TokenStream {
                                    offset: 18,
                                    value: "}"
                                },
                                None
                            )
                        })
                    }
                }),
                TokenStream {
                    offset: 19,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn expr_if() {
        assert_eq!(
            Expr::parse(TokenStream::from("if web3::is_connected() {}")),
            Ok((
                Expr::If(ExprIf {
                    meta_list: vec![],
                    keyword: KeywordIf(
                        TokenStream {
                            offset: 0,
                            value: "if"
                        },
                        S(TokenStream {
                            offset: 2,
                            value: " "
                        })
                    ),
                    cond: Box::new(Expr::Path(ExprPath {
                        first: PathStart::TypePath(
                            vec![],
                            TypePath {
                                first: Ident(TokenStream {
                                    offset: 3,
                                    value: "web3"
                                }),
                                rest: vec![(
                                    SepColonColon(
                                        None,
                                        TokenStream {
                                            offset: 7,
                                            value: "::"
                                        },
                                        None
                                    ),
                                    Ident(TokenStream {
                                        offset: 9,
                                        value: "is_connected"
                                    })
                                )]
                            }
                        ),
                        rest: vec![PathSegment::Call(PathCall(Delimiter {
                            delimiter_start: SepLeftParen(
                                None,
                                TokenStream {
                                    offset: 21,
                                    value: "("
                                },
                                None
                            ),
                            body: Punctuated {
                                pairs: vec![],
                                tail: None
                            },
                            delimiter_end: SepRightParen(
                                None,
                                TokenStream {
                                    offset: 22,
                                    value: ")"
                                },
                                Some(S(TokenStream {
                                    offset: 23,
                                    value: " "
                                }))
                            )
                        }))]
                    })),
                    then_branch: ExprBlock {
                        meta_list: vec![],
                        block: Block(Delimiter {
                            delimiter_start: SepLeftBrace(
                                None,
                                TokenStream {
                                    offset: 24,
                                    value: "{"
                                },
                                None
                            ),
                            body: Stmts(vec![]),
                            delimiter_end: SepRightBrace(
                                None,
                                TokenStream {
                                    offset: 25,
                                    value: "}"
                                },
                                None
                            )
                        })
                    },
                    else_branch: None
                }),
                TokenStream {
                    offset: 26,
                    value: ""
                }
            ))
        );
    }
}
