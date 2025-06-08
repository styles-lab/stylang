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
    use parserc::{Delimiter, Parse};

    use crate::lang::{
        expr::{BinOp, Expr, ExprBinary, ExprBlock, ExprWhile},
        input::TokenStream,
        lit::{Lit, LitNum},
        meta::Meta,
        stmt::{Block, Stmts},
        token::*,
    };

    #[test]
    fn expr_while() {
        assert_eq!(
            Expr::parse(TokenStream::from("while a < 10 {}")),
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
                    cond: Box::new(Expr::Binary(ExprBinary {
                        start: Box::new(Expr::Ident(
                            Default::default(),
                            Ident(TokenStream {
                                offset: 6,
                                value: "a"
                            }),
                        ),),
                        rest: vec![(
                            BinOp::Lt(SepLt(
                                Some(S(TokenStream {
                                    offset: 7,
                                    value: " "
                                })),
                                TokenStream {
                                    offset: 8,
                                    value: "<"
                                },
                                Some(S(TokenStream {
                                    offset: 9,
                                    value: " "
                                }))
                            )),
                            Expr::Lit(
                                Default::default(),
                                Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream {
                                        offset: 10,
                                        value: "10"
                                    })),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            ),
                        )]
                    })),
                    body: ExprBlock {
                        meta_list: vec![Meta::S(S(TokenStream {
                            offset: 12,
                            value: " "
                        }))],
                        block: Block(Delimiter {
                            delimiter_start: SepLeftBrace(
                                None,
                                TokenStream {
                                    offset: 13,
                                    value: "{"
                                },
                                None
                            ),
                            body: Stmts(vec![]),
                            delimiter_end: SepRightBrace(
                                None,
                                TokenStream {
                                    offset: 14,
                                    value: "}"
                                },
                                None
                            )
                        })
                    }
                }),
                TokenStream {
                    offset: 15,
                    value: ""
                }
            ))
        );
    }
}
