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
        expr::{Expr, ExprBlock, ExprIf, ExprPath, PathCall, PathSegment, PathStart},
        input::TokenStream,
        stmt::{Block, Stmts},
        token::*,
        ty::TypePath,
    };

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
