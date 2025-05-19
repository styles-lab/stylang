use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{KeywordElse, KeywordIf, S},
};

use super::{Expr, ExprBlock};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// token `if`
    pub if_token: KeywordIf<I>,
    /// if condition expr.
    pub cond: Box<Expr<I>>,
    /// required then code block.
    pub then_branch: ExprBlock<I>,
    /// optional else block.
    pub else_branch: Option<(Option<S<I>>, KeywordElse<I>, Option<S<I>>, Box<Expr<I>>)>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Block, ExprBlock, ExprCall, ExprPath, PathSegment},
        inputs::TokenStream,
        meta::{Comment, LineComment, Meta},
        punct::Punctuated,
        stmt::Stmts,
        tokens::{Ident, LeftBrace, LeftParen, PathSep, RightBrace, RightParen},
    };

    use super::*;

    #[test]
    fn test_expr_if() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                r#"
                // core mod `web3`.
                if web3::is_connected() {
                } else {
                }
            "#,
            )),
            Ok((
                Expr::If(ExprIf {
                    meta_list: MetaList(vec![Meta::Comment(Comment::LineComment(LineComment(
                        TokenStream::from((19, " core mod `web3`."))
                    )))]),
                    if_token: KeywordIf(TokenStream::from((53, "if"))),
                    cond: Box::new(Expr::Call(ExprCall {
                        target: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((56, "web3"))),
                            segments: vec![PathSegment {
                                sep: (None, PathSep(TokenStream::from((60, "::"))), None),

                                ident: Ident(TokenStream::from((62, "is_connected")))
                            }]
                        })),
                        meta_list: MetaList(vec![]),
                        delimiter_start: LeftParen(TokenStream::from((74, "("))),
                        params: Punctuated {
                            items: vec![],
                            last: None
                        },
                        delimiter_end: RightParen(TokenStream::from((75, ")")))
                    })),
                    then_branch: ExprBlock {
                        meta_list: MetaList(vec![]),
                        block: Block {
                            delimiter_start: LeftBrace(TokenStream::from((77, "{"))),
                            stmts: Stmts(vec![]),
                            meta_list: MetaList(vec![]),
                            delimiter_end: RightBrace(TokenStream::from((95, "}")))
                        }
                    },
                    else_branch: Some((
                        Some(S(TokenStream::from((96, " ")))),
                        KeywordElse(TokenStream::from((97, "else"))),
                        Some(S(TokenStream::from((101, " ")))),
                        Box::new(Expr::Block(ExprBlock {
                            meta_list: MetaList(vec![]),
                            block: Block {
                                delimiter_start: LeftBrace(TokenStream::from((102, "{"))),
                                stmts: Stmts(vec![]),
                                meta_list: MetaList(vec![]),
                                delimiter_end: RightBrace(TokenStream::from((120, "}")))
                            }
                        }))
                    ))
                }),
                TokenStream::from((121, "\n            "))
            ))
        );
    }
}
