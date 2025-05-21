use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    punct::Punctuated,
    tokens::{Comma, LeftBracket, RightBracket},
};

use super::Expr;

/// A slice literal expression: [a, b, c, d].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprArray<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token `[`
    pub delimiter_start: LeftBracket<I>,
    /// inner element list.
    pub els: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token `]`
    pub delimiter_end: RightBracket<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{ChainInit, Expr, ExprArray, ExprChain, ExprPath},
        inputs::TokenStream,
        meta::MetaList,
        punct::Punctuated,
        tokens::{Comma, Ident, LeftBracket, RightBracket},
    };

    #[test]
    fn test_array() {
        assert_eq!(
            Expr::parse(TokenStream::from("[a,b,c,]")),
            Ok((
                Expr::Array(ExprArray {
                    meta_list: MetaList(vec![]),
                    delimiter_start: LeftBracket(TokenStream::from("[")),
                    els: Punctuated {
                        items: vec![
                            (
                                Expr::Chain(ExprChain {
                                    start: ChainInit::Path(ExprPath {
                                        meta_list: MetaList(vec![]),
                                        first: Ident(TokenStream::from((1, "a"))),
                                        segments: vec![]
                                    }),
                                    segments: vec![]
                                }),
                                Comma(TokenStream::from((2, ",")))
                            ),
                            (
                                Expr::Chain(ExprChain {
                                    start: ChainInit::Path(ExprPath {
                                        meta_list: MetaList(vec![]),
                                        first: Ident(TokenStream::from((3, "b"))),
                                        segments: vec![]
                                    }),
                                    segments: vec![]
                                }),
                                Comma(TokenStream::from((4, ",")))
                            ),
                            (
                                Expr::Chain(ExprChain {
                                    start: ChainInit::Path(ExprPath {
                                        meta_list: MetaList(vec![]),
                                        first: Ident(TokenStream::from((5, "c"))),
                                        segments: vec![]
                                    }),
                                    segments: vec![]
                                }),
                                Comma(TokenStream::from((6, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightBracket(TokenStream::from((7, "]")))
                }),
                TokenStream::from((8, ""))
            ))
        );
    }
}
