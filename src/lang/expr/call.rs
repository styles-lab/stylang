use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    punct::Punctuated,
    tokens::{Comma, LeftParen, RightParen},
};

use super::Expr;

/// The right part of the function call expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Call<I>
where
    I: LangInput,
{
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// params list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

/// The right part of the function call expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprCall<I>
where
    I: LangInput,
{
    /// call target expr.
    pub target: Box<Expr<I>>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParen<I>,
    /// params list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParen<I>,
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprCall, ExprPath},
        inputs::TokenStream,
        meta::MetaList,
        path::PathSegment,
        punct::Punctuated,
        tokens::{Ident, LeftParen, PathSep, RightParen},
    };

    #[test]
    fn test_call() {
        assert_eq!(
            Expr::parse(TokenStream::from("web3::is_connected()")),
            Ok((
                Expr::Call(ExprCall {
                    target: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList::default(),
                        first: Ident(TokenStream::from("web3")),
                        segments: vec![PathSegment {
                            sep: (None, PathSep(TokenStream::from((4, "::"))), None),
                            ident: Ident(TokenStream::from((6, "is_connected")))
                        }]
                    })),
                    delimiter_start: LeftParen(TokenStream::from((18, "("))),
                    params: Punctuated {
                        items: vec![],
                        last: None
                    },
                    delimiter_end: RightParen(TokenStream::from((19, ")")))
                }),
                TokenStream::from((20, ""))
            ))
        );
    }
}
