use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{LeftParen, RightParen},
};

use super::Expr;

/// A parenthesized expression: (a + b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprParen<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token `(`
    pub delimiter_start: LeftParen<I>,
    /// inner expr.
    pub expr: Box<Expr<I>>,
    /// delimiter end token `)`
    pub delimiter_end: RightParen<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{BinOp, Expr, ExprBinary, ExprParen, ExprPath},
        inputs::TokenStream,
        meta::MetaList,
        tokens::{Ident, LeftParen, Plus, RightParen},
    };

    #[test]
    fn test_paren() {
        assert_eq!(
            Expr::parse(TokenStream::from("(a + b)")),
            Ok((
                Expr::Paren(ExprParen {
                    meta_list: MetaList(vec![]),
                    delimiter_start: LeftParen(TokenStream::from("(")),
                    expr: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((1, "a"))),
                            segments: vec![]
                        })),
                        op: BinOp::Add(Plus(TokenStream::from((3, "+")))),
                        right: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from((5, "b"))),
                            segments: vec![]
                        }))
                    })),
                    delimiter_end: RightParen(TokenStream::from((6, ")")))
                }),
                TokenStream::from((7, ""))
            ))
        );
    }
}
