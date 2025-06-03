use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    tokens::{LeftBracket, RightBracket},
};

use super::Expr;

/// The right part of indexing expression
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Index<I>
where
    I: LangInput,
{
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

/// A square bracketed indexing expression: vector[2].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprIndex<I>
where
    I: LangInput,
{
    pub base: Box<Expr<I>>,
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprIndex, ExprLit, ExprPath},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        tokens::{Digits, Ident, LeftBracket, RightBracket},
    };

    #[test]
    fn test_index() {
        assert_eq!(
            Expr::parse(TokenStream::from("a[0]")),
            Ok((
                Expr::Index(ExprIndex {
                    base: Box::new(Expr::Path(ExprPath {
                        meta_list: Default::default(),
                        first: Ident(TokenStream::from("a")),
                        segments: vec![]
                    })),
                    delimiter_start: LeftBracket(TokenStream::from((1, "["))),
                    index: Box::new(Expr::Lit(ExprLit {
                        meta_list: Default::default(),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((2, "0")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })),
                    delimiter_end: RightBracket(TokenStream::from((3, "]")))
                }),
                TokenStream::from((4, ""))
            ))
        );
    }
}
