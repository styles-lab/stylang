use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    punct::Punctuated,
    tokens::{Comma, LeftParen, RightParen},
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

/// A local let binding: let x: u64 = 10.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprTuple<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start : `(`
    pub delimiter_start: LeftParen<I>,
    /// tuple elems list.
    pub elms: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end : `)`
    pub delimiter_end: RightParen<I>,
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        expr::{BinOp, Expr, ExprBinary, ExprLit, ExprParen, ExprPath, ExprTuple},
        inputs::TokenStream,
        lit::{Lit, LitNum, NumUnit},
        meta::MetaList,
        punct::Punctuated,
        tokens::{Comma, Digits, F32, I32, Ident, LeftParen, Plus, RightParen},
    };

    #[test]
    fn test_paren() {
        assert_eq!(
            Expr::parse(TokenStream::from("(a + b)")),
            Ok((
                Expr::Paren(ExprParen {
                    meta_list: MetaList::default(),
                    delimiter_start: LeftParen(TokenStream::from("(")),
                    expr: Box::new(Expr::Binary(ExprBinary {
                        left: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList::default(),
                            first: Ident(TokenStream::from((1, "a"))),
                            segments: vec![]
                        })),
                        op: BinOp::Add(Plus(TokenStream::from((3, "+")))),
                        right: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList::default(),
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

    #[test]
    fn test_tuple() {
        assert_eq!(
            Expr::parse(TokenStream::from("(1i32,2f32,)")),
            Ok((
                Expr::Tuple(ExprTuple {
                    meta_list: Default::default(),
                    delimiter_start: LeftParen(TokenStream::from("(")),
                    elms: Punctuated {
                        items: vec![
                            (
                                Expr::Lit(ExprLit {
                                    meta_list: Default::default(),
                                    lit: Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream::from((1, "1")))),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: Some(NumUnit::I32(I32(TokenStream::from((
                                            2, "i32"
                                        ))))),
                                    })
                                }),
                                Comma(TokenStream::from((5, ",")))
                            ),
                            (
                                Expr::Lit(ExprLit {
                                    meta_list: Default::default(),
                                    lit: Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream::from((6, "2")))),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: Some(NumUnit::F32(F32(TokenStream::from((
                                            7, "f32"
                                        ))))),
                                    })
                                }),
                                Comma(TokenStream::from((10, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightParen(TokenStream::from((11, ")")))
                }),
                TokenStream::from((12, ""))
            ))
        );
    }
}
