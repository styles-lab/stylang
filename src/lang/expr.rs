use parserc::derive_parse;

use super::{Eq, KeywordLet, Lit, MetaList, ParseError, Patt, S, StylangInput};

/// A local let binding: let x: u64 = 10.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub struct ExprLet<I>
where
    I: StylangInput,
{
    /// optional metadata list.
    pub meta_list: MetaList<I>,
    /// keyword `let`.
    pub let_token: (KeywordLet<I>, S<I>),
    /// let binding patt.
    pub patt: Patt<I>,
    /// equal token: `=`
    pub eq_token: (Option<S<I>>, Eq<I>, Option<S<I>>),
    /// init expr part.
    pub expr: Box<Expr<I>>,
}

/// A Rust expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError,input = I)]
pub enum Expr<I>
where
    I: StylangInput,
{
    Lit(MetaList<I>, Lit<I>),
    Let(ExprLet<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Digits, Eq, Expr, ExprLet, Ident, KeywordLet, LitNum, MetaList, Patt, PattPath, S,
        TokenStream, ty::TypePath,
    };

    #[test]
    fn test_let() {
        assert_eq!(
            Expr::parse(TokenStream::from("let a = 10")),
            Ok((
                Expr::Let(ExprLet {
                    meta_list: MetaList(vec![]),
                    let_token: (
                        KeywordLet(TokenStream::from("let")),
                        S(TokenStream::from((3, " ")))
                    ),
                    patt: Patt::Path(PattPath {
                        meta_list: MetaList(vec![]),
                        path: TypePath {
                            first: Ident(TokenStream::from((4, "a"))),
                            rest: vec![]
                        }
                    }),
                    eq_token: (
                        None,
                        Eq(TokenStream::from((6, "="))),
                        Some(S(TokenStream::from((7, " "))))
                    ),
                    expr: Box::new(Expr::Lit(
                        MetaList(vec![]),
                        crate::lang::Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((8, "10")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None
                        })
                    ))
                }),
                TokenStream::from((10, ""))
            ))
        );
    }
}
