use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    patt::Patt,
    tokens::{Eq, KeywordLet, S},
};

use super::Expr;

/// A local let binding: let x: u64 = 10.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLet<I>
where
    I: LangInput,
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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use super::*;
    use crate::lang::{
        expr::ExprLit,
        inputs::TokenStream,
        lit::Lit,
        meta::{Attr, Meta},
        patt::PattType,
        tokens::{At, Colon, Ident, KeywordNone, KeywordString},
        ty::Type,
    };

    #[test]
    fn test_let() {
        assert_eq!(
            Expr::parse(TokenStream::from(
                "let @state @option value: string = none;",
            )),
            Ok((
                Expr::Let(ExprLet {
                    meta_list: MetaList::default(),
                    let_token: (
                        KeywordLet(TokenStream::from((0, "let"))),
                        S(TokenStream::from((3, " ")))
                    ),
                    patt: Patt::Type(PattType {
                        meta_list: MetaList(vec![
                            Meta::Attr(Attr {
                                keyword: At(TokenStream::from((4, "@"))),
                                ident: (
                                    Ident(TokenStream::from((5, "state"))),
                                    Some(S(TokenStream::from((10, " "))))
                                ),
                                params: None
                            }),
                            Meta::Attr(Attr {
                                keyword: At(TokenStream::from((11, "@"))),
                                ident: (
                                    Ident(TokenStream::from((12, "option"))),
                                    Some(S(TokenStream::from((18, " "))))
                                ),
                                params: None
                            })
                        ]),
                        name: Ident(TokenStream::from((19, "value"))),
                        colon_token: (
                            None,
                            Colon(TokenStream::from((24, ":"))),
                            Some(S(TokenStream::from((25, " "))))
                        ),
                        ty: Box::new(Type::String(KeywordString(TokenStream::from((
                            26, "string"
                        )))))
                    }),
                    eq_token: (
                        Some(S(TokenStream::from((32, " ")))),
                        Eq(TokenStream::from((33, "="))),
                        Some(S(TokenStream::from((34, " "))))
                    ),
                    expr: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList::default(),
                        lit: Lit::None(KeywordNone(TokenStream::from((35, "none"))))
                    }))
                }),
                TokenStream::from((39, ";"))
            ))
        );
    }
}
