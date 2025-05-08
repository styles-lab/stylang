use super::{Expr, LeftBracket, RightBracket, StylangInput};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIndex<I>
where
    I: StylangInput,
{
    pub expr: Box<Expr<I>>,
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        Digits, Dot, Expr, ExprField, ExprIndex, ExprLit, ExprPath, Ident, LeftBracket, Lit,
        LitNum, Member, MetaList, RightBracket, TokenStream,
    };

    #[test]
    fn test_index() {
        assert_eq!(
            Expr::parse(TokenStream::from("a.b[1]")),
            Ok((
                Expr::Index(ExprIndex {
                    expr: Box::new(Expr::Field(ExprField {
                        target: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from("a")),
                            tails: vec![]
                        })),
                        dot_token: Dot(TokenStream::from((1, "."))),
                        member: Member::Named(Ident(TokenStream::from((2, "b"))))
                    })),
                    delimiter_start: LeftBracket(TokenStream::from((3, "["))),
                    index: Box::new(Expr::Lit(ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((4, "1")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: None,
                        })
                    })),
                    delimiter_end: RightBracket(TokenStream::from((5, "]")))
                }),
                TokenStream::from((6, ""))
            ))
        );
    }
}
