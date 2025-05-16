use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{LeftBracket, RightBracket, S},
};

use super::{Expr, parse::PartialParse};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIndex<I>
where
    I: LangInput,
{
    pub expr: Box<Expr<I>>,
    pub meta_list: MetaList<I>,
    pub delimiter_start: LeftBracket<I>,
    pub index: Box<Expr<I>>,
    pub delimiter_end: RightBracket<I>,
}
impl<I> PartialParse<I> for ExprIndex<I>
where
    I: LangInput,
{
    type LeadingToken = LeftBracket<I>;

    fn partial_parse(
        meta_list: crate::lang::meta::MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, LangError> {
        let (index, input) = Expr::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::ExprIndex, input.span()))
            .fatal()
            .parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;
        let (delimiter_end, input) = RightBracket::parse(input)?;

        Ok((
            Expr::Index(Self {
                meta_list,
                expr: Box::new(parsed),
                delimiter_start: leading_token,
                index: Box::new(index),
                delimiter_end,
            }),
            input,
        ))
    }
}

#[cfg(test)]
mod tests {

    use parserc::Parse;

    use crate::lang::{
        expr::{Expr, ExprField, ExprIndex, ExprLit, ExprPath, Member},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        meta::MetaList,
        tokens::{Digits, Dot, Ident, LeftBracket, RightBracket},
    };

    #[test]
    fn test_index() {
        assert_eq!(
            Expr::parse(TokenStream::from("a.b[1]")),
            Ok((
                Expr::Index(ExprIndex {
                    expr: Box::new(Expr::Field(ExprField {
                        meta_list: MetaList(vec![]),
                        target: Box::new(Expr::Path(ExprPath {
                            meta_list: MetaList(vec![]),
                            first: Ident(TokenStream::from("a")),
                            segments: vec![]
                        })),
                        dot_token: Dot(TokenStream::from((1, "."))),
                        member: Member::Named(Ident(TokenStream::from((2, "b"))))
                    })),
                    meta_list: MetaList(vec![]),
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
