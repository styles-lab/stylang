use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    punct::Punctuated,
    tokens::{Comma, LeftParenthesis, RightParenthesis, S},
};

use super::{Expr, parse::PartialParse};

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprCall<I>
where
    I: LangInput,
{
    /// target expr.
    pub target: Box<Expr<I>>,
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParenthesis<I>,
    /// params list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParenthesis<I>,
}

impl<I> PartialParse<I> for ExprCall<I>
where
    I: LangInput,
{
    type LeadingToken = LeftParenthesis<I>;

    fn partial_parse(
        meta_list: crate::lang::meta::MetaList<I>,
        parsed: Expr<I>,
        leading_token: Self::LeadingToken,
        input: I,
    ) -> parserc::Result<Expr<I>, I, LangError> {
        let (params, input) = Punctuated::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (delimiter_end, input) = RightParenthesis::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;

        Ok((
            Expr::Call(Self {
                meta_list,
                target: Box::new(parsed),
                delimiter_start: leading_token,
                params,
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
        expr::{Expr, ExprCall, ExprLit, ExprPath},
        inputs::TokenStream,
        lit::{Lit, LitNum, LitStr},
        meta::MetaList,
        punct::Punctuated,
        tokens::{Comma, Digits, Ident, LeftParenthesis, RightParenthesis},
    };

    #[test]
    fn test_call() {
        assert_eq!(
            Expr::parse(TokenStream::from(r#"a(1,"hello")"#)),
            Ok((
                Expr::Call(ExprCall {
                    meta_list: MetaList(vec![]),
                    target: Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList(vec![]),
                        first: Ident(TokenStream::from("a")),
                        segments: vec![]
                    })),
                    delimiter_start: LeftParenthesis(TokenStream::from((1, "("))),
                    params: Punctuated {
                        items: vec![(
                            Expr::Lit(ExprLit {
                                meta_list: MetaList(vec![]),
                                lit: Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream::from((2, "1")))),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None
                                })
                            }),
                            Comma(TokenStream::from((3, ",")))
                        )],
                        last: Some(Box::new(Expr::Lit(ExprLit {
                            meta_list: MetaList(vec![]),
                            lit: Lit::String(LitStr(TokenStream::from((5, "hello"))))
                        })))
                    },
                    delimiter_end: RightParenthesis(TokenStream::from((11, ")")))
                }),
                TokenStream::from((12, ""))
            ))
        );
    }
}
