use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    punct::Punctuated,
    tokens::{Comma, LeftParenthesis, RightParenthesis, S},
};

use super::{Expr, ExprField, ExprLit, ExprPath};

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
enum CallTarget<I>
where
    I: LangInput,
{
    Field(ExprField<I>),
    Lit(ExprLit<I>),
    Path(ExprPath<I>),
}

impl<I> From<CallTarget<I>> for Expr<I>
where
    I: LangInput,
{
    fn from(value: CallTarget<I>) -> Self {
        match value {
            CallTarget::Field(v) => Self::Field(v),
            CallTarget::Lit(v) => Self::Lit(v),
            CallTarget::Path(v) => Self::Path(v),
        }
    }
}

/// A function call expression: invoke(a, b).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprCall<I>
where
    I: LangInput,
{
    pub target: Box<Expr<I>>,
    /// delimiter start token: `(`
    pub delimiter_start: LeftParenthesis<I>,
    /// params list.
    pub params: Punctuated<Expr<I>, Comma<I>>,
    /// delimiter end token: `)`
    pub delimiter_end: RightParenthesis<I>,
}

impl<I> Parse<I> for ExprCall<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (target, input) = CallTarget::into_parser()
            .map(|v| Expr::from(v))
            .boxed()
            .parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;
        let (delimiter_start, input) = LeftParenthesis::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (params, input) = Punctuated::parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;
        let (delimiter_end, input) = RightParenthesis::parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;

        Ok((
            Self {
                target,
                delimiter_start,
                params,
                delimiter_end,
            },
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
