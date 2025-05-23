use parserc::{Parse, Parser, ParserExt};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    tokens::{Digits, LeftBracket, RightBracket, S, SemiColon},
};

use super::ExprLit;

/// An array literal constructed from one repeated element: [0u8; N].
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprRepeat<I>
where
    I: LangInput,
{
    /// The leading meta-data list.
    pub meta_list: MetaList<I>,
    /// delimiter start token: `[`
    pub delimiter_start: LeftBracket<I>,
    /// The literial value expr.
    pub expr: ExprLit<I>,
    /// split token: `;`
    pub semi_token: SemiColon<I>,
    /// array length.
    pub len: Digits<I>,
    /// delimiter end token: `]`
    pub delimiter_end: RightBracket<I>,
}

impl<I> Parse<I> for ExprRepeat<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (delimiter_start, input) = LeftBracket::parse(input)?;

        let (expr, input) = ExprLit::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RepeatLit, input.span()))
            .fatal()
            .parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;
        let (semi_token, input) = SemiColon::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Token(";"), input.span()))
            .fatal()
            .parse(input)?;
        let (_, input) = S::into_parser().ok().parse(input)?;

        let (len, input) = Digits::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::RepeatLen, input.span()))
            .fatal()
            .parse(input)?;

        let (delimiter_end, input) = RightBracket::parse(input)?;

        Ok((
            Self {
                meta_list,
                delimiter_start,
                expr,
                semi_token,
                len,
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
        expr::{Expr, ExprLit},
        inputs::TokenStream,
        lit::{Lit, LitNum, NumUnit},
        meta::MetaList,
        tokens::{Digits, LeftBracket, RightBracket, SemiColon, U8},
    };

    use super::ExprRepeat;

    #[test]
    fn test_repeat() {
        assert_eq!(
            Expr::parse(TokenStream::from("[10u8;20]")),
            Ok((
                Expr::Repeat(ExprRepeat {
                    meta_list: MetaList(vec![]),
                    delimiter_start: LeftBracket(TokenStream::from("[")),
                    expr: ExprLit {
                        meta_list: MetaList(vec![]),
                        lit: Lit::Num(LitNum {
                            sign: None,
                            trunc: Some(Digits(TokenStream::from((1, "10")))),
                            dot: None,
                            fract: None,
                            exp: None,
                            unit: Some(NumUnit::U8(U8(TokenStream::from((3, "u8"))))),
                        })
                    },
                    semi_token: SemiColon(TokenStream::from((5, ";"))),
                    len: Digits(TokenStream::from((6, "20"))),
                    delimiter_end: RightBracket(TokenStream::from((8, "]")))
                }),
                TokenStream::from((9, ""))
            ))
        );
    }
}
