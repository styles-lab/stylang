use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    patt::Patt,
    tokens::{Comma, FatArrow, KeywordIf, KeywordMatch, LeftBrace, RightBrace, S},
};

use super::Expr;

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Arm<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// pattern expr.
    pub patt: Patt<I>,
    /// optional guard expr.
    pub guard: Option<(KeywordIf<I>, Box<Expr<I>>)>,
    /// token `=>`
    pub fat_arrow_token: FatArrow<I>,
    /// body expr.
    pub body: Box<Expr<I>>,
    /// optional sep token: `,`
    pub comma: Option<Comma<I>>,
}

impl<I> Parse<I> for Arm<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (patt, input) = Patt::parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (guard, input) = match KeywordIf::into_parser().ok().parse(input)? {
            (Some(if_token), input) => {
                let (expr, input) = Expr::into_parser()
                    .map_err(|input: I, _| LangError::expect(TokenKind::If, input.span()))
                    .fatal()
                    .boxed()
                    .parse(input)?;

                (Some((if_token, expr)), input)
            }
            (_, input) => (None, input),
        };

        let (fat_arrow_token, input) = FatArrow::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Token("=>"), input.span()))
            .fatal()
            .parse(input)?;

        let (body, input) = Expr::into_parser()
            .boxed()
            .map_err(|input: I, _| LangError::expect(TokenKind::ArmExpr, input.span()))
            .fatal()
            .parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (comma, input) = Comma::into_parser().ok().parse(input)?;

        Ok((
            Self {
                meta_list,
                patt,
                guard,
                fat_arrow_token,
                body,
                comma,
            },
            input,
        ))
    }
}

/// A match expression: match n { Some(n) => {}, None => {} }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprMatch<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// keyword token: `match`
    pub match_token: KeywordMatch<I>,
    /// match target expr.
    pub expr: Box<Expr<I>>,
    /// delimiter start token: `{`
    pub delimiter_start: (Option<S<I>>, LeftBrace<I>),
    /// arm branches.
    pub arms: Vec<Arm<I>>,
    /// delimiter end token: `}`
    pub delimiter_end: (Option<S<I>>, RightBrace<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{Arm, Block, Expr, ExprBlock, ExprLit, ExprMatch, ExprPath},
        inputs::TokenStream,
        lit::{Lit, LitNum},
        patt::{Patt, PattWild},
        tokens::{Digits, FatArrow, Ident, KeywordMatch, LeftBrace, RightBrace, S, Underscore},
    };

    #[test]
    fn test_match_empty_arms() {
        assert_eq!(
            Expr::parse(TokenStream::from("match a {}")),
            Ok((
                Expr::Match(ExprMatch {
                    meta_list: Default::default(),
                    match_token: KeywordMatch(TokenStream::from("match")),
                    expr: Box::new(Expr::Path(ExprPath {
                        meta_list: Default::default(),
                        first: Ident(TokenStream::from((6, "a"))),
                        segments: vec![]
                    })),
                    delimiter_start: (
                        Some(S(TokenStream::from((7, " ")))),
                        LeftBrace(TokenStream::from((8, "{")))
                    ),
                    arms: vec![],
                    delimiter_end: (None, RightBrace(TokenStream::from((9, "}"))))
                }),
                TokenStream::from((10, ""))
            ))
        );
    }

    #[test]
    fn test_match_wild() {
        assert_eq!(
            Expr::parse(TokenStream::from("match a { 1 => {} _ => {} }")),
            Ok((
                Expr::Match(ExprMatch {
                    meta_list: Default::default(),
                    match_token: KeywordMatch(TokenStream::from("match")),
                    expr: Box::new(Expr::Path(ExprPath {
                        meta_list: Default::default(),
                        first: Ident(TokenStream::from((6, "a"))),
                        segments: vec![]
                    })),
                    delimiter_start: (
                        Some(S(TokenStream::from((7, " ")))),
                        LeftBrace(TokenStream::from((8, "{")))
                    ),
                    arms: vec![
                        Arm {
                            meta_list: Default::default(),
                            patt: Patt::Lit(ExprLit {
                                meta_list: Default::default(),
                                lit: Lit::Num(LitNum {
                                    sign: None,
                                    trunc: Some(Digits(TokenStream::from((10, "1")))),
                                    dot: None,
                                    fract: None,
                                    exp: None,
                                    unit: None,
                                })
                            }),
                            guard: None,
                            fat_arrow_token: FatArrow(TokenStream::from((12, "=>"))),
                            body: Box::new(Expr::Block(ExprBlock {
                                meta_list: Default::default(),
                                block: Block {
                                    delimiter_start: LeftBrace(TokenStream::from((15, "{"))),
                                    stmts: Default::default(),
                                    meta_list: Default::default(),
                                    delimiter_end: RightBrace(TokenStream::from((16, "}")))
                                },
                            })),
                            comma: None
                        },
                        Arm {
                            meta_list: Default::default(),
                            patt: Patt::Wild(PattWild {
                                meta_list: Default::default(),
                                under_score_token: Underscore(TokenStream::from((18, "_")))
                            }),
                            guard: None,
                            fat_arrow_token: FatArrow(TokenStream::from((20, "=>"))),
                            body: Box::new(Expr::Block(ExprBlock {
                                meta_list: Default::default(),
                                block: Block {
                                    delimiter_start: LeftBrace(TokenStream::from((23, "{"))),
                                    stmts: Default::default(),
                                    meta_list: Default::default(),
                                    delimiter_end: RightBrace(TokenStream::from((24, "}")))
                                },
                            })),
                            comma: None
                        }
                    ],
                    delimiter_end: (None, RightBrace(TokenStream::from((26, "}"))))
                }),
                TokenStream::from((27, ""))
            ))
        );
    }
}
