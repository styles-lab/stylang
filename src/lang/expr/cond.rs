use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::{
    errors::{LangError, TokenKind},
    inputs::LangInput,
    meta::MetaList,
    patt::Patt,
    tokens::{
        KeywordBreak, KeywordElse, KeywordFor, KeywordIf, KeywordIn, KeywordLoop, KeywordReturn, S,
    },
};

use super::{Block, Expr, ExprBlock};

/// An if expression with an optional else block: if expr { ... } else { ... }.
///
/// The else branch expression may only be an If or Block expression, not any of the other types of expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprIf<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// token `if`
    pub if_token: KeywordIf<I>,
    /// if condition expr.
    pub cond: Box<Expr<I>>,
    /// required then code block.
    pub then_branch: ExprBlock<I>,
    /// optional else block.
    pub else_branch: Option<(KeywordElse<I>, Box<Expr<I>>)>,
}

impl<I> Parse<I> for ExprIf<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (meta_list, input) = MetaList::parse(input)?;
        let (if_token, input) = KeywordIf::parse(input)?;
        let (cond, input) = Expr::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Cond, input.span()))
            .fatal()
            .boxed()
            .parse(input)?;

        let (then_branch, input) = ExprBlock::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Then, input.span()))
            .fatal()
            .parse(input)?;

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (Some(else_token), input) = KeywordElse::into_parser().ok().parse(input.clone())?
        else {
            return Ok((
                Self {
                    meta_list,
                    if_token,
                    cond,
                    then_branch,
                    else_branch: None,
                },
                input,
            ));
        };

        let (_, input) = S::into_parser().ok().parse(input)?;

        let (else_branch, input) = Expr::into_parser()
            .map_err(|input: I, _| LangError::expect(TokenKind::Else, input.span()))
            .fatal()
            .boxed()
            .parse(input)?;

        Ok((
            Self {
                meta_list,
                if_token,
                cond,
                then_branch,
                else_branch: Some((else_token, else_branch)),
            },
            input,
        ))
    }
}

/// A return, with an optional value to be returned.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprReturn<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `return`
    pub return_token: KeywordReturn<I>,
    /// Optional returns value expr.
    pub expr: Option<Box<Expr<I>>>,
}

/// A break, with an optional expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprBreak<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `break`
    pub break_token: KeywordBreak<I>,
    /// Optional break expr.
    pub expr: Option<Box<Expr<I>>>,
}

/// A for loop: for pat in expr { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprFor<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `for`
    pub for_token: KeywordFor<I>,
    /// for pattern expr.
    pub patt: Box<Patt<I>>,
    /// keyword `in`
    pub in_token: (S<I>, KeywordIn<I>),
    /// in expr.
    pub expr: Box<Expr<I>>,
    /// block of for expr.
    pub block: ExprBlock<I>,
}

/// Conditionless loop: loop { ... }.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ExprLoop<I>
where
    I: LangInput,
{
    /// optional meta list.
    pub meta_list: MetaList<I>,
    /// keyword `loop`
    pub loop_token: (KeywordLoop<I>, Option<S<I>>),
    /// block of for expr.
    pub block: Block<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        expr::{
            BinOp, Block, Expr, ExprBinary, ExprBlock, ExprBreak, ExprFor, ExprIf, ExprLit,
            ExprLoop, ExprPath, ExprReturn,
        },
        inputs::TokenStream,
        lit::{Lit, LitNum},
        meta::MetaList,
        patt::{Patt, PattIdent, PattTuple, PattWild},
        punct::Punctuated,
        tokens::{
            Comma, Digits, Ident, KeywordBreak, KeywordElse, KeywordFor, KeywordIf, KeywordIn,
            KeywordLoop, KeywordReturn, LeftBrace, LeftParen, Lt, RightBrace, RightParen, S,
            Underscore,
        },
    };

    #[test]
    fn test_if() {
        assert_eq!(
            Expr::parse(TokenStream::from("if a {} else if b < 4 {} else {}")),
            Ok((
                Expr::If(ExprIf {
                    meta_list: Default::default(),
                    if_token: KeywordIf(TokenStream::from("if")),
                    cond: Box::new(Expr::Path(ExprPath {
                        meta_list: Default::default(),
                        first: Ident(TokenStream::from((3, "a"))),
                        segments: vec![]
                    })),
                    then_branch: ExprBlock {
                        meta_list: Default::default(),
                        block: Block {
                            delimiter_start: LeftBrace(TokenStream::from((5, "{"))),
                            stmts: Default::default(),
                            meta_list: Default::default(),
                            delimiter_end: RightBrace(TokenStream::from((6, "}")))
                        }
                    },
                    else_branch: Some((
                        KeywordElse(TokenStream::from((8, "else"))),
                        Box::new(Expr::If(ExprIf {
                            meta_list: Default::default(),
                            if_token: KeywordIf(TokenStream::from((13, "if"))),
                            cond: Box::new(Expr::Binary(ExprBinary {
                                left: Box::new(Expr::Path(ExprPath {
                                    meta_list: Default::default(),
                                    first: Ident(TokenStream::from((16, "b"))),
                                    segments: vec![]
                                })),
                                op: BinOp::Lt(Lt(TokenStream::from((18, "<")))),
                                right: Box::new(Expr::Lit(ExprLit {
                                    meta_list: Default::default(),
                                    lit: Lit::Num(LitNum {
                                        sign: None,
                                        trunc: Some(Digits(TokenStream::from((20, "4")))),
                                        dot: None,
                                        fract: None,
                                        exp: None,
                                        unit: None,
                                    })
                                })),
                            })),
                            then_branch: ExprBlock {
                                meta_list: Default::default(),
                                block: Block {
                                    delimiter_start: LeftBrace(TokenStream::from((22, "{"))),
                                    stmts: Default::default(),
                                    meta_list: Default::default(),
                                    delimiter_end: RightBrace(TokenStream::from((23, "}")))
                                }
                            },
                            else_branch: Some((
                                KeywordElse(TokenStream::from((25, "else"))),
                                Box::new(Expr::Block(ExprBlock {
                                    meta_list: Default::default(),
                                    block: Block {
                                        delimiter_start: LeftBrace(TokenStream::from((30, "{"))),
                                        stmts: Default::default(),
                                        meta_list: Default::default(),
                                        delimiter_end: RightBrace(TokenStream::from((31, "}")))
                                    }
                                }))
                            ))
                        }))
                    ))
                }),
                TokenStream::from((32, ""))
            ))
        );
    }

    #[test]
    fn test_return() {
        assert_eq!(
            Expr::parse(TokenStream::from("return;")),
            Ok((
                Expr::Return(ExprReturn {
                    meta_list: MetaList::default(),
                    return_token: KeywordReturn(TokenStream::from("return")),
                    expr: None,
                }),
                TokenStream::from((6, ";"))
            ))
        );
    }

    #[test]
    fn test_return_with_expr() {
        assert_eq!(
            Expr::parse(TokenStream::from("return a;")),
            Ok((
                Expr::Return(ExprReturn {
                    meta_list: MetaList::default(),
                    return_token: KeywordReturn(TokenStream::from("return")),
                    expr: Some(Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList::default(),
                        first: Ident(TokenStream::from((7, "a"))),
                        segments: vec![]
                    }))),
                }),
                TokenStream::from((8, ";"))
            ))
        );
    }

    #[test]
    fn test_break() {
        assert_eq!(
            Expr::parse(TokenStream::from("break;")),
            Ok((
                Expr::Break(ExprBreak {
                    meta_list: MetaList::default(),
                    break_token: KeywordBreak(TokenStream::from("break")),
                    expr: None,
                }),
                TokenStream::from((5, ";"))
            ))
        );
    }

    #[test]
    fn test_break_with_expr() {
        assert_eq!(
            Expr::parse(TokenStream::from("break a;")),
            Ok((
                Expr::Break(ExprBreak {
                    meta_list: MetaList::default(),
                    break_token: KeywordBreak(TokenStream::from("break")),
                    expr: Some(Box::new(Expr::Path(ExprPath {
                        meta_list: MetaList::default(),
                        first: Ident(TokenStream::from((6, "a"))),
                        segments: vec![]
                    }))),
                }),
                TokenStream::from((7, ";"))
            ))
        );
    }

    #[test]
    fn test_loop() {
        assert_eq!(
            Expr::parse(TokenStream::from("loop {}")),
            Ok((
                Expr::Loop(ExprLoop {
                    meta_list: Default::default(),
                    loop_token: (
                        KeywordLoop(TokenStream::from("loop")),
                        Some(S(TokenStream::from((4, " "))))
                    ),
                    block: Block {
                        delimiter_start: LeftBrace(TokenStream::from((5, "{"))),
                        stmts: Default::default(),
                        meta_list: Default::default(),
                        delimiter_end: RightBrace(TokenStream::from((6, "}")))
                    }
                }),
                TokenStream::from((7, ""))
            ))
        );
    }

    #[test]
    fn test_for_loop() {
        assert_eq!(
            Expr::parse(TokenStream::from("for (a,_) in c {}")),
            Ok((
                Expr::For(ExprFor {
                    meta_list: Default::default(),
                    for_token: KeywordFor(TokenStream::from("for")),
                    patt: Box::new(Patt::Tuple(PattTuple {
                        meta_list: Default::default(),
                        delimiter_start: LeftParen(TokenStream::from((4, "("))),
                        elems: Punctuated {
                            items: vec![(
                                Patt::Ident(PattIdent {
                                    meta_list: Default::default(),
                                    ident: Ident(TokenStream::from((5, "a"))),
                                    subpatt: None
                                }),
                                Comma(TokenStream::from((6, ",")))
                            )],
                            last: Some(Box::new(Patt::Wild(PattWild {
                                meta_list: Default::default(),
                                under_score_token: Underscore(TokenStream::from((7, "_")))
                            })))
                        },
                        delimiter_end: RightParen(TokenStream::from((8, ")")))
                    })),
                    in_token: (
                        S(TokenStream::from((9, " "))),
                        KeywordIn(TokenStream::from((10, "in")))
                    ),
                    expr: Box::new(Expr::Path(ExprPath {
                        meta_list: Default::default(),
                        first: Ident(TokenStream::from((13, "c"))),
                        segments: vec![]
                    })),
                    block: ExprBlock {
                        meta_list: Default::default(),
                        block: Block {
                            delimiter_start: LeftBrace(TokenStream::from((15, "{"))),
                            stmts: Default::default(),
                            meta_list: Default::default(),
                            delimiter_end: RightBrace(TokenStream::from((16, "}")))
                        },
                    }
                },),
                TokenStream::from((17, ""))
            ))
        );
    }
}
