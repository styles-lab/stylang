use parserc::{Parse, Parser, ParserExt, keyword};

use crate::lang::parse_attr_comment_list;

use super::{AttrOrComment, LitExpr, ParseError, Patt, StylangInput};

/// A stylang expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LetExpr<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// `let` keyword.
    pub let_token: I,
    /// variable pattern.
    pub patt: Box<Patt<I>>,
    /// equal token: `=`
    pub eq_token: I,
    /// variable initialize expr.
    pub expr: Box<Expr<I>>,
}

impl<I> Parse<I> for LetExpr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (let_token, input) = keyword("let").parse(input)?;
        let (patt, input) = Patt::parse(input)?;
        let (eq_token, input) = keyword("=").parse(input)?;
        let (expr, input) = Expr::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                let_token,
                patt: Box::new(patt),
                eq_token,
                expr: Box::new(expr),
            },
            input,
        ))
    }
}

/// A stylang expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I> {
    Lit(LitExpr<I>),
    Let(LetExpr<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        LitExpr::into_parser()
            .map(|v| Self::Lit(v))
            .or(LetExpr::into_parser().map(|v| Self::Let(v)))
            .parse(input)
    }
}
