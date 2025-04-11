use parserc::{Kind, Parse, Parser, ParserExt, keyword};

use crate::lang::parse_attr_comment_list;

use super::{AttrOrComment, LitExpr, ParseError, Patt, StylangInput, Token, skip_ws};

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
        let (let_token, input) = keyword("let")
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Keyword("let"), input.span()))
            .parse(input)?;
        let (patt, input) = Patt::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (eq_token, input) = keyword("=")
            .map_err(|input: I, _: Kind| ParseError::Expect(Token::Keyword("="), input.span()))
            .parse(input)?;

        let (_, input) = skip_ws(input)?;

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

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Digits, Expr, Ident, LitExpr, LitNum, Patt, PattIdent, TokenStream, Unit, UnitInteger,
    };

    use super::LetExpr;

    #[test]
    fn test_let() {
        assert_eq!(
            LetExpr::parse(TokenStream::from("let a = 10i32")),
            Ok((
                LetExpr {
                    attr_comment_list: vec![],
                    let_token: TokenStream::from((0, "let")),
                    patt: Box::new(Patt::Ident(PattIdent {
                        attr_comment_list: vec![],
                        ident: Ident(TokenStream::from((4, "a")))
                    })),
                    eq_token: TokenStream::from((6, "=")),
                    expr: Box::new(Expr::Lit(LitExpr::Num(LitNum {
                        sign: None,
                        trunc: Some(Digits(TokenStream::from((8, "10")))),
                        comma: None,
                        fract: None,
                        exp: None,
                        unit: Some(Unit::Integer(UnitInteger(TokenStream::from((10, "i32"))))),
                    }))),
                },
                TokenStream::from((13, ""))
            ))
        );
    }
}
