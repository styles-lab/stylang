use parserc::{Parse, Parser, ParserExt};

use crate::lang::{parse_attr_comment_list, parse_punctuation_sep};

use super::{AttrOrComment, ParseError, Patt, PattLit, StylangInput, token_of};

/// A let guard: `let value: @state @option string = none`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ExprLet<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// let keyword token: `let`
    pub let_token: I,
    /// local binding patt.
    pub patt: Box<Patt<I>>,
    /// eq token `=`
    pub eq_token: I,
    /// initialize expr.
    pub expr: Box<Expr<I>>,
}

impl<I> Parse<I> for ExprLet<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (let_token, input) = token_of("let").parse(input)?;

        let (patt, input) = Patt::parse(input)?;
        let (eq_token, input) = parse_punctuation_sep(b'=').parse(input)?;
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

/// A literal in place of an expression: 1, "foo".
pub type ExprLit<I> = PattLit<I>;

/// A stylang expression.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Expr<I> {
    Let(ExprLet<I>),
    Lit(ExprLit<I>),
}

impl<I> Parse<I> for Expr<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        ExprLet::into_parser()
            .map(|v| Self::Let(v))
            .or(ExprLit::into_parser().map(|v| Self::Lit(v)))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Expr, ExprLet, ExprLit, Ident, Lit, LitNone, Patt, PattIdent,
        PattType, TokenStream, Type,
    };

    #[test]
    fn test_let_expr() {
        assert_eq!(
            Expr::parse(TokenStream::from("let @state @option value: string = none")),
            Ok((
                Expr::Let(ExprLet {
                    attr_comment_list: vec![],
                    let_token: TokenStream::from("let"),
                    patt: Box::new(Patt::Type(PattType {
                        attr_comment_list: vec![
                            AttrOrComment::Attr(Attr {
                                keyword: TokenStream::from((4, "@")),
                                ident: Ident(TokenStream::from((5, "state"))),
                                body: None
                            }),
                            AttrOrComment::Attr(Attr {
                                keyword: TokenStream::from((11, "@")),
                                ident: Ident(TokenStream::from((12, "option"))),
                                body: None
                            })
                        ],
                        patt: Box::new(Patt::Ident(PattIdent {
                            attr_comment_list: vec![],
                            ident: Ident(TokenStream::from((19, "value")))
                        })),
                        colon_token: TokenStream::from((24, ":")),
                        ty: Box::new(Type::Primary(TokenStream::from((26, "string"))))
                    })),
                    eq_token: TokenStream::from((33, "=")),
                    expr: Box::new(Expr::Lit(ExprLit {
                        attr_comment_list: vec![],
                        lit: Lit::None(LitNone(TokenStream::from((35, "none"))))
                    }))
                }),
                TokenStream::from((39, ""))
            ),)
        );
    }
}
