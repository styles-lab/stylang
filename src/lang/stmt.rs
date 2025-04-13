use parserc::{ControlFlow, Parse, Parser, ParserExt};

use super::{
    Comment, Expr, Item, ParseError, StylangInput, TokenError, parse_comment_list, token_of,
};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Stmt<I> {
    /// A item definition.
    Item(Item<I>),
    /// Expression, with or without trailing semicolon.
    Expr(Expr<I>, Option<I>),
    /// the tail comment list.
    Comments(Vec<Comment<I>>),
}

impl<I> Parse<I> for Stmt<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (item, input) = Item::into_parser().ok().parse(input)?;

        if let Some(item) = item {
            return Ok((Self::Item(item), input));
        }

        let (expr, input) = Expr::into_parser().ok().parse(input)?;

        if let Some(expr) = expr {
            let (semicolon, input) = token_of(";").ok().parse(input)?;

            return Ok((Self::Expr(expr, semicolon), input));
        }

        let (comments, input) = parse_comment_list(input)?;

        if comments.is_empty() {
            return Err(ControlFlow::Recovable(ParseError::Expect(
                TokenError::Stmt,
                input.span(),
            )));
        }

        return Ok((Self::Comments(comments), input));
    }
}

/// Parse a sequence of stmt.
pub fn parse_stmts<I>(mut input: I) -> parserc::Result<Vec<Stmt<I>>, I, ParseError>
where
    I: StylangInput,
{
    let mut stmts = vec![];

    loop {
        let stmt;
        (stmt, input) = Stmt::into_parser().ok().parse(input)?;

        if let Some(stmt) = stmt {
            stmts.push(stmt);
        } else {
            return Ok((stmts, input));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{
        Attr, AttrOrComment, Comment, Expr, ExprLet, ExprLit, Ident, Lit, LitNone, Patt, PattIdent,
        PattType, Stmt, TokenStream, Type, parse_stmts,
    };

    #[test]
    fn test_parse_stmts() {
        assert_eq!(
            parse_stmts(TokenStream::from(
                r#"let @state @option value: string = none;
                /// hello 
                /// world
                "#
            )),
            Ok((
                vec![
                    Stmt::Expr(
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
                        Some(TokenStream::from((39, ";")))
                    ),
                    Stmt::Comments(vec![
                        Comment(TokenStream::from((60, " hello "))),
                        Comment(TokenStream::from((87, " world"))),
                    ])
                ],
                TokenStream::from((110, ""))
            ))
        );
    }
}
