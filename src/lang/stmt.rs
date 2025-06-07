use parserc::{Parse, derive_parse};

use crate::lang::{
    errors::LangError,
    expr::Expr,
    input::LangInput,
    item::Item,
    token::{Brace, SepSemiColon},
};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Stmt<I>
where
    I: LangInput,
{
    /// expr end with optional `;`
    Expr(Expr<I>, Option<SepSemiColon<I>>),
    /// declare a `class`,`data`,`fn`,...
    Item(Item<I>),
}

/// A list of stmts.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Stmts<I>(pub Vec<Stmt<I>>)
where
    I: LangInput;

impl<I> Parse<I> for Stmts<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(mut input: I) -> parserc::Result<Self, I, Self::Error> {
        let mut stmts = vec![];

        loop {
            let stmt;
            (stmt, input) = Stmt::parse(input)?;

            match &stmt {
                Stmt::Item(Item::MetaList(meta_list)) if meta_list.is_empty() => {
                    break;
                }
                _ => {}
            }

            stmts.push(stmt);
        }

        Ok((Self(stmts), input))
    }
}

/// stmts group by `{..}`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Block<I>(pub Brace<I, Stmts<I>>)
where
    I: LangInput;

#[cfg(test)]
mod tests {
    use parserc::{Delimiter, Parse};

    use crate::lang::{
        expr::{Expr, ExprPath, PathStart},
        input::TokenStream,
        stmt::{Block, Stmt, Stmts},
        token::{Ident, SepLeftBrace, SepRightBrace},
        ty::TypePath,
    };

    #[test]
    fn block_value() {
        assert_eq!(
            Block::parse(TokenStream::from("{value}")),
            Ok((
                Block(Delimiter {
                    delimiter_start: SepLeftBrace(
                        None,
                        TokenStream {
                            offset: 0,
                            value: "{"
                        },
                        None
                    ),
                    body: Stmts(vec![Stmt::Expr(
                        Expr::Path(ExprPath {
                            meta_list: vec![],
                            first: PathStart::TypePath(TypePath {
                                first: Ident(TokenStream {
                                    offset: 1,
                                    value: "value"
                                }),
                                rest: vec![]
                            }),
                            rest: vec![]
                        }),
                        None
                    )]),
                    delimiter_end: SepRightBrace(
                        None,
                        TokenStream {
                            offset: 6,
                            value: "}"
                        },
                        None
                    )
                }),
                TokenStream {
                    offset: 7,
                    value: ""
                }
            ))
        );
    }
}
