use parserc::{
    inputs::lang::LangInput,
    syntax::{AsSpan, Syntax},
};

use crate::lang::{
    errors::LangError,
    expr::Expr,
    item::Item,
    token::{Brace, S, TokenSemiColon},
};

/// A statement, usually ending in a semicolon.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Stmt<I>
where
    I: LangInput,
{
    /// expr end with optional `;`
    Expr(
        Expr<I>,
        Option<S<I>>,
        Option<TokenSemiColon<I>>,
        Option<S<I>>,
    ),
    /// declare a `class`,`data`,`fn`,...
    Item(Item<I>),
}

/// A list of stmts.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Stmts<I>(pub Vec<Stmt<I>>)
where
    I: LangInput;

impl<I> AsSpan for Stmts<I>
where
    I: LangInput,
{
    fn as_span(&self) -> Option<parserc::inputs::Span> {
        self.0.as_span()
    }
}

impl<I> Syntax<I, LangError> for Stmts<I>
where
    I: LangInput,
{
    fn parse(mut input: I) -> parserc::errors::Result<Self, I, LangError> {
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
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct Block<I>(pub Brace<I, Stmts<I>>)
where
    I: LangInput;

#[cfg(test)]
mod tests {
    use parserc::{
        inputs::lang::TokenStream,
        syntax::{Delimiter, Syntax},
    };

    use crate::lang::{
        expr::Expr,
        stmt::{Block, Stmt, Stmts},
        token::{Ident, TokenLeftBrace, TokenRightBrace},
    };

    #[test]
    fn block_value() {
        assert_eq!(
            Block::parse(TokenStream::from("{value}")),
            Ok((
                Block(Delimiter {
                    start: (
                        None,
                        TokenLeftBrace(TokenStream {
                            offset: 0,
                            value: "{"
                        }),
                        None
                    ),
                    body: Stmts(vec![Stmt::Expr(
                        Expr::Ident(
                            Default::default(),
                            Ident(TokenStream {
                                offset: 1,
                                value: "value"
                            })
                        ),
                        None,
                        None,
                        None
                    )]),
                    end: (
                        None,
                        TokenRightBrace(TokenStream {
                            offset: 6,
                            value: "}"
                        }),
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
