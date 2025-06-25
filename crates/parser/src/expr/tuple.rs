use parserc::{
    lang::LangInput,
    syntax::{PartialSyntax, Syntax},
};

use crate::{errors::LangError, expr::Expr, meta::MetaList, token::*};

/// A tuple expression parser: `(expr)`
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct ExprTuple<I>
where
    I: LangInput,
{
    /// optional leading meta-data list.
    pub meta_list: MetaList<I>,
    /// tuple body delimited by `(...)`
    pub body: Paren<I, Box<Expr<I>>>,
}

impl<I> PartialSyntax<I, LangError, (MetaList<I>, TokenLeftParen<I>)> for ExprTuple<I>
where
    I: LangInput,
{
    /// parse syntax `ExprTuple` with parsed `(`
    fn parse_with_prefix(
        (meta_list, token_left_paren): (MetaList<I>, TokenLeftParen<I>),
        input: I,
    ) -> parserc::errors::Result<Self, I, LangError> {
        use parserc::syntax::SyntaxEx;

        let (s, input) = input.parse()?;
        let start = (None, token_left_paren, s);
        let (body, input) = input.parse()?;
        let (end, input) = input.parse()?;

        Ok((
            Self {
                meta_list,
                body: Paren { start, end, body },
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::{
        lang::TokenStream,
        syntax::{Delimiter, Syntax},
    };

    use super::*;

    #[test]
    fn test_tuple() {
        assert_eq!(
            Expr::parse(TokenStream::from("(expr)")),
            Ok((
                Expr::Tuple(ExprTuple {
                    meta_list: vec![],
                    body: Delimiter {
                        start: (
                            None,
                            TokenLeftParen(TokenStream {
                                offset: 0,
                                value: "("
                            }),
                            None
                        ),
                        end: (
                            None,
                            TokenRightParen(TokenStream {
                                offset: 5,
                                value: ")"
                            }),
                            None
                        ),
                        body: Box::new(Expr::Ident(
                            vec![],
                            Ident(TokenStream {
                                offset: 1,
                                value: "expr"
                            })
                        ))
                    }
                }),
                TokenStream {
                    offset: 6,
                    value: ""
                }
            ))
        );
    }
}
