use parserc::{Parse, Parser, ParserExt, next};

use crate::lang::delimited;

use super::{Delimiter, Ident, ParseError, Punctuated, StylangInput, Type, skip_ws};

/// Variable list declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Variable<I> {
    Name(Ident<I>),
    Typed {
        /// variable name.
        ident: Ident<I>,
        /// seperator character: `:`
        sep: I,
        /// explicit type declaration.
        ty: Type<I>,
    },
    Tuple {
        /// tuple delimiter: `(...)`
        delimiter: Delimiter<I>,
        /// variable list.
        items: Punctuated<I, Variable<I>, b','>,
    },
}

impl<I> Parse<I> for Variable<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (ident, input) = Ident::into_parser().ok().parse(input)?;

        if let Some(ident) = ident {
            let (_, input) = skip_ws(input)?;

            let (sep, input) = next(b':').ok().parse(input)?;

            if let Some(sep) = sep {
                let (_, input) = skip_ws(input)?;
                let (ty, input) = Type::parse(input)?;

                return Ok((Self::Typed { ident, sep, ty }, input));
            }

            return Ok((Self::Name(ident), input));
        }

        let ((delimiter, items), input) =
            delimited("(", Punctuated::into_parser(), ")").parse(input)?;

        Ok((Self::Tuple { delimiter, items }, input))
    }
}

/// A code block delimited by `{...}`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Block<I> {
    /// delimiter of the block: `{...}`
    pub delimiter: Delimiter<I>,
}

impl<I> Parse<I> for Block<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(_input: I) -> parserc::Result<Self, I, Self::Error> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Delimiter, Ident, Punctuated, TokenStream, Type, Variable};

    #[test]
    fn test_variable() {
        assert_eq!(
            Variable::parse(TokenStream::from("name")),
            Ok((
                Variable::Name(Ident(TokenStream::from("name"))),
                TokenStream::from((4, ""))
            ))
        );

        assert_eq!(
            Variable::parse(TokenStream::from("name : i32")),
            Ok((
                Variable::Typed {
                    ident: Ident(TokenStream::from("name")),
                    sep: TokenStream::from((5, ":")),
                    ty: Type::Primary(TokenStream::from((7, "i32")))
                },
                TokenStream::from((10, ""))
            ))
        );

        assert_eq!(
            Variable::parse(TokenStream::from("(name : i32, passwd: string,)")),
            Ok((
                Variable::Tuple {
                    delimiter: Delimiter {
                        start: TokenStream::from("("),
                        end: TokenStream::from((28, ")"))
                    },
                    items: Punctuated {
                        items: vec![
                            (
                                Variable::Typed {
                                    ident: Ident(TokenStream::from((1, "name"))),
                                    sep: TokenStream::from((6, ":")),
                                    ty: Type::Primary(TokenStream::from((8, "i32")))
                                },
                                TokenStream::from((11, ","))
                            ),
                            (
                                Variable::Typed {
                                    ident: Ident(TokenStream::from((13, "passwd"))),
                                    sep: TokenStream::from((19, ":")),
                                    ty: Type::Primary(TokenStream::from((21, "string")))
                                },
                                TokenStream::from((27, ","))
                            )
                        ],
                        last: None
                    }
                },
                TokenStream::from((29, ""))
            ))
        );
    }
}
