use parserc::Parse;

use crate::lang::parse_attr_comment_list;

use super::{AttrOrComment, Ident, Lit, ParseError, StylangInput, Type};

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I> {
    Lit(Lit<I>),
    Ident(PattIdent<I>),
    Type(PattType<I>),
}

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattType<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// variable binding pattern.
    pub pat: Box<Patt<I>>,
    /// seperator `:`
    pub colon_token: I,
    /// type declaration.
    pub ty: Box<Type<I>>,
}

/// A pattern that binds a new variable:
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattIdent<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// variable ident.
    pub ident: Ident<I>,
}

impl<I> Parse<I> for PattIdent<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (ident, input) = Ident::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                ident,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Attr, AttrOrComment, Comment, Ident, TokenStream};

    use super::PattIdent;

    #[test]
    fn test_patt_ident() {
        assert_eq!(
            PattIdent::parse(TokenStream::from(
                r#"
                /// hello
                @hello   ident
                "#
            )),
            Ok((
                PattIdent {
                    attr_comment_list: vec![
                        AttrOrComment::Comment(Comment(TokenStream::from((20, " hello")))),
                        AttrOrComment::Attr(Attr {
                            keyword: TokenStream::from((43, "@")),
                            ident: Ident(TokenStream::from((44, "hello"))),
                            body: None
                        })
                    ],
                    ident: Ident(TokenStream::from((52, "ident"))),
                },
                TokenStream::from((57, "\n                "))
            ))
        );
    }
}
