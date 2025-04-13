use parserc::{Parse, Parser, ParserExt};

use crate::lang::{parse_attr_comment_list, parse_punctuation_sep};

use super::{AttrOrComment, Ident, Lit, ParseError, StylangInput, Type};

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I> {
    Lit(PattLit<I>),
    Ident(PattIdent<I>),
    Type(PattType<I>),
}

impl<I> Parse<I> for Patt<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        PattLit::into_parser()
            .map(|v| Self::Lit(v))
            .or(PattType::into_parser().map(|v| Self::Type(v)))
            .or(PattIdent::into_parser().map(|v| Self::Ident(v)))
            .parse(input)
    }
}

/// A literal in place of an expression: 1, "foo".
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattLit<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// lit patt.
    pub lit: Lit<I>,
}

impl<I> Parse<I> for PattLit<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (lit, input) = Lit::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                lit,
            },
            input,
        ))
    }
}

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattType<I> {
    /// atrribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// variable binding pattern.
    pub patt: Box<Patt<I>>,
    /// seperator `:`
    pub colon_token: I,
    /// type declaration.
    pub ty: Box<Type<I>>,
}

impl<I> Parse<I> for PattType<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (patt, input) = PattLit::into_parser()
            .map(|v| Patt::Lit(v))
            .or(PattIdent::into_parser().map(|v| Patt::Ident(v)))
            .parse(input)?;

        let (colon_token, input) = parse_punctuation_sep(b':').parse(input)?;
        let (ty, input) = Type::parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                patt: Box::new(patt),
                colon_token,
                ty: Box::new(ty),
            },
            input,
        ))
    }
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

    use crate::lang::{Attr, AttrOrComment, Comment, Ident, Patt, TokenStream, Type};

    use super::{PattIdent, PattType};

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

    #[test]
    fn test_patt_ty() {
        assert_eq!(
            PattType::parse(TokenStream::from("foo: f64")),
            Ok((
                PattType {
                    attr_comment_list: vec![],
                    patt: Box::new(Patt::Ident(PattIdent {
                        attr_comment_list: vec![],
                        ident: Ident(TokenStream::from((0, "foo"))),
                    })),
                    colon_token: TokenStream::from((3, ":")),
                    ty: Box::new(Type::Primary(TokenStream::from((5, "f64"))))
                },
                TokenStream::from((8, ""))
            ))
        );
    }
}
