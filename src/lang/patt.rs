use parserc::{Parse, Parser, ParserExt, keyword};

use super::{
    AttrOrComment, Ident, ParseError, StylangInput, Type, parse_attr_comment_list, skip_ws,
};

/// A pattern that binds a new variable
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattIdent<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// ident pattern.
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

/// A type ascription pattern: foo: f64.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PattType<I> {
    /// attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// patt part.
    pub patt: Box<Patt<I>>,
    /// colon token: `:`
    pub colon_token: I,
    /// type declaration part.
    pub ty: Box<Type<I>>,
}

impl<I> Parse<I> for PattType<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;
        let (patt, input) = Patt::parse(input)?;
        let (_, input) = skip_ws(input)?;
        let (colon_token, input) = keyword(":").parse(input)?;
        let (_, input) = skip_ws(input)?;
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

/// A pattern in a local binding, function signature, match expression, or various other places.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Patt<I> {
    Ident(PattIdent<I>),
    Type(PattType<I>),
}

impl<I> Parse<I> for Patt<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        PattIdent::into_parser()
            .map(|v| Self::Ident(v))
            .or(PattType::into_parser().map(|v| Self::Type(v)))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{Ident, Patt, PattIdent, TokenStream, Type};

    use super::PattType;

    #[test]
    fn test_patt_type() {
        assert_eq!(
            PattType::parse(TokenStream::from("name: i32")),
            Ok((
                PattType {
                    attr_comment_list: vec![],
                    patt: Box::new(Patt::Ident(PattIdent {
                        attr_comment_list: vec![],
                        ident: Ident(TokenStream::from("name"))
                    })),
                    colon_token: TokenStream::from((4, ":")),
                    ty: Box::new(Type::Primary(TokenStream::from((6, "i32"))))
                },
                TokenStream::from((9, ""))
            ))
        );
    }

    #[test]
    fn test_patt_ident() {
        assert_eq!(
            PattIdent::parse(TokenStream::from("name")),
            Ok((
                PattIdent {
                    attr_comment_list: vec![],
                    ident: Ident(TokenStream::from("name"))
                },
                TokenStream::from((4, ""))
            ))
        );
    }
}
