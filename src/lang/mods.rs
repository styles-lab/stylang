use parserc::{Parse, Parser, keyword, next};

use crate::lang::{parse_attr_comment_list, skip_ws, ws};

use super::{AttrOrComment, Ident, ParseError, StylangInput, Visibility};

/// A module declaration.
#[derive(Debug, PartialEq, Clone)]
pub struct Mod<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// visibility token,
    pub vis: Visibility<I>,
    /// keyword: `mod`
    pub keyword: I,
    /// mod name.
    pub ident: Ident<I>,
    /// semi_colon token: `;`
    pub semi: I,
}

impl<I> Parse<I> for Mod<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (vis, input) = Visibility::parse(input)?;

        let (keyword, input) = keyword("mod").parse(input)?;

        let (_, input) = ws(input)?;

        let (ident, input) = Ident::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (semi, input) = next(b';').parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                vis,
                keyword,
                ident,
                semi,
            },
            input,
        ))
    }
}
