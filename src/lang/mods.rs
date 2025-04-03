use parserc::{Parse, Parser, ParserExt, keyword, next};

use crate::lang::{delimited, parse_attr_comment_list, skip_ws, ws};

use super::{AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, Visibility};

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

/// A module declaration.
#[derive(Debug, PartialEq, Clone)]
pub enum UseTree<I> {
    /// A path prefix of imports in a use item: std::....
    Path(UsePath<I>),
    /// An identifier imported by a use item: HashMap.
    Name(Ident<I>),
    /// A glob import in a use item: *.
    Glob(I),
    /// A braced group of imports in a use item: {A, B, C}.
    Group(UseGroup<I>),
}

impl<I> Parse<I> for UseTree<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        next(b'*')
            .map(|v| Self::Glob(v))
            .or(UseGroup::into_parser().map(|v| Self::Group(v)))
            .or(UsePath::into_parser().map(|v| Self::Path(v)))
            .or(Ident::into_parser().map(|v| Self::Name(v)))
            .parse(input)
    }
}

/// a path prefix of imports in a use item: std::....
#[derive(Debug, PartialEq, Clone)]
pub struct UsePath<I> {
    pub ident: Ident<I>,
    pub separator: I,
    pub tree: Box<UseTree<I>>,
}

impl<I> Parse<I> for UsePath<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (ident, input) = Ident::parse(input)?;
        let (_, input) = skip_ws(input)?;
        let (separator, input) = keyword("::").parse(input)?;
        let (_, input) = skip_ws(input)?;
        let (tree, input) = UseTree::parse(input)?;

        Ok((
            Self {
                ident,
                separator,
                tree: Box::new(tree),
            },
            input,
        ))
    }
}

/// A braced group of imports in a use item: {A, B, C}.
#[derive(Debug, PartialEq, Clone)]
pub struct UseGroup<I> {
    /// brace delimiter: `{...}`
    pub delimiter: Delimiter<I>,
    /// punctuated group items: `A,B, C`
    pub items: Punctuated<I, UseTree<I>, b','>,
}

impl<I> Parse<I> for UseGroup<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let ((delimiter, items), input) =
            delimited("{", Punctuated::into_parser(), "}").parse(input)?;

        Ok((Self { delimiter, items }, input))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Delimiter, Ident, LitCallBody, LitExpr, LitStr, Punctuated,
        TokenStream, Visibility,
    };

    use super::Mod;

    #[test]
    fn test_mod() {
        assert_eq!(
            Mod::parse(TokenStream::from(
                r#"
                @sol("./erc20.json")
                pub mod erc20;
                "#
            )),
            Ok((
                Mod {
                    attr_comment_list: vec![AttrOrComment::Attr(Attr {
                        keyword: TokenStream::from((17, "@")),
                        ident: Ident(TokenStream::from((18, "sol"))),
                        body: Some(LitCallBody {
                            delimiter: Delimiter {
                                start: TokenStream::from((21, "(")),
                                end: TokenStream::from((36, ")")),
                            },
                            inputs: Punctuated {
                                items: vec![],
                                last: Some(Box::new(LitExpr::Str(LitStr(TokenStream::from((
                                    23,
                                    "./erc20.json"
                                ))))))
                            }
                        })
                    })],
                    vis: Visibility::Public(TokenStream::from((54, "pub"))),
                    keyword: TokenStream::from((58, "mod")),
                    ident: Ident(TokenStream::from((62, "erc20"))),
                    semi: TokenStream::from((67, ";"))
                },
                TokenStream::from((68, "\n                "))
            ))
        );
    }
}
