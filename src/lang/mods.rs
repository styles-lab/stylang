use parserc::{Kind, Parse, Parser, ParserExt, keyword, next};

use crate::lang::{delimited, parse_attr_comment_list, skip_ws, ws};

use super::{
    AttrOrComment, Delimiter, Ident, ParseError, Punctuated, StylangInput, TokenError, Visibility,
};

/// A module declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

        let (keyword, input) = keyword("mod")
            .map_err(|input: I, _: Kind| ParseError::Expect(TokenError::Keyword("mod"), input.span()))
            .parse(input)?;

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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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

/// A use statement: `use std::...;`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Use<I> {
    /// optional attribute/comment list.
    pub attr_comment_list: Vec<AttrOrComment<I>>,
    /// visibility token,
    pub vis: Visibility<I>,
    /// keyword: `use`
    pub keyword: I,
    /// mod name.
    pub tree: UseTree<I>,
    /// semi_colon token: `;`
    pub semi: I,
}

impl<I> Parse<I> for Use<I>
where
    I: StylangInput,
{
    type Error = ParseError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        let (attr_comment_list, input) = parse_attr_comment_list(input)?;

        let (vis, input) = Visibility::parse(input)?;

        let (keyword, input) = keyword("use")
            .map_err(|input: I, _: Kind| ParseError::Expect(TokenError::Keyword("use"), input.span()))
            .parse(input)?;

        let (_, input) = ws(input)?;

        let (tree, input) = UseTree::parse(input)?;

        let (_, input) = skip_ws(input)?;

        let (semi, input) = next(b';').parse(input)?;

        Ok((
            Self {
                attr_comment_list,
                vis,
                keyword,
                tree,
                semi,
            },
            input,
        ))
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        Attr, AttrOrComment, Delimiter, Ident, Lit, LitCallBody, LitStr, Punctuated, TokenStream,
        UseGroup, UsePath, UseTree, Visibility,
    };

    use super::{Mod, Use};

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
                                last: Some(Box::new(Lit::Str(LitStr(TokenStream::from((
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

    #[test]
    fn test_use() {
        assert_eq!(
            Use::parse(TokenStream::from("pub use theme::*;")),
            Ok((
                Use {
                    attr_comment_list: vec![],
                    vis: Visibility::Public(TokenStream::from("pub")),
                    keyword: TokenStream::from((4, "use")),
                    tree: UseTree::Path(UsePath {
                        ident: Ident(TokenStream::from((8, "theme"))),
                        separator: TokenStream::from((13, "::")),
                        tree: Box::new(UseTree::Glob(TokenStream::from((15, "*"))))
                    }),
                    semi: TokenStream::from((16, ";"))
                },
                TokenStream::from((17, ""))
            ))
        );

        assert_eq!(
            Use::parse(TokenStream::from("pub use theme::{a::*,b};")),
            Ok((
                Use {
                    attr_comment_list: vec![],
                    vis: Visibility::Public(TokenStream {
                        offset: 0,
                        value: "pub",
                    },),
                    keyword: TokenStream {
                        offset: 4,
                        value: "use",
                    },
                    tree: UseTree::Path(UsePath {
                        ident: Ident(TokenStream {
                            offset: 8,
                            value: "theme",
                        },),
                        separator: TokenStream {
                            offset: 13,
                            value: "::",
                        },
                        tree: Box::new(UseTree::Group(UseGroup {
                            delimiter: Delimiter {
                                start: TokenStream {
                                    offset: 15,
                                    value: "{",
                                },
                                end: TokenStream {
                                    offset: 22,
                                    value: "}",
                                },
                            },
                            items: Punctuated {
                                items: vec![(
                                    UseTree::Path(UsePath {
                                        ident: Ident(TokenStream {
                                            offset: 16,
                                            value: "a",
                                        },),
                                        separator: TokenStream {
                                            offset: 17,
                                            value: "::",
                                        },
                                        tree: Box::new(UseTree::Glob(TokenStream {
                                            offset: 19,
                                            value: "*",
                                        },),)
                                    },),
                                    TokenStream {
                                        offset: 20,
                                        value: ",",
                                    },
                                ),],
                                last: Some(Box::new(UseTree::Name(Ident(TokenStream {
                                    offset: 21,
                                    value: "b",
                                },),),),),
                            },
                        },),)
                    },),
                    semi: TokenStream {
                        offset: 23,
                        value: ";",
                    },
                },
                TokenStream {
                    offset: 24,
                    value: "",
                },
            ),)
        );
    }
}
