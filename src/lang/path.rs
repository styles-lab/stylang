use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    inputs::LangInput,
    meta::MetaList,
    tokens::{Ident, PathSep, S},
};

/// A path at which a named item is exported (e.g. std::collections::HashMap).
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Path<I>
where
    I: LangInput,
{
    /// optional metalist.
    pub meta_list: MetaList<I>,
    /// The first segment of a path.
    pub first: Ident<I>,
    /// The rest segments.
    pub segments: Vec<PathSegment<I>>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct PathSegment<I>
where
    I: LangInput,
{
    /// seperate token `::`
    #[key_field]
    pub sep: (Option<S<I>>, PathSep<I>, Option<S<I>>),
    /// segment ident.
    pub ident: Ident<I>,
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        inputs::TokenStream,
        meta::MetaList,
        path::*,
        tokens::{Ident, PathSep, S},
    };

    #[test]
    fn test_path() {
        assert_eq!(
            Path::parse(TokenStream::from("a :: b ::c")),
            Ok((
                Path {
                    meta_list: MetaList::default(),
                    first: Ident(TokenStream::from("a")),
                    segments: vec![
                        PathSegment {
                            sep: (
                                Some(S(TokenStream::from((1, " ")))),
                                PathSep(TokenStream::from((2, "::"))),
                                Some(S(TokenStream::from((4, " "))))
                            ),
                            ident: Ident(TokenStream::from((5, "b")))
                        },
                        PathSegment {
                            sep: (
                                Some(S(TokenStream::from((6, " ")))),
                                PathSep(TokenStream::from((7, "::"))),
                                None,
                            ),
                            ident: Ident(TokenStream::from((9, "c")))
                        }
                    ]
                },
                TokenStream::from((10, ""))
            ))
        );

        assert_eq!(
            Path::parse(TokenStream::from("a ")),
            Ok((
                Path {
                    meta_list: MetaList::default(),
                    first: Ident(TokenStream::from("a")),
                    segments: vec![]
                },
                TokenStream::from((1, " "))
            ))
        );
    }
}
