use parserc::derive_parse;

use super::{
    KeywordCrate, KeywordPub, KeywordSuper, LeftParenthesis, ParseError, RightParenthesis, S,
    StylangInput,
};

/// The visibility level of an item: `pub`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = ParseError, input = I)]
pub enum Visibility<I>
where
    I: StylangInput,
{
    PublicCrate {
        keyword_pub: KeywordPub<I>,
        s1: Option<S<I>>,
        delimiter_start: LeftParenthesis<I>,
        s2: Option<S<I>>,
        keyword_crate: KeywordCrate<I>,
        s3: Option<S<I>>,
        delimiter_end: RightParenthesis<I>,
    },

    PublicSuper {
        keyword_pub: KeywordPub<I>,
        s1: Option<S<I>>,
        delimiter_start: LeftParenthesis<I>,
        s2: Option<S<I>>,
        keyword_super: KeywordSuper<I>,
        s3: Option<S<I>>,
        delimiter_end: RightParenthesis<I>,
    },
    Public(KeywordPub<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        KeywordCrate, KeywordPub, KeywordSuper, LeftParenthesis, RightParenthesis, S, TokenStream,
    };

    use super::Visibility;

    #[test]
    fn test_vs() {
        assert_eq!(
            Visibility::parse(TokenStream::from("pub")),
            Ok((
                Visibility::Public(KeywordPub(TokenStream::from("pub"))),
                TokenStream::from((3, ""))
            ))
        );

        assert_eq!(
            Visibility::parse(TokenStream::from("pub (crate)")),
            Ok((
                Visibility::PublicCrate {
                    keyword_pub: KeywordPub(TokenStream::from("pub")),
                    s1: Some(S(TokenStream::from((3, " ")))),
                    delimiter_start: LeftParenthesis(TokenStream::from((4, "("))),
                    s2: None,
                    keyword_crate: KeywordCrate(TokenStream::from((5, "crate"))),
                    s3: None,
                    delimiter_end: RightParenthesis(TokenStream::from((10, ")")))
                },
                TokenStream::from((11, ""))
            ))
        );

        assert_eq!(
            Visibility::parse(TokenStream::from("pub (super)")),
            Ok((
                Visibility::PublicSuper {
                    keyword_pub: KeywordPub(TokenStream::from("pub")),
                    s1: Some(S(TokenStream::from((3, " ")))),
                    delimiter_start: LeftParenthesis(TokenStream::from((4, "("))),
                    s2: None,
                    keyword_super: KeywordSuper(TokenStream::from((5, "super"))),
                    s3: None,
                    delimiter_end: RightParenthesis(TokenStream::from((10, ")")))
                },
                TokenStream::from((11, ""))
            ))
        );
    }
}
