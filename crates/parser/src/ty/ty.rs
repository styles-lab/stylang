use parserc::{lang::LangInput, syntax::Syntax};

use crate::{
    errors::LangError,
    token::*,
    ty::{TypeFn, TypeNum, TypePath},
};

/// The parser of array type: [ty;N]
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct TypeArray<I>(
    Bracket<
        I,
        (
            Box<Type<I>>,
            Option<S<I>>,
            TokenSemiColon<I>,
            Option<S<I>>,
            Digits<I>,
        ),
    >,
)
where
    I: LangInput;

/// The parser of slice type: [ty]
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub struct TypeSlice<I>(Bracket<I, Box<Type<I>>>)
where
    I: LangInput;

/// The parser for `stylang` types.
#[derive(Debug, PartialEq, Clone, Syntax)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[error(LangError)]
pub enum Type<I>
where
    I: LangInput,
{
    Fn(TypeFn<I>),
    View(KeywordView<I>),
    Enum(KeywordEnum<I>),
    Data(KeywordData<I>),
    Class(KeywordClass<I>),
    Color(TokenColor<I>),
    Length(TokenLength<I>),
    Angle(TokenAngle<I>),
    Bool(TokenBool<I>),
    String(TokenString<I>),
    Num(TypeNum<I>),
    Path(TypePath<I>),
    Array(TypeArray<I>),
    Slice(TypeSlice<I>),
}

#[cfg(test)]
mod tests {

    use parserc::lang::TokenStream;

    use super::*;

    #[test]
    fn parse_priority() {
        assert_eq!(
            Type::parse(TokenStream::from("i32")),
            Ok((
                Type::Num(TypeNum::I32(TokenI32(TokenStream {
                    offset: 0,
                    value: "i32"
                }))),
                TokenStream {
                    offset: 3,
                    value: ""
                }
            ))
        );
    }
}
