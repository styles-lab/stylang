use parserc::derive_parse;

use crate::lang::{
    errors::LangError,
    input::LangInput,
    token::*,
    ty::{TypeFn, TypeNum, TypePath},
};

/// The parser for `stylang` types.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(input = I, error = LangError)]
pub enum Type<I>
where
    I: LangInput,
{
    Fn(TypeFn<I>),
    View(TokenView<I>),
    Enum(TokenEnum<I>),
    Data(TokenData<I>),
    Class(TokenClass<I>),
    Color(TokenColor<I>),
    Length(TokenLength<I>),
    Angle(TokenAngle<I>),
    Bool(TokenBool<I>),
    String(TokenString<I>),
    Num(TypeNum<I>),
    Path(TypePath<I>),
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{
        input::TokenStream,
        token::TokenI32,
        ty::{Type, TypeNum},
    };

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
