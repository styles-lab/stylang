//! The syntax analyser for `stylang` items.

use parserc::{Parse, Parser, ParserExt, derive_parse};

use crate::lang::errors::ItemKind;

use super::{
    errors::LangError,
    expr::Block,
    inputs::LangInput,
    meta::{Comments, MetaList},
    patt::PattType,
    punct::Punctuated,
    tokens::*,
    ty::{Type, TypeReturn},
    vs::Visibility,
};

/// named field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct NamedField<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// field name.
    pub name: Ident<I>,
    /// colon token: `:`
    pub colon: (Option<S<I>>, Colon<I>, Option<S<I>>),
    /// field type.
    pub ty: Type<I>,
}

/// uname field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UnameField<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// field type.
    pub ty: Type<I>,
}

/// Field patt.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Fields<I>
where
    I: LangInput,
{
    Named {
        delimiter_start: LeftBrace<I>,
        fields: Punctuated<NamedField<I>, Comma<I>>,
        delimiter_end: RightBrace<I>,
    },
    Uname {
        delimiter_start: LeftParen<I>,
        fields: Punctuated<UnameField<I>, Comma<I>>,
        delimiter_end: RightParen<I>,
    },
}

/// Class item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemClass<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `class`
    pub keyword: (KeywordClass<I>, S<I>),
    /// class name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// field list
    pub fields: Fields<I>,
    /// optional semi-colon: `;`
    pub semi_colon: Option<SemiColon<I>>,
}

/// Data item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemData<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `data`
    pub keyword: (KeywordData<I>, S<I>),
    /// data name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// field list
    pub fields: Fields<I>,
    /// optional semi-colon: `;`
    pub semi_colon: Option<SemiColon<I>>,
}

/// Enum variant field.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Variant<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// variant name.
    pub name: (Ident<I>, Option<S<I>>),
    /// variant fields
    pub fields: Option<Fields<I>>,
}

/// Enum item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemEnum<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `enum`
    pub keyword: (KeywordEnum<I>, S<I>),
    /// data name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// delimiter `{`
    pub delimiter_start: LeftBrace<I>,
    ///  variant list.
    pub variants: Punctuated<Variant<I>, Comma<I>>,
    /// delimiter `}`
    pub delimiter_end: RightBrace<I>,
}

/// Fn Block.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum FnBlock<I>
where
    I: LangInput,
{
    Block(Block<I>),
    SemiColon(SemiColon<I>),
}

/// Fn item.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemFn<I>
where
    I: LangInput,
{
    /// meta data list.
    pub meta_list: MetaList<I>,
    /// Visibility keyword.
    pub vs: Option<(Visibility<I>, S<I>)>,
    /// keyword token: `extern`
    pub extern_token: Option<(KeywordExtern<I>, S<I>)>,
    /// keyword token: `fn`
    pub keyword: (KeywordFn<I>, S<I>),
    /// function name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// delimiter `(`
    pub delimiter_start: LeftParen<I>,
    /// argument list.
    pub args: Punctuated<PattType<I>, Comma<I>>,
    /// delimiter `)`
    pub delimiter_end: RightParen<I>,
    /// returns type part.
    pub return_type: Option<TypeReturn<I>>,
    /// function block part.
    pub block: (Option<S<I>>, FnBlock<I>),
}

/// A module declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemMod<I>
where
    I: LangInput,
{
    /// optional attribute/comment list.
    pub meta_list: MetaList<I>,
    /// visibility token,
    pub vis: Option<(Visibility<I>, S<I>)>,
    /// keyword: `mod`
    pub keyword: (KeywordMod<I>, S<I>),
    /// mod name.
    pub ident: (Ident<I>, Option<S<I>>),
    /// semi_colon token: `;`
    pub semi_colon: SemiColon<I>,
}

/// A module declaration.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UseTree<I>
where
    I: LangInput,
{
    /// A path prefix of imports in a use item: std::....
    Path(UsePath<I>),
    /// An identifier imported by a use item: HashMap.
    Name(Ident<I>),
    /// A glob import in a use item: *.
    Glob(Star<I>),
    /// A braced group of imports in a use item: {A, B, C}.
    Group(UseGroup<I>),
}

/// a path prefix of imports in a use item: std::....
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UsePath<I>
where
    I: LangInput,
{
    pub ident: Ident<I>,
    pub separator: (Option<S<I>>, PathSep<I>, Option<S<I>>),
    pub tree: Box<UseTree<I>>,
}

/// A braced group of imports in a use item: {A, B, C}.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UseGroup<I>
where
    I: LangInput,
{
    /// brace delimiter: `{`
    pub delimiter_start: LeftBrace<I>,
    /// punctuated group items: `A,B, C`
    pub items: Punctuated<UseTree<I>, Comma<I>>,
    /// brace delimiter end: `}`
    pub delimiter_end: RightBrace<I>,
}

/// A use statement: `use std::...;`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemUse<I>
where
    I: LangInput,
{
    /// optional attribute/comment list.
    pub meta_list: MetaList<I>,
    /// visibility token,
    pub vis: Option<(Visibility<I>, S<I>)>,
    /// keyword: `use`
    pub keyword: (KeywordUse<I>, S<I>),
    /// mod name.
    pub tree: UseTree<I>,
    /// semi_colon token: `;`
    pub semi_colon: SemiColon<I>,
}

/// stylang item variant.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
// #[derive_parse(error = LangError,input = I)]
pub enum Item<I>
where
    I: LangInput,
{
    Class(ItemClass<I>),
    Data(ItemData<I>),
    Enum(ItemEnum<I>),
    Fn(ItemFn<I>),
    Mod(ItemMod<I>),
    Use(ItemUse<I>),
    Comments(Comments<I>),
}

impl<I> Parse<I> for Item<I>
where
    I: LangInput,
{
    type Error = LangError;

    fn parse(input: I) -> parserc::Result<Self, I, Self::Error> {
        ItemClass::into_parser()
            .map(|v| Self::Class(v))
            .map_err(|input: I, err| LangError::from((err, ItemKind::Class(input.span()))))
            .or(ItemData::into_parser()
                .map(|v| Self::Data(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Data(input.span())))))
            .or(ItemEnum::into_parser()
                .map(|v| Self::Enum(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Enum(input.span())))))
            .or(ItemFn::into_parser()
                .map(|v| Self::Fn(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Fn(input.span())))))
            .or(ItemMod::into_parser()
                .map(|v| Self::Mod(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Mod(input.span())))))
            .or(ItemUse::into_parser()
                .map(|v| Self::Use(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Use(input.span())))))
            .or(Comments::into_parser()
                .map(|v| Self::Comments(v))
                .map_err(|input: I, err| LangError::from((err, ItemKind::Use(input.span())))))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::Parse;

    use crate::lang::{inputs::TokenStream, meta::*, path::TypePath, ty::*};

    use super::*;

    #[test]
    fn test_class() {
        let input = TokenStream::from(
            r#"
            /// Fill properties.
            class Fill {
                foreground_color: color,
                background_color: color,
            }
            "#,
        );

        assert_eq!(
            ItemClass::parse(input),
            Ok((
                ItemClass {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((16, " Fill properties."))
                    ))),]),
                    vs: None,
                    keyword: (
                        KeywordClass(TokenStream::from((46, "class"))),
                        S(TokenStream::from((51, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((52, "Fill"))),
                        Some(S(TokenStream::from((56, " "))))
                    ),
                    fields: Fields::Named {
                        delimiter_start: LeftBrace(TokenStream::from((57, "{"))),
                        fields: Punctuated {
                            items: vec![
                                (
                                    NamedField {
                                        meta_list: MetaList::default(),
                                        vs: None,
                                        name: Ident(TokenStream::from((75, "foreground_color"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((91, ":"),),),
                                            Some(S(TokenStream::from((92, " "))))
                                        ),
                                        ty: Type::Color(KeywordColor(TokenStream::from((
                                            93, "color"
                                        ))))
                                    },
                                    Comma(TokenStream::from((98, ",")))
                                ),
                                (
                                    NamedField {
                                        meta_list: MetaList::default(),
                                        vs: None,
                                        name: Ident(TokenStream::from((116, "background_color"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((132, ":"),),),
                                            Some(S(TokenStream::from((133, " "))))
                                        ),
                                        ty: Type::Color(KeywordColor(TokenStream::from((
                                            134, "color"
                                        ))))
                                    },
                                    Comma(TokenStream::from((139, ",")))
                                )
                            ],
                            last: None
                        },
                        delimiter_end: RightBrace(TokenStream::from((153, "}")))
                    },
                    semi_colon: None
                },
                TokenStream::from((154, "\n            "))
            ))
        );
    }

    #[test]
    fn test_data() {
        let input = TokenStream::from(
            r#"
            /// All fields of a `state` are default required.
            data Label {
                /// The fields is a value object passed by value.
                @state text: string,
                /// optional content builder function.
                @option content: fn() -> view,
            }
            "#,
        );

        assert_eq!(
            ItemData::parse(input),
            Ok((
                ItemData {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((16, " All fields of a `state` are default required."))
                    ))),]),
                    vs: None,
                    keyword: (
                        KeywordData(TokenStream::from((75, "data"))),
                        S(TokenStream::from((79, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((80, "Label"))),
                        Some(S(TokenStream::from((85, " "))))
                    ),
                    fields: Fields::Named {
                        delimiter_start: LeftBrace(TokenStream::from((86, "{"))),
                        fields: Punctuated {
                            items: vec![
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![
                                            Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                                                TokenStream::from((
                                                    107,
                                                    " The fields is a value object passed by value."
                                                ))
                                            ))),
                                            Meta::Attr(Attr {
                                                keyword: At(TokenStream::from((170, "@"))),
                                                ident: (
                                                    Ident(TokenStream::from((171, "state"))),
                                                    Some(S(TokenStream::from((176, " "))))
                                                ),
                                                params: None
                                            })
                                        ]),
                                        vs: None,
                                        name: Ident(TokenStream::from((177, "text"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((181, ":"),),),
                                            Some(S(TokenStream::from((182, " "))))
                                        ),
                                        ty: Type::String(KeywordString(TokenStream::from((
                                            183, "string"
                                        ))))
                                    },
                                    Comma(TokenStream::from((189, ",")))
                                ),
                                (
                                    NamedField {
                                        meta_list: MetaList(vec![
                                            Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                                                TokenStream::from((
                                                    210,
                                                    " optional content builder function."
                                                ))
                                            ))),
                                            Meta::Attr(Attr {
                                                keyword: At(TokenStream::from((262, "@"))),
                                                ident: (
                                                    Ident(TokenStream::from((263, "option"))),
                                                    Some(S(TokenStream::from((269, " "))))
                                                ),
                                                params: None
                                            })
                                        ]),
                                        vs: None,
                                        name: Ident(TokenStream::from((270, "content"))),
                                        colon: (
                                            None,
                                            Colon(TokenStream::from((277, ":"),),),
                                            Some(S(TokenStream::from((278, " "))))
                                        ),
                                        ty: Type::Fn(TypeFn {
                                            keyword_fn: KeywordFn(TokenStream::from((279, "fn"))),
                                            delimiter_start: LeftParen(TokenStream::from((
                                                281, "("
                                            ))),
                                            inputs: Punctuated {
                                                items: vec![],
                                                last: None
                                            },
                                            delimiter_end: RightParen(TokenStream::from((
                                                282, ")"
                                            ))),
                                            output: Some(TypeReturn {
                                                arrow_right: (
                                                    Some(S(TokenStream::from((283, " ")))),
                                                    ArrowRight(TokenStream::from((284, "->"))),
                                                    Some(S(TokenStream::from((286, " "))))
                                                ),
                                                ty: Box::new(Type::View(KeywordView(
                                                    TokenStream::from((287, "view"))
                                                )))
                                            })
                                        })
                                    },
                                    Comma(TokenStream::from((291, ",")))
                                )
                            ],
                            last: None
                        },
                        delimiter_end: RightBrace(TokenStream::from((305, "}"))),
                    },
                    semi_colon: None,
                },
                TokenStream::from((306, "\n            "))
            ))
        );
    }

    #[test]
    fn test_enum() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(
                r#"
                /// This property describes decorations that are added to the text of an element.
                enum TextDecoration {
                    Underline,
                    Overline,
                    LineThrough,
                    Blink,
                }
                "#
            )),
            Ok((
                ItemEnum {
                    meta_list: MetaList(vec![Meta::Comment(Comment::OutlineDoc(OutlineDoc(
                        TokenStream::from((
                            20,
                            " This property describes decorations that are added to the text of an element."
                        ))
                    )))]),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((115, "enum"))),
                        S(TokenStream::from((119, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((120, "TextDecoration"))),
                        Some(S(TokenStream::from((134, " "))))
                    ),
                    delimiter_start: LeftBrace(TokenStream::from((135, "{"))),
                    variants: Punctuated {
                        items: vec![
                            (
                                Variant {
                                    meta_list: MetaList::default(),
                                    name: (Ident(TokenStream::from((157, "Underline"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((166, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList::default(),
                                    name: (Ident(TokenStream::from((188, "Overline"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((196, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList::default(),
                                    name: (Ident(TokenStream::from((218, "LineThrough"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((229, ",")))
                            ),
                            (
                                Variant {
                                    meta_list: MetaList::default(),
                                    name: (Ident(TokenStream::from((251, "Blink"))), None),
                                    fields: None
                                },
                                Comma(TokenStream::from((256, ",")))
                            )
                        ],
                        last: None
                    },
                    delimiter_end: RightBrace(TokenStream::from((274, "}")))
                },
                TokenStream::from((275, "\n                "))
            ))
        );
    }

    #[test]
    fn test_enum2() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(r#"enum A { V(i32) }"#)),
            Ok((
                ItemEnum {
                    meta_list: MetaList::default(),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((0, "enum"))),
                        S(TokenStream::from((4, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((5, "A"))),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    delimiter_start: LeftBrace(TokenStream::from((7, "{"))),
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            meta_list: MetaList::default(),
                            name: (Ident(TokenStream::from((9, "V"))), None),
                            fields: Some(Fields::Uname {
                                delimiter_start: LeftParen(TokenStream::from((10, "("))),
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(UnameField {
                                        meta_list: MetaList::default(),
                                        vs: None,
                                        ty: Type::I32(I32(TokenStream::from((11, "i32"))))
                                    })),
                                },
                                delimiter_end: RightParen(TokenStream::from((14, ")")))
                            })
                        }))
                    },
                    delimiter_end: RightBrace(TokenStream::from((16, "}")))
                },
                TokenStream::from((17, ""))
            ))
        );
    }

    #[test]
    fn test_enum3() {
        assert_eq!(
            ItemEnum::parse(TokenStream::from(r#"enum A { V {v:i32} }"#)),
            Ok((
                ItemEnum {
                    meta_list: MetaList::default(),
                    vs: None,
                    keyword: (
                        KeywordEnum(TokenStream::from((0, "enum"))),
                        S(TokenStream::from((4, " ")))
                    ),
                    ident: (
                        Ident(TokenStream::from((5, "A"))),
                        Some(S(TokenStream::from((6, " "))))
                    ),
                    delimiter_start: LeftBrace(TokenStream::from((7, "{"))),
                    variants: Punctuated {
                        items: vec![],
                        last: Some(Box::new(Variant {
                            meta_list: MetaList::default(),
                            name: (
                                Ident(TokenStream::from((9, "V"))),
                                Some(S(TokenStream::from((10, " "))))
                            ),
                            fields: Some(Fields::Named {
                                delimiter_start: LeftBrace(TokenStream::from((11, "{"))),
                                fields: Punctuated {
                                    items: vec![],
                                    last: Some(Box::new(NamedField {
                                        meta_list: MetaList::default(),
                                        vs: None,
                                        name: Ident(TokenStream::from((12, "v"))),
                                        colon: (None, Colon(TokenStream::from((13, ":"))), None),
                                        ty: Type::I32(I32(TokenStream::from((14, "i32"))))
                                    })),
                                },
                                delimiter_end: RightBrace(TokenStream::from((17, "}")))
                            })
                        }))
                    },
                    delimiter_end: RightBrace(TokenStream::from((19, "}")))
                },
                TokenStream::from((20, ""))
            ))
        );
    }

    #[test]
    fn parse_fn_without_returns() {
        assert_eq!(
            ItemFn::parse(TokenStream::from(
                "@platform extern fn label(label: Label, layout: TextLayout, fill: Fill);",
            )),
            Ok((
                ItemFn {
                    meta_list: MetaList(vec![Meta::Attr(Attr {
                        keyword: At(TokenStream::from("@")),
                        ident: (
                            Ident(TokenStream::from((1, "platform"))),
                            Some(S(TokenStream::from((9, " "))))
                        ),
                        params: None
                    })]),
                    vs: None,
                    extern_token: Some((
                        KeywordExtern(TokenStream::from((10, "extern")),),
                        S(TokenStream::from((16, " ")))
                    )),
                    keyword: (
                        KeywordFn(TokenStream::from((17, "fn"))),
                        S(TokenStream::from((19, " ")))
                    ),
                    ident: (Ident(TokenStream::from((20, "label"))), None),
                    delimiter_start: LeftParen(TokenStream::from((25, "("))),
                    args: Punctuated {
                        items: vec![
                            (
                                PattType {
                                    meta_list: MetaList::default(),
                                    name: Ident(TokenStream::from((26, "label"))),
                                    colon_token: (
                                        None,
                                        Colon(TokenStream::from((31, ":"))),
                                        Some(S(TokenStream::from((32, " "))))
                                    ),
                                    ty: Box::new(Type::Path(TypePath {
                                        meta_list: MetaList::default(),
                                        first: Ident(TokenStream::from((33, "Label"))),
                                        segments: vec![]
                                    }))
                                },
                                Comma(TokenStream::from((38, ",")))
                            ),
                            (
                                PattType {
                                    meta_list: MetaList::default(),
                                    name: Ident(TokenStream::from((40, "layout"))),
                                    colon_token: (
                                        None,
                                        Colon(TokenStream::from((46, ":"))),
                                        Some(S(TokenStream::from((47, " "))))
                                    ),
                                    ty: Box::new(Type::Path(TypePath {
                                        meta_list: MetaList::default(),
                                        first: Ident(TokenStream::from((48, "TextLayout"))),
                                        segments: vec![]
                                    }))
                                },
                                Comma(TokenStream::from((58, ",")))
                            )
                        ],
                        last: Some(Box::new(PattType {
                            meta_list: MetaList::default(),
                            name: Ident(TokenStream::from((60, "fill"))),
                            colon_token: (
                                None,
                                Colon(TokenStream::from((64, ":"))),
                                Some(S(TokenStream::from((65, " "))))
                            ),
                            ty: Box::new(Type::Path(TypePath {
                                meta_list: MetaList::default(),
                                first: Ident(TokenStream::from((66, "Fill"))),
                                segments: vec![]
                            }))
                        })),
                    },
                    delimiter_end: RightParen(TokenStream::from((70, ")"))),
                    return_type: None,
                    block: (
                        None,
                        FnBlock::SemiColon(SemiColon(TokenStream::from((71, ";"))))
                    )
                },
                TokenStream::from((72, ""))
            ))
        );
    }

    #[test]
    fn parse_fn_with_return() {
        assert_eq!(
            ItemFn::parse(TokenStream::from(
                "extern fn label(@option len: f32) -> view;"
            )),
            Ok((
                ItemFn {
                    meta_list: MetaList::default(),
                    vs: None,
                    extern_token: Some((
                        KeywordExtern(TokenStream::from((0, "extern")),),
                        S(TokenStream::from((6, " ")))
                    )),
                    keyword: (
                        KeywordFn(TokenStream::from((7, "fn"))),
                        S(TokenStream::from((9, " ")))
                    ),
                    ident: (Ident(TokenStream::from((10, "label"))), None),
                    delimiter_start: LeftParen(TokenStream::from((15, "("))),
                    args: Punctuated {
                        items: vec![],
                        last: Some(Box::new(PattType {
                            meta_list: MetaList(vec![Meta::Attr(Attr {
                                keyword: At(TokenStream::from((16, "@"))),
                                ident: (
                                    Ident(TokenStream::from((17, "option"))),
                                    Some(S(TokenStream::from((23, " "))))
                                ),
                                params: None
                            })]),
                            name: Ident(TokenStream::from((24, "len"))),
                            colon_token: (
                                None,
                                Colon(TokenStream::from((27, ":"))),
                                Some(S(TokenStream::from((28, " "))))
                            ),
                            ty: Box::new(Type::F32(F32(TokenStream::from((29, "f32")))))
                        })),
                    },
                    delimiter_end: RightParen(TokenStream::from((32, ")"))),
                    return_type: Some(TypeReturn {
                        arrow_right: (
                            Some(S(TokenStream::from((33, " ")))),
                            ArrowRight(TokenStream::from((34, "->"))),
                            Some(S(TokenStream::from((36, " "))))
                        ),
                        ty: Box::new(Type::View(KeywordView(TokenStream::from((37, "view"))))),
                    }),
                    block: (
                        None,
                        FnBlock::SemiColon(SemiColon(TokenStream::from((41, ";"))))
                    )
                },
                TokenStream::from((42, ""))
            ))
        );
    }
}
