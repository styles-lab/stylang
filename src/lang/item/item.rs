use parserc::{Parse, Parser, ParserExt, Punctuated, derive_parse};

use crate::lang::{
    errors::{ItemKind, LangError},
    input::LangInput,
    item::{ItemFn, Visibility},
    meta::MetaList,
    patt::PattType,
    token::{
        Brace, Ident, KeywordClass, KeywordData, KeywordEnum, KeywordMod, KeywordUse, Paren,
        SepColonColon, SepComma, SepSemiColon, TokenStar,
    },
    ty::Type,
};

/// Name field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct NameField<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// field type ascription pattern.
    pub patt: PattType<I>,
}

/// Uname field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UnameField<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// field type.
    pub ty: Type<I>,
}

/// Name field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum Fields<I>
where
    I: LangInput,
{
    Name(Brace<I, Punctuated<NameField<I>, SepComma<I>>>),
    Uname(
        Paren<I, Punctuated<UnameField<I>, SepComma<I>>>,
        SepSemiColon<I>,
    ),
}

/// Item `class`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemClass<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `class[s]+`
    pub keyword: KeywordClass<I>,
    /// class name.
    #[fatal]
    pub ident: Ident<I>,
    /// field list.
    #[fatal]
    pub fields: Fields<I>,
}

/// Item `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemData<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `data[s]+`
    pub keyword: KeywordData<I>,
    /// class name.
    #[fatal]
    pub ident: Ident<I>,
    /// field list.
    #[fatal]
    pub fields: Fields<I>,
}

/// Name field for `class` or `data`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum VariantFields<I>
where
    I: LangInput,
{
    Name(Brace<I, Punctuated<NameField<I>, SepComma<I>>>),
    Uname(Paren<I, Punctuated<UnameField<I>, SepComma<I>>>),
}

/// Field for [`ItemEnum`]
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct Variant<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// variant name.
    pub ident: Ident<I>,
    /// optional field list clause.
    pub fields: Option<VariantFields<I>>,
}

/// Item `enum`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemEnum<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `enum[s]+`
    pub keyword: KeywordEnum<I>,
    /// class name.
    #[fatal]
    pub ident: Ident<I>,
    /// variant list.
    #[fatal]
    pub variants: Brace<I, Punctuated<Variant<I>, SepComma<I>>>,
}

/// Item `mod`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemMod<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `mod[s]+`
    pub keyword: KeywordMod<I>,
    /// mod name.
    #[fatal]
    pub ident: Ident<I>,
    /// end token: `;`
    #[fatal]
    pub semi_colon: SepSemiColon<I>,
}

/// Path used by `ItemUse`
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct UsePath<I>
where
    I: LangInput,
{
    /// first element of use path.
    pub first: Ident<I>,
    pub rest: Vec<(SepColonColon<I>, UseSegment<I>)>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub enum UseSegment<I>
where
    I: LangInput,
{
    Ident(Ident<I>),
    Glob(TokenStar<I>),
    Group(Brace<I, Punctuated<UsePath<I>, SepComma<I>>>),
}

/// Item `use`.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[derive_parse(error = LangError,input = I)]
pub struct ItemUse<I>
where
    I: LangInput,
{
    /// leading meta-data list.
    pub meta_list: MetaList<I>,
    /// visibilty clause.
    pub vs: Option<Visibility<I>>,
    /// keyword `use[s]+`
    pub keyword: KeywordUse<I>,
    /// path clause.
    #[fatal]
    pub path: UsePath<I>,
    /// end token: `;`
    #[fatal]
    pub semi_colon: SepSemiColon<I>,
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
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
    MetaList(MetaList<I>),
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
            .or(MetaList::into_parser().map(|v| Self::MetaList(v)))
            .parse(input)
    }
}

#[cfg(test)]
mod tests {
    use parserc::{ControlFlow, Delimiter, Parse, span::Span};

    use super::*;
    use crate::lang::{
        errors::TokenKind, input::TokenStream, item::Scope, meta::Meta, token::*, ty::TypeNum,
    };

    #[test]
    fn use_path() {
        assert_eq!(
            Item::parse(TokenStream::from("use a::b::{c,d::*};")),
            Ok((
                Item::Use(ItemUse {
                    meta_list: vec![],
                    vs: None,
                    keyword: KeywordUse(
                        TokenStream {
                            offset: 0,
                            value: "use"
                        },
                        S(TokenStream {
                            offset: 3,
                            value: " "
                        })
                    ),
                    path: UsePath {
                        first: Ident(TokenStream {
                            offset: 4,
                            value: "a"
                        }),
                        rest: vec![
                            (
                                SepColonColon(
                                    None,
                                    TokenStream {
                                        offset: 5,
                                        value: "::"
                                    },
                                    None
                                ),
                                UseSegment::Ident(Ident(TokenStream {
                                    offset: 7,
                                    value: "b"
                                }))
                            ),
                            (
                                SepColonColon(
                                    None,
                                    TokenStream {
                                        offset: 8,
                                        value: "::"
                                    },
                                    None
                                ),
                                UseSegment::Group(Delimiter {
                                    delimiter_start: SepLeftBrace(
                                        None,
                                        TokenStream {
                                            offset: 10,
                                            value: "{"
                                        },
                                        None
                                    ),
                                    body: Punctuated {
                                        pairs: vec![(
                                            UsePath {
                                                first: Ident(TokenStream {
                                                    offset: 11,
                                                    value: "c"
                                                }),
                                                rest: vec![]
                                            },
                                            SepComma(
                                                None,
                                                TokenStream {
                                                    offset: 12,
                                                    value: ","
                                                },
                                                None
                                            )
                                        )],
                                        tail: Some(Box::new(UsePath {
                                            first: Ident(TokenStream {
                                                offset: 13,
                                                value: "d"
                                            }),
                                            rest: vec![(
                                                SepColonColon(
                                                    None,
                                                    TokenStream {
                                                        offset: 14,
                                                        value: "::"
                                                    },
                                                    None
                                                ),
                                                UseSegment::Glob(TokenStar(TokenStream {
                                                    offset: 16,
                                                    value: "*"
                                                }))
                                            )]
                                        }))
                                    },
                                    delimiter_end: SepRightBrace(
                                        None,
                                        TokenStream {
                                            offset: 17,
                                            value: "}"
                                        },
                                        None
                                    )
                                })
                            )
                        ]
                    },
                    semi_colon: SepSemiColon(
                        None,
                        TokenStream {
                            offset: 18,
                            value: ";"
                        },
                        None
                    )
                }),
                TokenStream {
                    offset: 19,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn item_tuple_data() {
        assert_eq!(
            Item::parse(TokenStream::from(
                r#"
                data A(i32,string);
                "#
            )),
            Ok((
                Item::Data(ItemData {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n                "
                    }))],
                    vs: None,
                    keyword: KeywordData(
                        TokenStream {
                            offset: 17,
                            value: "data"
                        },
                        S(TokenStream {
                            offset: 21,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 22,
                        value: "A"
                    }),
                    fields: Fields::Uname(
                        Delimiter {
                            delimiter_start: SepLeftParen(
                                None,
                                TokenStream {
                                    offset: 23,
                                    value: "("
                                },
                                None
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    UnameField {
                                        meta_list: vec![],
                                        ty: Type::Num(TypeNum::I32(TokenI32(TokenStream {
                                            offset: 24,
                                            value: "i32"
                                        })))
                                    },
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 27,
                                            value: ","
                                        },
                                        None
                                    )
                                )],
                                tail: Some(Box::new(UnameField {
                                    meta_list: vec![],
                                    ty: Type::String(TokenString(TokenStream {
                                        offset: 28,
                                        value: "string"
                                    }))
                                }))
                            },
                            delimiter_end: SepRightParen(
                                None,
                                TokenStream {
                                    offset: 34,
                                    value: ")"
                                },
                                None
                            )
                        },
                        SepSemiColon(
                            None,
                            TokenStream {
                                offset: 35,
                                value: ";"
                            },
                            Some(S(TokenStream {
                                offset: 36,
                                value: "\n                "
                            }))
                        )
                    )
                }),
                TokenStream {
                    offset: 53,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn tuple_class() {
        assert_eq!(
            Item::parse(TokenStream::from(
                r#"
                class A(i32,string);
                "#,
            )),
            Ok((
                Item::Class(ItemClass {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n                "
                    }))],
                    vs: None,
                    keyword: KeywordClass(
                        TokenStream {
                            offset: 17,
                            value: "class"
                        },
                        S(TokenStream {
                            offset: 22,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 23,
                        value: "A"
                    }),
                    fields: Fields::Uname(
                        Delimiter {
                            delimiter_start: SepLeftParen(
                                None,
                                TokenStream {
                                    offset: 24,
                                    value: "("
                                },
                                None
                            ),
                            body: Punctuated {
                                pairs: vec![(
                                    UnameField {
                                        meta_list: vec![],
                                        ty: Type::Num(TypeNum::I32(TokenI32(TokenStream {
                                            offset: 25,
                                            value: "i32"
                                        })))
                                    },
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 28,
                                            value: ","
                                        },
                                        None
                                    )
                                )],
                                tail: Some(Box::new(UnameField {
                                    meta_list: vec![],
                                    ty: Type::String(TokenString(TokenStream {
                                        offset: 29,
                                        value: "string"
                                    }))
                                }))
                            },
                            delimiter_end: SepRightParen(
                                None,
                                TokenStream {
                                    offset: 35,
                                    value: ")"
                                },
                                None
                            )
                        },
                        SepSemiColon(
                            None,
                            TokenStream {
                                offset: 36,
                                value: ";"
                            },
                            Some(S(TokenStream {
                                offset: 37,
                                value: "\n                "
                            }))
                        )
                    )
                }),
                TokenStream {
                    offset: 54,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn class() {
        assert_eq!(
            Item::parse(TokenStream::from(
                r#"
            class A {
                name: string,
                level: u32,
            }
                "#
            )),
            Ok((
                Item::Class(ItemClass {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n            "
                    }))],
                    vs: None,
                    keyword: KeywordClass(
                        TokenStream {
                            offset: 13,
                            value: "class"
                        },
                        S(TokenStream {
                            offset: 18,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 19,
                        value: "A"
                    }),
                    fields: Fields::Name(Delimiter {
                        delimiter_start: SepLeftBrace(
                            Some(S(TokenStream {
                                offset: 20,
                                value: " "
                            })),
                            TokenStream {
                                offset: 21,
                                value: "{"
                            },
                            Some(S(TokenStream {
                                offset: 22,
                                value: "\n                "
                            }))
                        ),
                        body: Punctuated {
                            pairs: vec![
                                (
                                    NameField {
                                        meta_list: vec![],
                                        patt: PattType {
                                            ident: Ident(TokenStream {
                                                offset: 39,
                                                value: "name"
                                            }),
                                            sep: SepColon(
                                                None,
                                                TokenStream {
                                                    offset: 43,
                                                    value: ":"
                                                },
                                                Some(S(TokenStream {
                                                    offset: 44,
                                                    value: " "
                                                }))
                                            ),
                                            ty: Type::String(TokenString(TokenStream {
                                                offset: 45,
                                                value: "string"
                                            }))
                                        }
                                    },
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 51,
                                            value: ","
                                        },
                                        Some(S(TokenStream {
                                            offset: 52,
                                            value: "\n                "
                                        }))
                                    )
                                ),
                                (
                                    NameField {
                                        meta_list: vec![],
                                        patt: PattType {
                                            ident: Ident(TokenStream {
                                                offset: 69,
                                                value: "level"
                                            }),
                                            sep: SepColon(
                                                None,
                                                TokenStream {
                                                    offset: 74,
                                                    value: ":"
                                                },
                                                Some(S(TokenStream {
                                                    offset: 75,
                                                    value: " "
                                                }))
                                            ),
                                            ty: Type::Num(TypeNum::U32(TokenU32(TokenStream {
                                                offset: 76,
                                                value: "u32"
                                            })))
                                        }
                                    },
                                    SepComma(
                                        None,
                                        TokenStream {
                                            offset: 79,
                                            value: ","
                                        },
                                        Some(S(TokenStream {
                                            offset: 80,
                                            value: "\n            "
                                        }))
                                    )
                                )
                            ],
                            tail: None
                        },
                        delimiter_end: SepRightBrace(
                            None,
                            TokenStream {
                                offset: 93,
                                value: "}"
                            },
                            Some(S(TokenStream {
                                offset: 94,
                                value: "\n                "
                            }))
                        )
                    })
                }),
                TokenStream {
                    offset: 111,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn item_enum() {
        assert_eq!(
            Item::parse(TokenStream::from(
                r#"
        pub(crate) enum A {
            A (i32),
            B {
                name: string,
                value: f32
            }
        }
            "#
            )),
            Ok((
                Item::Enum(ItemEnum {
                    meta_list: vec![Meta::S(S(TokenStream {
                        offset: 0,
                        value: "\n        "
                    }))],
                    vs: Some(Visibility::Scope {
                        token: TokenPub(TokenStream {
                            offset: 9,
                            value: "pub"
                        }),
                        scope: Delimiter {
                            delimiter_start: SepLeftParen(
                                None,
                                TokenStream {
                                    offset: 12,
                                    value: "("
                                },
                                None
                            ),
                            body: Scope::Crate(TokenCrate(TokenStream {
                                offset: 13,
                                value: "crate"
                            })),
                            delimiter_end: SepRightParen(
                                None,
                                TokenStream {
                                    offset: 18,
                                    value: ")"
                                },
                                Some(S(TokenStream {
                                    offset: 19,
                                    value: " "
                                }))
                            )
                        }
                    }),
                    keyword: KeywordEnum(
                        TokenStream {
                            offset: 20,
                            value: "enum"
                        },
                        S(TokenStream {
                            offset: 24,
                            value: " "
                        })
                    ),
                    ident: Ident(TokenStream {
                        offset: 25,
                        value: "A"
                    }),
                    variants: Delimiter {
                        delimiter_start: SepLeftBrace(
                            Some(S(TokenStream {
                                offset: 26,
                                value: " "
                            })),
                            TokenStream {
                                offset: 27,
                                value: "{"
                            },
                            Some(S(TokenStream {
                                offset: 28,
                                value: "\n            "
                            }))
                        ),
                        body: Punctuated {
                            pairs: vec![(
                                Variant {
                                    meta_list: vec![],
                                    ident: Ident(TokenStream {
                                        offset: 41,
                                        value: "A"
                                    }),
                                    fields: Some(VariantFields::Uname(Delimiter {
                                        delimiter_start: SepLeftParen(
                                            Some(S(TokenStream {
                                                offset: 42,
                                                value: " "
                                            })),
                                            TokenStream {
                                                offset: 43,
                                                value: "("
                                            },
                                            None
                                        ),
                                        body: Punctuated {
                                            pairs: vec![],
                                            tail: Some(Box::new(UnameField {
                                                meta_list: vec![],
                                                ty: Type::Num(TypeNum::I32(TokenI32(
                                                    TokenStream {
                                                        offset: 44,
                                                        value: "i32"
                                                    }
                                                )))
                                            }))
                                        },
                                        delimiter_end: SepRightParen(
                                            None,
                                            TokenStream {
                                                offset: 47,
                                                value: ")"
                                            },
                                            None
                                        )
                                    }))
                                },
                                SepComma(
                                    None,
                                    TokenStream {
                                        offset: 48,
                                        value: ","
                                    },
                                    Some(S(TokenStream {
                                        offset: 49,
                                        value: "\n            "
                                    }))
                                )
                            )],
                            tail: Some(Box::new(Variant {
                                meta_list: vec![],
                                ident: Ident(TokenStream {
                                    offset: 62,
                                    value: "B"
                                }),
                                fields: Some(VariantFields::Name(Delimiter {
                                    delimiter_start: SepLeftBrace(
                                        Some(S(TokenStream {
                                            offset: 63,
                                            value: " "
                                        })),
                                        TokenStream {
                                            offset: 64,
                                            value: "{"
                                        },
                                        Some(S(TokenStream {
                                            offset: 65,
                                            value: "\n                "
                                        }))
                                    ),
                                    body: Punctuated {
                                        pairs: vec![(
                                            NameField {
                                                meta_list: vec![],
                                                patt: PattType {
                                                    ident: Ident(TokenStream {
                                                        offset: 82,
                                                        value: "name"
                                                    }),
                                                    sep: SepColon(
                                                        None,
                                                        TokenStream {
                                                            offset: 86,
                                                            value: ":"
                                                        },
                                                        Some(S(TokenStream {
                                                            offset: 87,
                                                            value: " "
                                                        }))
                                                    ),
                                                    ty: Type::String(TokenString(TokenStream {
                                                        offset: 88,
                                                        value: "string"
                                                    }))
                                                }
                                            },
                                            SepComma(
                                                None,
                                                TokenStream {
                                                    offset: 94,
                                                    value: ","
                                                },
                                                Some(S(TokenStream {
                                                    offset: 95,
                                                    value: "\n                "
                                                }))
                                            )
                                        )],
                                        tail: Some(Box::new(NameField {
                                            meta_list: vec![],
                                            patt: PattType {
                                                ident: Ident(TokenStream {
                                                    offset: 112,
                                                    value: "value"
                                                }),
                                                sep: SepColon(
                                                    None,
                                                    TokenStream {
                                                        offset: 117,
                                                        value: ":"
                                                    },
                                                    Some(S(TokenStream {
                                                        offset: 118,
                                                        value: " "
                                                    }))
                                                ),
                                                ty: Type::Num(TypeNum::F32(TokenF32(
                                                    TokenStream {
                                                        offset: 119,
                                                        value: "f32"
                                                    }
                                                )))
                                            }
                                        }))
                                    },
                                    delimiter_end: SepRightBrace(
                                        Some(S(TokenStream {
                                            offset: 122,
                                            value: "\n            "
                                        })),
                                        TokenStream {
                                            offset: 135,
                                            value: "}"
                                        },
                                        Some(S(TokenStream {
                                            offset: 136,
                                            value: "\n        "
                                        }))
                                    )
                                }))
                            }))
                        },
                        delimiter_end: SepRightBrace(
                            None,
                            TokenStream {
                                offset: 145,
                                value: "}"
                            },
                            Some(S(TokenStream {
                                offset: 146,
                                value: "\n            "
                            }))
                        )
                    }
                }),
                TokenStream {
                    offset: 159,
                    value: ""
                }
            ))
        );
    }

    #[test]
    fn item_mod_without_comma() {
        assert_eq!(
            Item::parse(TokenStream::from("pub use a")),
            Err(ControlFlow::Fatal(LangError::Expect {
                kind: TokenKind::Token(";"),
                span: Span { offset: 9, len: 0 },
                item: Some(ItemKind::Use(Span { offset: 0, len: 9 }))
            }))
        );
    }

    #[test]
    fn item_mod() {
        assert_eq!(
            Item::parse(TokenStream::from("pub use a;")),
            Ok((
                Item::Use(ItemUse {
                    meta_list: vec![],
                    vs: Some(Visibility::Outer {
                        token: TokenPub(TokenStream {
                            offset: 0,
                            value: "pub"
                        }),
                        tail_s: S(TokenStream {
                            offset: 3,
                            value: " "
                        })
                    }),
                    keyword: KeywordUse(
                        TokenStream {
                            offset: 4,
                            value: "use"
                        },
                        S(TokenStream {
                            offset: 7,
                            value: " "
                        })
                    ),
                    path: UsePath {
                        first: Ident(TokenStream {
                            offset: 8,
                            value: "a"
                        }),
                        rest: vec![]
                    },
                    semi_colon: SepSemiColon(
                        None,
                        TokenStream {
                            offset: 9,
                            value: ";"
                        },
                        None
                    )
                }),
                TokenStream {
                    offset: 10,
                    value: ""
                }
            ))
        );
    }
}
