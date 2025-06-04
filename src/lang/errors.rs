//! Types associated with error reporting by this module.

use parserc::span::Span;

/// Error variants return by compiler frontend.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LangError {
    /// Fallback error that catching the parserc raw error kind.
    #[error(transparent)]
    Fallback(#[from] parserc::Kind),
    /// Report an unexpect token error with span information.
    #[error("unexpect {kind} {span:?}")]
    Unexpect {
        /// token kind.
        kind: TokenKind,
        /// token span.
        span: Span,
        /// optional parsing item.
        item: Option<ItemKind>,
    },

    /// Report a token missing error with span information.
    #[error("unexpect {kind} {span:?}")]
    Expect {
        /// token kind.
        kind: TokenKind,
        /// token span.
        span: Span,
        /// optional parsing item.
        item: Option<ItemKind>,
    },
}

impl LangError {
    /// Create a [`LangError::Expect`] error without optional `item` field.
    pub fn expect(kind: TokenKind, span: Span) -> Self {
        Self::Expect {
            kind,
            span,
            item: None,
        }
    }

    /// Create a [`LangError::Unexpect`] error without optional `item` field.
    pub fn unexpect(kind: TokenKind, span: Span) -> Self {
        Self::Unexpect {
            kind,
            span,
            item: None,
        }
    }
}

impl From<(LangError, ItemKind)> for LangError {
    fn from(value: (LangError, ItemKind)) -> Self {
        match value.0 {
            LangError::Expect {
                kind,
                span,
                item: _,
            } => Self::Expect {
                kind,
                span,
                item: Some(value.1),
            },
            LangError::Unexpect {
                kind,
                span,
                item: _,
            } => Self::Unexpect {
                kind,
                span,
                item: Some(value.1),
            },
            parserc => parserc,
        }
    }
}

/// Error variants pointing to `stylang` item.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ItemKind {
    #[error("class{0:?}")]
    Class(Span),
    #[error("data{0:?}")]
    Data(Span),
    #[error("enum{0:?}")]
    Enum(Span),
    #[error("fn{0:?}")]
    Fn(Span),
    #[error("mod{0:?}")]
    Mod(Span),
    #[error("use{0:?}")]
    Use(Span),
    #[error("comments{0:?}")]
    Comments(Span),
}

/// Error variants pointing to lexical tokens.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum TokenKind {
    #[error("`unknown token`")]
    Unknown,
    /// token.
    #[error("token[`{0}`]")]
    Token(&'static str),
    /// space words
    #[error("`space words`")]
    S,
    /// decimal digits string.
    #[error("`digits`")]
    Digits,
    /// hexadecimal digits string.
    #[error("`hexdigits`")]
    HexDigits,
    /// hexadecimal sign part `0x`
    #[error("`0x`")]
    HexSign,
    #[error("`ident`")]
    Ident,
    #[error("`xml-ident`")]
    XmlIdent,
    #[error("`xml_end_tag`")]
    XmlEndTag(Span),
    #[error("`.. or ..=`")]
    RangeLimits,
    #[error("`expr-field`")]
    ExprField,
    #[error("`expr-member`")]
    Member,
    #[error("`expr-member`")]
    RightOperand,
    #[error("`expr-binary-op`")]
    ExprBinary,
    #[error("expr-index")]
    ExprIndex,
    #[error("`comments`")]
    Comments,
    #[error("`repeat-lit`")]
    RepeatLit,
    #[error("`repeat-len`")]
    RepeatLen,
    #[error("`expr-cond`")]
    Cond,
    #[error("`then-branch`")]
    Then,
    #[error("`else-branch`")]
    Else,
    #[error("`if-branch`")]
    If,
    #[error("`arm-expr`")]
    ArmExpr,
    #[error("`type`")]
    Type,
}
