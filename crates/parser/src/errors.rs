//! Types associated with error reporting by this module.

use parserc::{errors::ErrorKind, lang::Span, span::ToSpan};

/// Error variants return by compiler frontend.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum LangError {
    /// Unhandle parsing error.
    #[error(transparent)]
    Fallback(parserc::errors::ErrorKind<usize>),

    /// Report an unexpect token error with span information.
    #[error("unexpect {kind} {span:?}")]
    Unexpect {
        /// token kind.
        kind: SyntaxKind,
        /// token span.
        span: Span,
        /// optional parsing item.
        item: Option<ItemKind>,
    },

    /// Report a token missing error with span information.
    #[error("unexpect {kind} {span:?}")]
    Expect {
        /// token kind.
        kind: SyntaxKind,
        /// token span.
        span: Span,
        /// optional parsing item.
        item: Option<ItemKind>,
    },
    /// Report a invalid literial value.
    #[error("invalid literial value {kind} {span:?}")]
    Invalid {
        /// token kind.
        kind: SyntaxKind,
        /// token span.
        span: Span,
        /// optional parsing item.
        item: Option<ItemKind>,
    },

    #[error("Unparsed source {0:?}")]
    Unparsed(Span),
}

impl From<ErrorKind<usize>> for LangError {
    fn from(value: ErrorKind<usize>) -> Self {
        match value {
            ErrorKind::Token(name, span) => Self::expect(SyntaxKind::Token(name), span),
            value => Self::Fallback(value),
        }
    }
}

impl parserc::errors::ParseError<usize> for LangError {}

impl LangError {
    /// Create a [`LangError::Expect`] error without optional `item` field.
    pub fn expect(kind: SyntaxKind, span: Span) -> Self {
        Self::Expect {
            kind,
            span,
            item: None,
        }
    }

    /// Create a [`LangError::Unexpect`] error without optional `item` field.
    pub fn unexpect(kind: SyntaxKind, span: Span) -> Self {
        Self::Unexpect {
            kind,
            span,
            item: None,
        }
    }

    /// Create a [`LangError::Invalid`] error without optional `item` field.
    pub fn invalid(kind: SyntaxKind, span: Span) -> Self {
        Self::Invalid {
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
            LangError::Invalid {
                kind,
                span,
                item: _,
            } => Self::Invalid {
                kind,
                span,
                item: Some(value.1),
            },
            parserc => parserc,
        }
    }
}

impl ToSpan<usize> for LangError {
    fn to_span(&self) -> parserc::span::Span<usize> {
        match self {
            LangError::Fallback(error_kind) => error_kind.to_span(),
            LangError::Unexpect {
                kind: _,
                span,
                item: _,
            } => span.clone(),
            LangError::Expect {
                kind: _,
                span,
                item: _,
            } => span.clone(),
            LangError::Invalid {
                kind: _,
                span,
                item: _,
            } => span.clone(),
            LangError::Unparsed(span) => span.clone(),
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
    MetaList(Span),
}

/// Error variants pointing to lexical tokens.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum SyntaxKind {
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
    #[error("`xml_node`")]
    XmlNode,
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
    #[error("`arm-patt`")]
    ArmPatt,
    #[error("`arm-body`")]
    ArmBody,
    #[error("`match-expr`")]
    MatchExpr,
    #[error("`match-arms`")]
    MatchArms,
    #[error("`type`")]
    Type,
    #[error("`hex-color`")]
    HexColor,
    #[error("`rgb-digits`")]
    RgbDigits,
    #[error("`class,data,fn,...`")]
    Item,
    #[error("`slice init expr`")]
    ExprSlice,
    #[error("`else ...`")]
    ElseBranch,
}
