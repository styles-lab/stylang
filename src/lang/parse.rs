use parserc::{ControlFlow, Input, Parse, Parser, ParserExt, span::WithSpan};

use super::{Item, ParseError, S, TokenError, TokenStream};

/// Parse a source file.
pub fn parse(source: &str) -> Result<Vec<Item<TokenStream<'_>>>, ControlFlow<ParseError>> {
    let mut input = TokenStream::from(source);

    let mut stmts = vec![];

    loop {
        let stmt;
        (stmt, input) = Item::into_parser().ok().parse(input)?;

        if let Some(stmt) = stmt {
            stmts.push(stmt);

            (_, input) = S::into_parser().ok().parse(input)?;

            if input.is_empty() {
                return Ok(stmts);
            }

            continue;
        }

        return Err(ControlFlow::Fatal(ParseError::Unexpect(
            TokenError::Unknown,
            input.span(),
        )));
    }
}
