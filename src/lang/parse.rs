use parserc::{ControlFlow, Input, Parse, Parser, ParserExt, span::WithSpan};

use super::{ParseError, Stmt, TokenError, TokenStream, skip_ws};

/// Parse a source file.
pub fn parse(source: &str) -> Result<Vec<Stmt<TokenStream<'_>>>, ControlFlow<ParseError>> {
    let mut input = TokenStream::from(source);

    let mut stmts = vec![];

    loop {
        let stmt;
        (stmt, input) = Stmt::into_parser().ok().parse(input)?;

        if let Some(stmt) = stmt {
            stmts.push(stmt);

            (_, input) = skip_ws(input)?;

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
