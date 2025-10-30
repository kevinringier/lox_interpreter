use crate::scanner::token::Token;

/// `Span` contains data for compiler error reporting
#[derive(Clone, Debug)]
pub struct Span {
    token: Token,
}

impl Span {
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "token_type: {}, lexeme: {}, line: {}",
            self.token.token_type, self.token.lexeme, self.token.line,
        )
    }
}
