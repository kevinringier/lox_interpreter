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

