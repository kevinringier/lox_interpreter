use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals,
    Identifier(String),
    String(String),
    Number(f64), // TODO: does this need to be a differe numeric value?

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl TokenType {
    fn to_string(&self) -> &str {
        match self {
            TokenType::LeftParen => "left_paren",
            TokenType::RightParen => "right_paren",
            TokenType::LeftBrace => "left_brace",
            TokenType::RightBrace => "right_brace",
            TokenType::Comma => "comma",
            TokenType::Dot => "dot",
            TokenType::Minus => "minus",
            TokenType::Plus => "plus",
            TokenType::Semicolon => "semicolon",
            TokenType::Slash => "slash",
            TokenType::Star => "star",
            TokenType::Bang => "bang",
            TokenType::BangEqual => "bang_equal",
            TokenType::Equal => "equal",
            TokenType::EqualEqual => "equal_equal",
            TokenType::Greater => "greater_than",
            TokenType::GreaterEqual => "greater_than_equal",
            TokenType::Less => "less_than",
            TokenType::LessEqual => "greater_than_equal",
            TokenType::Identifier(_) => "identifier",
            TokenType::String(_) => "string",
            TokenType::Number(_) => "number",
            TokenType::And => "and",
            TokenType::Class => "class",
            TokenType::Else => "else",
            TokenType::False => "false",
            TokenType::Fun => "fun",
            TokenType::For => "for",
            TokenType::If => "if",
            TokenType::Nil => "nil",
            TokenType::Or => "or",
            TokenType::Print => "print",
            TokenType::Return => "return",
            TokenType::Super => "super",
            TokenType::This => "this",
            TokenType::True => "true",
            TokenType::Var => "var",
            TokenType::While => "while",
            TokenType::Eof => "eof",
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        return Self {
            token_type,
            lexeme,
            line,
        };
    }

    fn to_string(&self) -> String {
        match &self.token_type {
            TokenType::String(literal) => format!(
                "type: {}, lexeme: '{}', line: {}",
                self.token_type.to_string(),
                self.lexeme,
                literal
            ),
            TokenType::Number(literal) => format!(
                "type: {}, lexeme: '{}', line: {}",
                self.token_type.to_string(),
                self.lexeme,
                literal
            ),
            _ => format!(
                "type: {}, lexeme: '{}'",
                self.token_type.to_string(),
                self.lexeme
            ),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
