pub mod token;

use token::{Token, TokenType};

pub struct Scanner {
    start: usize,
    current: usize,
    line: usize,
}

// TODO: in the book, reserved words and identifiers

impl Scanner {
    pub fn new() -> Self {
        Self {
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self, source: &[u8]) -> Vec<Token> {
        let mut tokens = vec![];

        while let Some(_) = self.peek(source) {
            self.start = self.current;
            self.scan_token(source).map(|token| tokens.push(token));
        }

        tokens.push(Token::new(TokenType::Eof, String::from(""), self.line));

        tokens
    }

    fn scan_token(&mut self, source: &[u8]) -> Option<Token> {
        match self.advance(source).unwrap() {
            b'(' => Some(TokenType::LeftParen),
            b')' => Some(TokenType::RightParen),
            b'{' => Some(TokenType::LeftBrace),
            b'}' => Some(TokenType::RightBrace),
            b',' => Some(TokenType::Comma),
            b'.' => Some(TokenType::Dot),
            b'-' => Some(TokenType::Minus),
            b'+' => Some(TokenType::Plus),
            b';' => Some(TokenType::Semicolon),
            b'*' => Some(TokenType::Star),
            b'!' => {
                if let Some(b'=') = self.peek(source) {
                    self.advance(source);
                    Some(TokenType::BangEqual)
                } else {
                    Some(TokenType::Bang)
                }
            }
            b'=' => {
                if let Some(b'=') = self.peek(source) {
                    self.advance(source);
                    Some(TokenType::EqualEqual)
                } else {
                    Some(TokenType::Equal)
                }
            }
            b'<' => {
                if let Some(b'=') = self.peek(source) {
                    self.advance(source);
                    Some(TokenType::LessEqual)
                } else {
                    Some(TokenType::Less)
                }
            }
            b'>' => {
                if let Some(b'=') = self.peek(source) {
                    self.advance(source);
                    Some(TokenType::GreaterEqual)
                } else {
                    Some(TokenType::Greater)
                }
            }
            b'/' => {
                if let Some(b'/') = self.peek(source) {
                    // A comment goes until the end of line.
                    while let Some(byte) = self.peek(source) {
                        if byte != b'\n' {
                            self.advance(source);
                        } else {
                            self.line += 1;
                            break;
                        }
                    }
                    None
                } else {
                    Some(TokenType::Slash)
                }
            }
            b' ' | b'\r' | b'\t' => None,
            b'\n' => {
                self.line += 1;
                None
            }
            b'"' => match self.scan_string(source) {
                Ok(literal) => Some(TokenType::String(literal)),
                Err(_) => todo!("call error reporting here with context"),
            },
            b if is_digit(b) => Some(TokenType::Number(self.scan_number(source))),
            b if is_alphanumeric(b) => Some(get_identifier(self.scan_identifier(source))),
            _ => todo!("implement compile error and call"),
        }
        .map(|t| {
            let lexeme = String::from_utf8_lossy(&source[self.start..self.current]).to_string();
            Token::new(t, lexeme, self.line)
        })
    }

    fn advance(&mut self, source: &[u8]) -> Option<u8> {
        self.current += 1;
        if self.current < source.len() {
            return Some(source[self.current - 1]);
        }

        None
    }

    fn peek(&self, source: &[u8]) -> Option<u8> {
        if self.current + 1 < source.len() {
            return Some(source[self.current]);
        }

        None
    }

    fn peek_next(&self, source: &[u8]) -> Option<u8> {
        if self.current + 1 < source.len() {
            return Some(source[self.current + 1]);
        }

        None
    }

    fn scan_string(&mut self, source: &[u8]) -> Result<String, UnterminatedStringError> {
        while let Some(_) = match self.peek(source) {
            Some(b'\n') => {
                self.line += 1;
                Some(b'\n')
            }
            Some(b'"') => {
                self.advance(source);
                None
            }
            Some(b) => Some(b),
            None => return Err(UnterminatedStringError),
        } {
            self.advance(source);
        }

        Ok(String::from_utf8_lossy(&source[(self.start + 1)..(self.current - 1)]).to_string())
    }

    fn scan_number(&mut self, source: &[u8]) -> f64 {
        while let Some(_) = match self.peek(source) {
            Some(b) if is_digit(b) => Some(b),
            Some(b'.') => match self.peek_next(source) {
                Some(next_b) if is_digit(next_b) => Some(b'.'),
                _ => None, // TODO: do we leave the dot if the next byte is not a digit or is this
                           // an error?
            },
            _ => None,
        } {
            self.advance(source);
        }

        String::from_utf8_lossy(&source[self.start..self.current])
            .to_string()
            .parse::<f64>()
            // TODO: return error type and allow runtime recovery
            .expect("Failed to parse string to f64")
    }

    fn scan_identifier(&mut self, source: &[u8]) -> String {
        while let Some(_) = match self.peek(source) {
            Some(b) if is_alphanumeric(b) => Some(b),
            _ => None,
        } {
            self.advance(source);
        }

        String::from_utf8_lossy(&source[self.start..self.current]).to_string()
    }

    pub fn clear(&mut self) {
        self.start = 0;
        self.current = 0;
        self.line = 1;
    }
}

struct UnterminatedStringError;

fn is_digit(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}

fn is_alpha(byte: u8) -> bool {
    (byte >= b'a' && byte <= b'z') || (byte >= b'A' && byte <= b'Z') || (byte == b'_')
}

fn is_alphanumeric(byte: u8) -> bool {
    is_digit(byte) || is_alpha(byte)
}

fn get_identifier(identifier: String) -> TokenType {
    let str_slice: &str = &identifier;
    match str_slice {
        "and" => TokenType::And,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "false" => TokenType::False,
        "for" => TokenType::For,
        "fun" => TokenType::Fun,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "or" => TokenType::Or,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        _ => TokenType::Identifier(identifier),
    }
}
