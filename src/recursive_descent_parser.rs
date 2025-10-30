use crate::ast::{self, Expr, LitVal};
use crate::scanner::token::{
    self,
    TokenType::{self, *},
};
use crate::span::{self, Span};

// Refactor and implement parse as macro and use pattern match instead of static
static EQ_TOKEN_TYPES: &[token::TokenType] = &[EqualEqual, BangEqual];
static CMP_TOKEN_TYPES: &[token::TokenType] = &[Greater, GreaterEqual, Less, LessEqual];
static TERM_TOKEN_TYPES: &[token::TokenType] = &[Plus, Minus];
static FACTOR_TOKEN_TYPES: &[token::TokenType] = &[Slash, Star];
static UNARY_TOKEN_TYPES: &[token::TokenType] = &[Bang, Minus];

#[derive(Debug)]
pub struct ParseError {
    token_opt: Option<token::Token>,
    msg: &'static str,
}

impl ParseError {
    fn new(token_opt: Option<token::Token>, msg: &'static str) -> Self {
        Self { token_opt, msg }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.token_opt {
            Some(t) => write!(f, "token: {}, msg: {}", t, self.msg),
            None => write!(f, "msg: {}", self.msg),
        }
    }
}

#[derive(Debug)]
pub struct RecursiveDescentParser {
    current: usize,
}

impl RecursiveDescentParser {
    pub fn new() -> Self {
        Self { current: 0 }
    }

    pub fn parse(&mut self, tokens: &Vec<token::Token>) -> Result<Vec<ast::Stmt>, ParseError> {
        let mut statements = Vec::new();

        while let Some(t) = self.peek_current(tokens) {
            match self.declaration(tokens) {
                Ok(t) => statements.push(t),
                Err(e) => {
                    // TODO: what should be returned to the cli on parse error?
                    self.synchronize(tokens);
                    println!("{}", e);
                }
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        match self.peek_current(tokens) {
            Some(t) if t.token_type == TokenType::Var => {
                self.advance(tokens);
                self.var_declaration(tokens)
            }
            _ => self.statement(tokens),
        }
    }

    fn var_declaration(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        self.advance(tokens)
            .map_or(
                Err(ParseError::new(None, "Expect token, got EOF")),
                |t| match &t.token_type {
                    TokenType::Identifier(name) => {
                        let initializer = match self.peek_current(tokens) {
                            Some(t) if t.token_type == TokenType::Equal => {
                                self.advance(tokens);
                                Some(self.expression(tokens)?)
                            }
                            _ => None,
                        };

                        match self.advance(tokens) {
                            Some(t) if t.token_type == TokenType::Semicolon => Ok(ast::Stmt::Var {
                                name: name.clone(),
                                initializer: initializer,
                                span: span::Span::new(t.clone()),
                            }),
                            Some(t) => Err(ParseError::new(
                                Some(t.clone()),
                                "Expect ';' after variable declaration",
                            )),
                            _ => Err(ParseError::new(
                                None,
                                "Expect ';' after variable declaration, got EOF",
                            )),
                        }
                    }

                    _ => return Err(ParseError::new(Some(t.clone()), "Expect variable name")),
                },
            )
    }

    fn statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, token::TokenType::Print) => {
                self.advance(tokens);
                Ok(ast::Stmt::Print {
                    rhs: self.print_statement(tokens)?,
                    span: span::Span::new(t.clone()),
                })
            }
            Some(t) if matches!(t.token_type, token::TokenType::LeftBrace) => {
                self.advance(tokens);
                Ok(ast::Stmt::Block {
                    statements: self.block(tokens)?,
                    span: span::Span::new(t.clone()),
                })
            }
            Some(t) => Ok(ast::Stmt::ExprStmt {
                inner: self.expression_statement(tokens)?,
                span: span::Span::new(t.clone()),
            }),
            None => panic!("tried to consume token with no more tokens available."),
        }
    }
    fn block(&mut self, tokens: &Vec<token::Token>) -> Result<Vec<ast::Stmt>, ParseError> {
        let mut statements = vec![];

        while let Some(_) = self
            .peek_current(tokens)
            .map(|token| match token {
                t if t.token_type == token::TokenType::RightBrace => None,
                t => Some(t),
            })
            .flatten()
        {
            statements.push(self.declaration(tokens)?);
        }

        match self.advance(tokens) {
            Some(t) if t.token_type == token::TokenType::RightBrace => Ok(statements),
            Some(t) => Err(ParseError::new(
                Some(t.clone()),
                "Expect '}' after block, got different token",
            )),
            _ => Err(ParseError::new(
                None,
                "Expect '}' after block, reached EOF.",
            )),
        }
    }

    fn print_statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let expr = self.expression(tokens)?;
        self.advance(tokens).map_or_else(
            || Err(ParseError::new(None, "Expect ';', got end of token stream")),
            |t| {
                if !matches!(t.token_type, token::TokenType::Semicolon) {
                    Err(ParseError::new(Some(t.clone()), "Expect ';' after value."))
                } else {
                    Ok(expr)
                }
            },
        )
    }

    fn expression_statement(
        &mut self,
        tokens: &Vec<token::Token>,
    ) -> Result<ast::Expr, ParseError> {
        let expr = self.expression(tokens)?;
        self.advance(tokens).map_or_else(
            || Err(ParseError::new(None, "Expect ';', got end of token stream")),
            |t| {
                if !matches!(t.token_type, token::TokenType::Semicolon) {
                    Err(ParseError::new(
                        Some(t.clone()),
                        "Expect ';' after expression.",
                    ))
                } else {
                    Ok(expr)
                }
            },
        )
    }

    fn expression(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.assignment(tokens)
    }

    fn assignment(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let expr = self.equality(tokens)?;

        self.peek_current(tokens).map_or(Ok(expr.clone()), |t| {
            if t.token_type == TokenType::Equal {
                self.advance(tokens);
                let r_value = self.assignment(tokens)?;

                match expr {
                    Expr::Variable { name, span } => Ok(Expr::Assign {
                        name: name,
                        value: Box::new(r_value),
                        span: span,
                    }),
                    _ => Err(ParseError::new(
                        Some(t.clone()),
                        "Invalid assignment target.",
                    )),
                }
            } else {
                Ok(expr)
            }
        })
    }

    fn equality(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.parse_binary_op(tokens, EQ_TOKEN_TYPES, Self::comparison)
    }

    fn comparison(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.parse_binary_op(tokens, CMP_TOKEN_TYPES, Self::term)
    }

    fn term(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.parse_binary_op(tokens, TERM_TOKEN_TYPES, Self::factor)
    }

    fn factor(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.parse_binary_op(tokens, FACTOR_TOKEN_TYPES, Self::unary)
    }

    fn unary(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        if let Some(un_op_token) = self
            .peek_current(tokens)
            .map(|t| {
                if match_token_types(UNARY_TOKEN_TYPES, t) {
                    self.advance(tokens)
                } else {
                    None
                }
            })
            .flatten()
        {
            use ast::UnOp::*;
            use token::TokenType::*;
            let un_op_ast = match un_op_token.token_type {
                Bang => Not,
                Minus => Neg,
                _ => panic!("expected unary operation token, got non-unary operation token"),
            };
            Ok(Expr::Unary {
                op: un_op_ast,
                rhs: Box::new(self.unary(tokens)?),
                span: span::Span::new(un_op_token.clone()),
            })
        } else {
            self.primary(tokens)
        }
    }

    fn primary(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        if let Some(token) = self.advance(tokens) {
            use token::TokenType::*;
            match &token.token_type {
                False => Ok(Expr::Literal {
                    inner: LitVal::False,
                    span: Span::new(token.clone()),
                }),
                True => Ok(Expr::Literal {
                    inner: LitVal::True,
                    span: Span::new(token.clone()),
                }),
                Nil => Ok(Expr::Literal {
                    inner: LitVal::Nil,
                    span: Span::new(token.clone()),
                }),
                String(val) => Ok(Expr::Literal {
                    inner: LitVal::String(val.to_string()),
                    span: Span::new(token.clone()),
                }),
                Number(val) => Ok(Expr::Literal {
                    inner: LitVal::Number(*val),
                    span: Span::new(token.clone()),
                }),
                Identifier(name) => Ok(Expr::Variable {
                    name: name.clone(),
                    span: Span::new(token.clone()),
                }),
                LeftParen => {
                    let expr = self.expression(tokens)?;

                    match self.advance(tokens) {
                        Some(t) if matches!(t.token_type, RightParen) => Ok(Expr::Grouping {
                            expr: Box::new(expr),
                            span: Span::new(token.clone()),
                        }),
                        _ => Err(ParseError::new(
                            Some(token.clone()),
                            "Expect ')' after expression.",
                        )),
                    }
                }
                _ => Err(ParseError::new(
                    Some(token.clone()),
                    "expected literal token, got non-literal token",
                )),
            }
        } else {
            Err(ParseError::new(
                None,
                "expected literal token, reached end of input",
            ))
        }
    }

    fn parse_binary_op(
        &mut self,
        tokens: &Vec<token::Token>,
        token_types: &[token::TokenType],
        precedence_match: fn(s: &mut Self, &Vec<token::Token>) -> Result<ast::Expr, ParseError>,
    ) -> Result<ast::Expr, ParseError> {
        let mut expr = precedence_match(self, tokens)?;

        while let Some(bin_op_token) = self
            .peek_current(tokens)
            .map(|t| {
                if match_token_types(token_types, t) {
                    self.advance(tokens)
                } else {
                    None
                }
            })
            .flatten()
        {
            use ast::BinOp::*;
            use token::TokenType::*;
            let bin_op_ast = match bin_op_token.token_type {
                EqualEqual => Eq,
                BangEqual => Ne,
                Less => Lt,
                LessEqual => Le,
                Greater => Gt,
                GreaterEqual => Ge,
                Plus => Add,
                Minus => Sub,
                Star => Mul,
                Slash => Div,
                _ => panic!("expected binary operation token, got non-binary operation token"),
            };
            let rhs = precedence_match(self, tokens)?;
            expr = Expr::Binary {
                op: bin_op_ast,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
                span: Span::new(bin_op_token.clone()),
            };
        }

        Ok(expr)
    }

    /// advance returns the token that is at index self.current and increments self.current by 1.
    /// `advance` assumes `tokens[length-1]` is EOF. When reaching EOF, `advance` returns `[None]`
    fn advance<'a>(&mut self, tokens: &'a Vec<token::Token>) -> Option<&'a token::Token> {
        if self.current + 1 < tokens.len() {
            self.current += 1;
            Some(&tokens[self.current - 1])
        } else {
            None
        }
    }

    /// peek_current returns the token at index self.current.
    /// `peek_current` assumes `tokens[length-1]` is EOF. When reaching EOF, `peek_current` returns `[None]`

    fn peek_current<'a>(&self, tokens: &'a Vec<token::Token>) -> Option<&'a token::Token> {
        if self.current < tokens.len() - 1 {
            Some(&tokens[self.current])
        } else {
            None
        }
    }

    /// `synchronize` should be called after a [`ParseError`]. It will consume any remaining tokens
    /// within the current statement and advance the current index to the start of the next
    /// statement.
    fn synchronize(&mut self, tokens: &Vec<token::Token>) {
        while let Some(token) = self.advance(tokens) {
            match token.token_type {
                // statement end, consume ';' and advance to start of next statement
                Semicolon => {
                    self.advance(tokens);
                    return;
                }
                // likely the start of a new statement
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => (),
            };
        }
    }
}

fn match_token_types(token_types: &[token::TokenType], token: &token::Token) -> bool {
    for token_type in token_types {
        if &token.token_type == token_type {
            return true;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    fn test_parse() {}
}
