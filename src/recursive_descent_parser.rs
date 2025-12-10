use crate::ast::{self, Expr, LitVal};
use crate::scanner::token::{
    self,
    TokenType::{self, *},
};
use crate::span::{self, Span};

static EQ_TOKEN_TYPES: &[TokenType] = &[EqualEqual, BangEqual];
static CMP_TOKEN_TYPES: &[TokenType] = &[Greater, GreaterEqual, Less, LessEqual];
static TERM_TOKEN_TYPES: &[TokenType] = &[Plus, Minus];
static FACTOR_TOKEN_TYPES: &[TokenType] = &[Slash, Star];
static UNARY_TOKEN_TYPES: &[TokenType] = &[Bang, Minus];

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
    id_incrementor: usize,
}

impl RecursiveDescentParser {
    pub fn new() -> Self {
        Self {
            current: 0,
            id_incrementor: 0,
        }
    }

    fn get_new_id(&mut self) -> usize {
        self.id_incrementor += 1;
        self.id_incrementor
    }

    pub fn parse(&mut self, tokens: &Vec<token::Token>) -> Result<Vec<ast::Stmt>, ParseError> {
        let mut statements = Vec::new();

        while let Some(_) = self.peek_current(tokens) {
            match self.declaration(tokens) {
                Ok(t) => statements.push(t),
                Err(e) => {
                    // FIX: collect all errors and return error type
                    self.synchronize(tokens);
                    println!("{}", e);
                }
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, TokenType::Var) => self.var_declaration(tokens),
            Some(t) if matches!(t.token_type, TokenType::Fun) => {
                self.function(tokens, FunctionType::Function)
            }
            Some(t) if matches!(t.token_type, TokenType::Class) => self.class_declaration(tokens),
            _ => self.statement(tokens),
        }
    }

    fn class_declaration(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        let span = span::Span::new(self.consume(TokenType::Class, tokens, "Expect class token")?);

        if let Some(token) = self.advance(tokens) {
            let name = match &token.token_type {
                TokenType::Identifier(name) => name.clone(),
                _ => Err(ParseError::new(Some(token.clone()), "Expect class name"))?,
            };

            self.consume(
                TokenType::LeftBrace,
                tokens,
                "Expect '{' before class body.",
            )?;

            let mut methods = vec![];
            while let Some(_) = match self.peek_current(tokens) {
                Some(t) if !matches!(t.token_type, TokenType::RightBrace) => Some(t),
                _ => None,
            } {
                methods.push(self.function(tokens, FunctionType::Method)?);
            }

            self.consume(
                TokenType::RightBrace,
                tokens,
                "Expect '}' after class body.",
            )?;

            Ok(ast::Stmt::Class {
                name,
                methods,
                span,
            })
        } else {
            Err(ParseError::new(None, "Expect token, got EOF"))
        }
    }

    fn var_declaration(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        let span = span::Span::new(self.consume(TokenType::Var, tokens, "Expect 'var' token")?);

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
                                span: span,
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

    fn function(
        &mut self,
        tokens: &Vec<token::Token>,
        function_type: FunctionType, // TODO: enum
    ) -> Result<ast::Stmt, ParseError> {
        let span: Span;
        if matches!(function_type, FunctionType::Function) {
            let fun_token = self.consume(TokenType::Fun, tokens, "Expect function token")?;
            span = span::Span::new(fun_token);
        } else {
            span = span::Span::new(self.peek_current(tokens).expect("").clone());
        }

        let name = self
            .advance(tokens)
            .map(|t| match &t.token_type {
                TokenType::Identifier(name) => Ok(name.clone()),
                _ => Err(ParseError::new(Some(t.clone()), "Expect function name.")),
            })
            .unwrap_or(Err(ParseError::new(None, "Expect function name, got EOF")))?;

        self.consume(
            TokenType::LeftParen,
            tokens,
            "Expect '(' after function name.",
        )?;

        let mut params = vec![];

        while let Some(t) = match self.peek_current(tokens) {
            Some(t) if !matches!(t.token_type, TokenType::RightParen) => Some(t),
            _ => None,
        } {
            if !matches!(t.token_type, TokenType::Comma) {
                if params.len() >= 255 {
                    Err(ParseError::new(
                        Some(t.clone()),
                        "Can't have more than 255 parameters",
                    ))?
                }

                let param_name = match &t.token_type {
                    TokenType::Identifier(name) => Ok(name.clone()),
                    _ => Err(ParseError::new(
                        Some(t.clone()),
                        "Expect IDENTIFIER for parameter name.",
                    )),
                }?;
                params.push(param_name);
            }

            self.advance(tokens);
        }

        self.consume(TokenType::RightParen, tokens, "Expect ')' after parameters")?;

        self.consume(
            TokenType::LeftBrace,
            tokens,
            "Expect '{' before function body",
        )?;

        let body = self.block(tokens)?;

        Ok(ast::Stmt::Function {
            name,
            params,
            body,
            span,
        })
    }

    fn statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, TokenType::Print) => {
                self.advance(tokens);
                Ok(ast::Stmt::Print {
                    rhs: self.print_statement(tokens)?,
                    span: span::Span::new(t.clone()),
                })
            }
            Some(t) if matches!(t.token_type, TokenType::Return) => self.return_statement(tokens),
            Some(t) if matches!(t.token_type, TokenType::LeftBrace) => {
                self.advance(tokens);
                Ok(ast::Stmt::Block {
                    statements: self.block(tokens)?,
                    span: span::Span::new(t.clone()),
                })
            }
            Some(t) if matches!(t.token_type, TokenType::If) => {
                self.advance(tokens);
                let (condition, then, else_branch) = self.if_statement(tokens)?;
                Ok(ast::Stmt::If {
                    condition: condition,
                    then: Box::new(then),
                    else_branch: else_branch.map(|e_b| Box::new(e_b)),
                    span: span::Span::new(t.clone()),
                })
            }
            Some(t) if matches!(t.token_type, TokenType::While) => self.while_statement(tokens),
            Some(t) if matches!(t.token_type, TokenType::For) => self.for_statement(tokens),
            Some(_) => self.expression_statement(tokens),
            None => panic!("tried to consume token with no more tokens available."),
        }
    }

    fn for_statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        // the for statement is desugared into a while statement. We will use the span for 'for'
        // for all desugared ast nodes.
        let for_span =
            span::Span::new(self.consume(TokenType::For, tokens, "Expect 'for' token")?);

        self.consume(TokenType::LeftParen, tokens, "Expect '(' after 'for'.")?;

        let initializier_opt = if let Some(t) = self.peek_current(tokens) {
            if matches!(t.token_type, TokenType::Semicolon) {
                self.advance(tokens);
                None
            } else if matches!(t.token_type, TokenType::Var) {
                Some(self.var_declaration(tokens)?)
            } else {
                Some(self.expression_statement(tokens)?)
            }
        } else {
            Err(ParseError::new(None, "Expect token, reached EOF"))?
        };

        let condition_opt = if let Some(t) = self.peek_current(tokens) {
            if !matches!(t.token_type, TokenType::Semicolon) {
                Some(self.expression(tokens)?)
            } else {
                None
            }
        } else {
            Err(ParseError::new(None, "Expect token, reached EOF"))?
        };

        self.consume(
            TokenType::Semicolon,
            tokens,
            "Expect ';' after loop condition.",
        )?;

        let incrementor_opt = if let Some(t) = self.peek_current(tokens) {
            if !matches!(t.token_type, TokenType::RightParen) {
                Some(ast::Stmt::ExprStmt {
                    inner: self.expression(tokens)?,
                    span: span::Span::new(t.clone()),
                })
            } else {
                None
            }
        } else {
            Err(ParseError::new(None, "Expect token, reached EOF"))?
        };

        self.consume(TokenType::RightParen, tokens, "Expect ')' after clauses.")?;

        let mut body = self.statement(tokens)?;

        if let Some(incrementor_stmt) = incrementor_opt {
            body = ast::Stmt::Block {
                statements: vec![body, incrementor_stmt],
                span: for_span.clone(),
            }
        };

        let condition = if let Some(condition_expr) = condition_opt {
            condition_expr
        } else {
            // TODO: span is dummy
            ast::Expr::Literal {
                inner: LitVal::True,
                span: for_span.clone(),
            }
        };

        body = ast::Stmt::While {
            condition: condition,
            body: Box::new(body),
            span: for_span.clone(),
        };

        if let Some(intiailizer) = initializier_opt {
            body = ast::Stmt::Block {
                statements: vec![intiailizer, body],
                span: for_span.clone(),
            }
        };

        Ok(body)
    }

    fn while_statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        let while_token = self.consume(TokenType::While, tokens, "Expect 'while' token.")?;

        self.consume(TokenType::LeftParen, tokens, "Expect '(' after 'while'.")?;

        let condition = self.expression(tokens)?;

        self.consume(TokenType::RightParen, tokens, "expect ') after condition.")?;

        let body = self.statement(tokens)?;

        Ok(ast::Stmt::While {
            condition: condition,
            body: Box::new(body),
            span: span::Span::new(while_token),
        })
    }

    fn if_statement(
        &mut self,
        tokens: &Vec<token::Token>,
    ) -> Result<(ast::Expr, ast::Stmt, Option<ast::Stmt>), ParseError> {
        match self.advance(tokens) {
            Some(t) if matches!(t.token_type, TokenType::LeftParen) => {
                let condition = self.expression(tokens)?;
                match self.advance(tokens) {
                    Some(t) if matches!(t.token_type, TokenType::RightParen) => {
                        let then = self.statement(tokens)?;

                        let else_branch = match self.peek_current(tokens) {
                            Some(t) if matches!(t.token_type, TokenType::Else) => {
                                self.advance(tokens);
                                Some(self.statement(tokens)?)
                            }
                            _ => None,
                        };

                        Ok((condition, then, else_branch))
                    }
                    Some(t) => Err(ParseError::new(
                        Some(t.clone()),
                        "Expect ')' after if condition.",
                    )),
                    None => Err(ParseError::new(None, "Expect '(' after if condition.")),
                }
            }
            Some(t) => Err(ParseError::new(Some(t.clone()), "Expect '(' after 'if'.")),
            None => Err(ParseError::new(None, "Expect '(' after 'if'.")),
        }
    }

    fn block(&mut self, tokens: &Vec<token::Token>) -> Result<Vec<ast::Stmt>, ParseError> {
        let mut statements = vec![];

        while let Some(_) = self
            .peek_current(tokens)
            .map(|token| match token {
                t if t.token_type == TokenType::RightBrace => None,
                t => Some(t),
            })
            .flatten()
        {
            statements.push(self.declaration(tokens)?);
        }

        match self.advance(tokens) {
            Some(t) if t.token_type == TokenType::RightBrace => Ok(statements),
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
                if !matches!(t.token_type, TokenType::Semicolon) {
                    Err(ParseError::new(Some(t.clone()), "Expect ';' after value."))
                } else {
                    Ok(expr)
                }
            },
        )
    }

    fn return_statement(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Stmt, ParseError> {
        let keyword = self
            .advance(tokens)
            .expect("return_statement called without current token being return");

        let value = match self.peek_current(tokens) {
            Some(t) if !matches!(t.token_type, TokenType::Semicolon) => {
                Some(self.expression(tokens)?)
            }
            _ => None,
        };

        self.consume(
            TokenType::Semicolon,
            tokens,
            "Expect ';' after return value",
        )?;

        Ok(ast::Stmt::Return {
            keyword: keyword.clone(),
            value: value,
            span: span::Span::new(keyword.clone()),
        })
    }

    fn expression_statement(
        &mut self,
        tokens: &Vec<token::Token>,
    ) -> Result<ast::Stmt, ParseError> {
        let span = span::Span::new(self.peek_current(tokens).unwrap().clone());
        let expr = self.expression(tokens)?;

        self.advance(tokens).map_or_else(
            || Err(ParseError::new(None, "Expect ';', got end of token stream")),
            |t| {
                if !matches!(t.token_type, TokenType::Semicolon) {
                    Err(ParseError::new(
                        Some(t.clone()),
                        "Expect ';' after expression.",
                    ))
                } else {
                    Ok(ast::Stmt::ExprStmt {
                        inner: expr,
                        span: span,
                    })
                }
            },
        )
    }

    fn expression(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        self.assignment(tokens)
    }

    fn assignment(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let expr = self.or(tokens)?;

        self.peek_current(tokens).map_or(Ok(expr.clone()), |t| {
            if t.token_type == TokenType::Equal {
                self.advance(tokens);
                let r_value = self.assignment(tokens)?;

                match expr {
                    Expr::Variable { id, name, span } => Ok(Expr::Assign {
                        id,
                        name: name,
                        value: Box::new(r_value),
                        span: span,
                    }),
                    Expr::Get { object, name, span } => {
                        let value = Box::new(r_value);
                        Ok(Expr::Set {
                            object,
                            name,
                            value,
                            span,
                        })
                    }
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

    fn or(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let mut expr = self.and(tokens)?;

        while let Some(t) = match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, TokenType::Or) => {
                self.advance(tokens);
                Some(t)
            }
            _ => None,
        } {
            let left = Box::new(expr);
            let right = Box::new(self.and(tokens)?);
            let span = span::Span::new(t.clone());

            expr = ast::Expr::LogicOr { left, right, span };
        }

        Ok(expr)
    }

    fn and(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let mut expr = self.equality(tokens)?;

        while let Some(t) = match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, TokenType::And) => {
                self.advance(tokens);
                Some(t)
            }
            _ => None,
        } {
            let left = Box::new(expr);
            let right = Box::new(self.and(tokens)?);
            let span = span::Span::new(t.clone());

            expr = ast::Expr::LogicAnd { left, right, span };
        }

        Ok(expr)
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
            use TokenType::*;
            use ast::UnOp::*;
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
            self.call(tokens)
        }
    }

    fn call(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        let mut expr = self.primary(tokens)?;

        // while it's a '(' and we successfully finished parsing the call, parse another call
        // setting the result of the finished parse as the new (outer most) callee
        while let Some(t) = match self.peek_current(tokens) {
            Some(t) if matches!(t.token_type, TokenType::LeftParen | TokenType::Dot) => Some(t),
            _ => None,
        } {
            if matches!(t.token_type, TokenType::LeftParen) {
                self.advance(tokens);
                expr = self.finish_call(expr, tokens)?;
            } else if matches!(t.token_type, TokenType::Dot) {
                let span = span::Span::new(self.advance(tokens).unwrap().clone());
                let name = match self.advance(tokens) {
                    Some(t) => match &t.token_type {
                        Identifier(name) => name.clone(),
                        _ => Err(ParseError::new(
                            Some(t.clone()),
                            "Expect property name after '.'.",
                        ))?,
                    },
                    _ => Err(ParseError::new(
                        None,
                        "Expect property name after .', got EOF",
                    ))?,
                };
                expr = ast::Expr::Get {
                    object: Box::new(expr),
                    name: name,
                    span: span,
                }
            }
        }

        Ok(expr)
    }

    fn finish_call(
        &mut self,
        callee: Expr,
        tokens: &Vec<token::Token>,
    ) -> Result<ast::Expr, ParseError> {
        if let Some(t) = self.peek_current(tokens) {
            let mut args = vec![];
            if !matches!(t.token_type, TokenType::RightParen) {
                let mut parse_another_arg = true;

                while parse_another_arg {
                    // TODO: this doesn't have to return an error immediately, during parsing. We
                    // could report the error using some other mechanism and continue parsing.
                    if args.len() > 255 {
                        Err(ParseError::new(
                            Some(t.clone()),
                            "Can't have more than 255 arguments",
                        ))?;
                    }
                    args.push(self.expression(tokens)?);

                    match self.peek_current(tokens) {
                        Some(t) if matches!(t.token_type, TokenType::Comma) => {
                            self.advance(tokens);
                        }
                        _ => parse_another_arg = false,
                    }
                }
            }

            match self.advance(tokens) {
                Some(t) if matches!(t.token_type, TokenType::RightParen) => Ok(ast::Expr::Call {
                    callee: Box::new(callee),
                    args: args,
                    span: span::Span::new(t.clone()),
                }),
                Some(t) => Err(ParseError::new(
                    Some(t.clone()),
                    "Expect ')' after arguments",
                )),
                None => Err(ParseError::new(None, "Expect ')' after arguments, got EOF")),
            }
        } else {
            Err(ParseError::new(None, "Expect argument or ')', got EOF"))
        }
    }

    fn primary(&mut self, tokens: &Vec<token::Token>) -> Result<ast::Expr, ParseError> {
        if let Some(token) = self.advance(tokens) {
            use TokenType::*;
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
                    id: self.get_new_id(),
                    name: name.clone(),
                    span: Span::new(token.clone()),
                }),
                This => Ok(Expr::This {
                    id: self.get_new_id(),
                    keyword: token.lexeme.clone(),
                    span: span::Span::new(token.clone()),
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
        token_types: &[TokenType],
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
            use TokenType::*;
            use ast::BinOp::*;
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

    fn consume<'a>(
        &mut self,
        token_type: TokenType,
        tokens: &'a Vec<token::Token>,
        msg: &'static str,
    ) -> Result<token::Token, ParseError> {
        if self.current + 1 < tokens.len() {
            self.current += 1;
            let t = tokens[self.current - 1].clone();
            if t.token_type == token_type {
                Ok(t)
            } else {
                Err(ParseError::new(Some(t), msg))
            }
        } else {
            Err(ParseError::new(None, "Expected a token, reached EOF"))
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

fn match_token_types(token_types: &[TokenType], token: &token::Token) -> bool {
    for token_type in token_types {
        if &token.token_type == token_type {
            return true;
        }
    }
    false
}

enum FunctionType {
    Function,
    Method,
}

#[cfg(test)]
mod tests {
    fn test_parse() {}
}
