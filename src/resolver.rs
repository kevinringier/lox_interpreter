use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprVisitor, Stmt, StmtVisitor},
    interpreter::Interpreter,
    span,
};

pub struct Resolver<'i> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'i mut Interpreter,
    current_function_type: FunctionType,
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        let scopes = vec![];
        let current_function_type = FunctionType::None;
        Self {
            scopes,
            interpreter,
            current_function_type,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) -> Result<(), ResolverError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name) {
                Err(ResolverError::new(
                    "Already a variable with this name in this scope.",
                ))
            } else {
                scope.insert(name, false);
                Ok(())
            }
        } else {
            Ok(())
        }
    }

    fn define(&mut self, name: String) {
        self.scopes.last_mut().map(|s| s.insert(name, true));
    }

    pub fn resolve_statements(&mut self, statements: &Vec<Stmt>) -> Result<(), ResolverError> {
        for statement in statements {
            self.resolve_statement(statement)?;
        }
        Ok(())
    }

    fn resolve_statement(&mut self, statement: &Stmt) -> Result<(), ResolverError> {
        self.visit_statement(statement)
    }

    fn resolve_expression(&mut self, expression: &Expr) -> Result<(), ResolverError> {
        self.visit_expr(expression)
    }

    fn resolve_local(&mut self, id: &usize, name: &String) {
        let mut depth: isize = (self.scopes.len() as isize) - 1;
        for scope in self.scopes.iter().rev() {
            if let Some(true) = scope.get(name) {
                self.interpreter
                    .resolve(*id, self.scopes.len() - 1 - (depth as usize));
                return;
            }
            depth -= 1;
        }
    }

    fn resolve_function(
        &mut self,
        params: &Vec<String>,
        body: &Vec<Stmt>,
        function_type: FunctionType,
    ) -> Result<(), ResolverError> {
        let enclosing_function_type = self.current_function_type;
        self.current_function_type = function_type;
        self.begin_scope();
        for param in params {
            self.declare(param.clone())?;
            self.define(param.clone());
        }

        self.resolve_statements(body)?;
        self.end_scope();
        self.current_function_type = enclosing_function_type;
        Ok(())
    }
}

impl StmtVisitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_block_statement(
        &mut self,
        statements: &Vec<Stmt>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.begin_scope();
        self.resolve_statements(statements)?;
        self.end_scope();
        Ok(())
    }

    fn visit_var_statement(
        &mut self,
        name: &String,
        initializer: &Option<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.declare(name.clone())?;
        initializer.as_ref().map(|i| self.resolve_expression(i));
        self.define(name.clone());
        Ok(())
    }

    fn visit_function_statement(
        &mut self,
        name: &String,
        params: &Vec<String>,
        body: &Vec<Stmt>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.declare(name.clone())?;
        self.define(name.clone());
        self.resolve_function(params, body, FunctionType::Function)
    }

    fn visit_expr_statement(&mut self, expr: &Expr, _: &span::Span) -> Result<(), ResolverError> {
        self.resolve_expression(expr)
    }

    fn visit_if_statement(
        &mut self,
        condition: &Expr,
        then: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(condition)?;
        self.resolve_statement(then)?;
        match else_branch.as_ref() {
            Some(branch) => {
                self.resolve_statement(branch)?;
            }
            _ => (),
        };
        Ok(())
    }

    fn visit_print_statement(&mut self, expr: &Expr, _: &span::Span) -> Result<(), ResolverError> {
        self.resolve_expression(expr)
    }

    fn visit_return_statement(
        &mut self,
        _: &crate::scanner::token::Token,
        value: &Option<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        if self.current_function_type == FunctionType::None {
            Err(ResolverError::new("Can't return from top-level code."))?;
        }
        match value.as_ref() {
            Some(expr) => {
                self.resolve_expression(expr)?;
            }
            _ => (),
        };
        Ok(())
    }

    fn visit_while_statement(
        &mut self,
        condition: &Expr,
        body: &Box<Stmt>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(condition)?;
        self.resolve_statement(body)
    }
}

impl ExprVisitor<Result<(), ResolverError>> for Resolver<'_> {
    fn visit_variable(
        &mut self,
        id: &usize,
        name: &String,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.scopes.last().map(|scope| {
            scope.get(name.as_str()).map(|v| {
                if !v {
                    todo!("refactor and return newly defined resolver error: Can't read local variable in its own initializer");
                }
            });
        });

        self.resolve_local(id, name);
        Ok(())
    }

    fn visit_assignment(
        &mut self,
        id: &usize,
        name: &String,
        value: &Box<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(value)?;
        self.resolve_local(id, name);
        Ok(())
    }

    fn visit_binary(
        &mut self,
        _: &crate::ast::BinOp,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(lhs)?;
        self.resolve_expression(rhs)
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        args: &Vec<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(callee)?;
        for arg in args {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>, _: &span::Span) -> Result<(), ResolverError> {
        self.resolve_expression(expr)
    }

    fn visit_literal(
        &mut self,
        _: &crate::ast::LitVal,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        Ok(())
    }

    fn visit_logic_and(
        &mut self,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(lhs)?;
        self.resolve_expression(rhs)
    }

    fn visit_logic_or(
        &mut self,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(lhs)?;
        self.resolve_expression(rhs)
    }

    fn visit_unary(
        &mut self,
        _: &crate::ast::UnOp,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) -> Result<(), ResolverError> {
        self.resolve_expression(rhs)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum FunctionType {
    None,
    Function,
}

#[derive(Debug)]
pub struct ResolverError {
    msg: &'static str,
}

impl ResolverError {
    fn new(msg: &'static str) -> Self {
        Self { msg }
    }
}

impl std::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ResolverError: {}", self.msg)
    }
}
