use std::collections::HashMap;

use crate::{
    ast::{Expr, ExprVisitor, Stmt, StmtVisitor},
    interpreter::Interpreter,
    span,
};

pub struct Resolver<'i> {
    scopes: Vec<HashMap<String, bool>>,
    interpreter: &'i mut Interpreter,
}

impl<'i> Resolver<'i> {
    pub fn new(interpreter: &'i mut Interpreter) -> Self {
        let scopes = vec![];
        Self {
            scopes,
            interpreter,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: String) {
        self.scopes.last_mut().map(|s| s.insert(name, false));
    }

    fn define(&mut self, name: String) {
        self.scopes.last_mut().map(|s| s.insert(name, true));
    }

    pub fn resolve_statements(&mut self, statements: &Vec<Stmt>) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement(&mut self, statement: &Stmt) {
        self.visit_statement(statement);
    }

    fn resolve_expression(&mut self, expression: &Expr) {
        self.visit_expr(expression);
    }

    fn resolve_local(&mut self, id: &usize, name: &String) {
        let mut depth: isize = (self.scopes.len() as isize) - 1;
        for scope in self.scopes.iter() {
            if let Some(true) = scope.get(name) {
                self.interpreter
                    .resolve(*id, self.scopes.len() - 1 - (depth as usize));
                return;
            }
            depth -= 1;
        }
    }

    fn resolve_function(&mut self, params: &Vec<String>, body: &Vec<Stmt>) {
        self.begin_scope();
        for param in params {
            self.declare(param.clone());
            self.define(param.clone());
        }

        self.resolve_statements(body);
        self.end_scope();
    }
}

impl StmtVisitor<()> for Resolver<'_> {
    fn visit_block_statement(&mut self, statements: &Vec<Stmt>, _: &span::Span) {
        self.begin_scope();
        self.resolve_statements(statements);
        self.end_scope();
    }

    fn visit_var_statement(&mut self, name: &String, initializer: &Option<Expr>, _: &span::Span) {
        self.declare(name.clone());
        initializer.as_ref().map(|i| self.resolve_expression(i));
        self.define(name.clone());
    }

    fn visit_function_statement(
        &mut self,
        name: &String,
        params: &Vec<String>,
        body: &Vec<Stmt>,
        _: &span::Span,
    ) {
        self.declare(name.clone());
        self.define(name.clone());
        self.resolve_function(params, body);
    }

    fn visit_expr_statement(&mut self, expr: &Expr, _: &span::Span) {
        self.resolve_expression(expr);
    }

    fn visit_if_statement(
        &mut self,
        condition: &Expr,
        then: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
        _: &span::Span,
    ) {
        self.resolve_expression(condition);
        self.resolve_statement(then);
        else_branch.as_ref().map(|e_b| self.resolve_statement(e_b));
    }

    fn visit_print_statement(&mut self, expr: &Expr, _: &span::Span) {
        self.resolve_expression(expr);
    }

    fn visit_return_statement(
        &mut self,
        _: &crate::scanner::token::Token,
        value: &Option<Expr>,
        _: &span::Span,
    ) {
        value.as_ref().map(|v| self.resolve_expression(v));
    }

    fn visit_while_statement(&mut self, condition: &Expr, body: &Box<Stmt>, _: &span::Span) {
        self.resolve_expression(condition);
        self.resolve_statement(body);
    }
}

impl ExprVisitor<()> for Resolver<'_> {
    fn visit_variable(&mut self, id: &usize, name: &String, _: &span::Span) -> () {
        self.scopes.last().map(|scope| {
            scope.get(name.as_str()).map(|v| {
                if !v {
                    todo!("refactor and return newly defined resolver error: Can't read local variable in its own initializer");
                }
            });
        });

        self.resolve_local(id, name);
    }

    fn visit_assignment(&mut self, id: &usize, name: &String, value: &Box<Expr>, _: &span::Span) {
        self.resolve_expression(value);
        self.resolve_local(id, name);
    }

    fn visit_binary(
        &mut self,
        _: &crate::ast::BinOp,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) {
        self.resolve_expression(lhs);
        self.resolve_expression(rhs);
    }

    fn visit_call(&mut self, callee: &Expr, args: &Vec<Expr>, _: &span::Span) {
        self.resolve_expression(callee);
        for arg in args {
            self.resolve_expression(arg);
        }
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>, _: &span::Span) {
        self.resolve_expression(expr);
    }

    fn visit_literal(&mut self, _: &crate::ast::LitVal, _: &span::Span) {}

    fn visit_logic_and(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, _: &span::Span) {
        self.resolve_expression(lhs);
        self.resolve_expression(rhs);
    }

    fn visit_logic_or(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, _: &span::Span) {
        self.resolve_expression(lhs);
        self.resolve_expression(rhs);
    }

    fn visit_unary(&mut self, _: &crate::ast::UnOp, rhs: &Box<Expr>, _: &span::Span) -> () {
        self.resolve_expression(rhs);
    }
}
