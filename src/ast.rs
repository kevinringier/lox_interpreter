// Literals - Numbers, strings, Booleans, nil.
// Unary expressions - A prefix ! to perform a logical not, and - to negate a number.
// Binary expressions - The infix arithmetic (+, -, *, /) and logic (==, !=, <, <=, >, >=)
// operators.
// Parenthesis - A pair of ( and ) wrapped around an expression.

use crate::span;

// syntactic grammar
// program      -> statement* EOF ;
// declaration  -> varDecl | statement ;
// varDecl      -> "var" IDENTIFIER ( "=" expression )? ";" ;
// statement    -> exprStmt | printStmt ;
// exprStmt     -> expression ";" ;
// printStmt    -> "print" expression ";" ;
// expression   -> literal | unary | binary | grouping ;
// literal      -> NUMBER | STRING | "true" | "false" | "nil" ;
// grouping     -> "(" expression ")" ;
// unary        -> ( "-" | "!" ) expression ;
// binary       -> expression operator expression
// operator     -> "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" ;

#[derive(Clone, Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
        span: span::Span,
    },
    ExprStmt {
        inner: Expr,
        span: span::Span,
    },
    Print {
        rhs: Expr,
        span: span::Span,
    },
    Var {
        name: String,
        initializer: Option<Expr>,
        span: span::Span,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Assign {
        name: String,
        value: Box<Expr>,
        span: span::Span,
    },
    Literal {
        inner: LitVal,
        span: span::Span,
    },
    Unary {
        op: UnOp,
        rhs: Box<Expr>,
        span: span::Span,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: span::Span,
    },
    Grouping {
        expr: Box<Expr>,
        span: span::Span,
    },
    Variable {
        name: String,
        span: span::Span,
    },
}

#[derive(Clone, Debug)]
pub enum LitVal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Neg,
    Not,
}

impl UnOp {
    fn to_string(&self) -> &str {
        use UnOp::*;
        match self {
            Neg => "negate",
            Not => "not",
        }
    }
}

#[derive(Clone, Debug)]
pub enum BinOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    fn to_string(&self) -> &str {
        use BinOp::*;
        match self {
            Eq => "eq",
            Ne => "ne",
            Lt => "lt",
            Le => "le",
            Gt => "gt",
            Ge => "ge",
            Add => "add",
            Sub => "sub",
            Mul => "mul",
            Div => "div",
        }
    }
}

pub trait StmtVisitor<T> {
    fn visit_stmt(&mut self, s: &Stmt) -> T {
        use Stmt::*;
        match s {
            Block { statements, span } => self.visit_block_stmt(statements, span),
            ExprStmt { inner, span } => self.visit_expr_stmt(inner, span),
            Print { rhs, span } => self.visit_print_stmt(rhs, span),
            Var {
                name,
                initializer,
                span,
            } => self.visit_var_stmt(name, initializer, span),
        }
    }

    fn visit_block_stmt(&mut self, statements: &Vec<Stmt>, span: &span::Span) -> T;

    fn visit_expr_stmt(&mut self, expr: &Expr, span: &span::Span) -> T;

    fn visit_print_stmt(&mut self, expr: &Expr, span: &span::Span) -> T;

    fn visit_var_stmt(&mut self, name: &String, initializer: &Option<Expr>, span: &span::Span)
    -> T;
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T {
        use Expr::*;
        match e {
            Assign { name, value, span } => self.visit_assignment(name, value, span),
            Literal { inner, span } => self.visit_literal(inner, span),
            Unary { op, rhs, span } => self.visit_unary(op, rhs, span),
            Binary { op, lhs, rhs, span } => self.visit_binary(op, lhs, rhs, span),
            Grouping { expr, span } => self.visit_grouping(expr, span),
            Variable { name, span } => self.visit_variable(name, span),
        }
    }

    fn visit_assignment(&mut self, name: &String, value: &Box<Expr>, span: &span::Span) -> T;

    fn visit_grouping(&mut self, expr: &Box<Expr>, span: &span::Span) -> T;

    fn visit_binary(
        &mut self,
        bin_op: &BinOp,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_literal(&mut self, l: &LitVal, span: &span::Span) -> T;

    fn visit_unary(&mut self, un_op: &UnOp, rhs: &Box<Expr>, span: &span::Span) -> T;

    // TODO: if we want to parenthesize the evaluated variable, we need to add a type constraint
    // for an environment interface
    fn visit_variable(&mut self, name: &String, span: &span::Span) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn print(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            println!("{}", self.visit_stmt(stmt));
        }
    }

    fn parenthesize(&mut self, name: &str, exprs: Vec<&Expr>) -> String {
        let mut result = String::new();

        result.push_str(&format!("( {}", name));

        for expr in exprs {
            let e: &str = &(self.visit_expr(expr));

            result.push_str(&format!(" {}", e));
        }

        result.push_str(" )");

        result
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_block_stmt(&mut self, statements: &Vec<Stmt>, _: &span::Span) -> String {
        let mut result = String::new();

        result.push_str("(block");

        for stmt in statements {
            let e: &str = &(self.visit_stmt(stmt));

            result.push_str(&format!(" {}", e));
        }

        result.push_str(" )");

        result
    }

    fn visit_expr_stmt(&mut self, expr: &Expr, _: &span::Span) -> String {
        self.parenthesize("expression statement", vec![expr])
    }

    fn visit_print_stmt(&mut self, expr: &Expr, _: &span::Span) -> String {
        self.parenthesize("print", vec![expr])
    }

    fn visit_var_stmt(
        &mut self,
        name: &String,
        initializer: &Option<Expr>,
        _: &span::Span,
    ) -> String {
        let exprs = match initializer {
            Some(expr) => vec![expr],
            None => vec![],
        };

        // TODO: do we want to print evaluated value?
        self.parenthesize(format!("var statement: {}", name).as_str(), exprs)
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_assignment(&mut self, name: &String, value: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize(format!("assignment: {}", name).as_str(), vec![value])
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize("group", vec![expr])
    }

    fn visit_binary(
        &mut self,
        bin_op: &BinOp,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        _: &span::Span,
    ) -> String {
        self.parenthesize(bin_op.to_string(), vec![lhs, rhs])
    }

    fn visit_literal(&mut self, l: &LitVal, _: &span::Span) -> String {
        match l {
            LitVal::Number(n) => format!("{}", n),
            LitVal::String(s) => format!("{}", s),
            LitVal::True => format!("true"),
            LitVal::False => format!("false"),
            LitVal::Nil => format!("nil"),
        }
    }

    fn visit_unary(&mut self, un_op: &UnOp, rhs: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize(un_op.to_string(), vec![rhs])
    }

    fn visit_variable(&mut self, name: &String, _: &span::Span) -> String {
        self.parenthesize(format!("evaluate var {}", name).as_str(), vec![])
    }
}
