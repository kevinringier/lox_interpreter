use crate::{scanner::token::Token, span};

#[derive(Clone, Debug)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
        span: span::Span,
    },
    Class {
        name: String,
        methods: Vec<Stmt>,
        span: span::Span,
    },
    ExprStmt {
        inner: Expr,
        span: span::Span,
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
        span: span::Span,
    },
    If {
        condition: Expr,
        then: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: span::Span,
    },
    Print {
        rhs: Expr,
        span: span::Span,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
        span: span::Span,
    },
    Var {
        name: String,
        initializer: Option<Expr>,
        span: span::Span,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
        span: span::Span,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Assign {
        id: usize,
        name: String,
        value: Box<Expr>,
        span: span::Span,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: span::Span,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: span::Span,
    },
    Get {
        object: Box<Expr>,
        name: String,
        span: span::Span,
    },
    Grouping {
        expr: Box<Expr>,
        span: span::Span,
    },
    Literal {
        inner: LitVal,
        span: span::Span,
    },
    LogicAnd {
        left: Box<Expr>,
        right: Box<Expr>,
        span: span::Span,
    },
    LogicOr {
        left: Box<Expr>,
        right: Box<Expr>,
        span: span::Span,
    },
    Set {
        object: Box<Expr>,
        name: String,
        value: Box<Expr>,
        span: span::Span,
    },
    This {
        id: usize,
        keyword: String,
        span: span::Span,
    },
    Unary {
        op: UnOp,
        rhs: Box<Expr>,
        span: span::Span,
    },
    Variable {
        id: usize,
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
    fn visit_statement(&mut self, s: &Stmt) -> T {
        use Stmt::*;
        match s {
            Block { statements, span } => self.visit_block_statement(statements, span),
            Class {
                name,
                methods,
                span,
            } => self.visit_class_statement(name, methods, span),
            ExprStmt { inner, span } => self.visit_expr_statement(inner, span),
            Function {
                name,
                params,
                body,
                span,
            } => self.visit_function_statement(name, params, body, span),
            If {
                condition,
                then,
                else_branch,
                span,
            } => self.visit_if_statement(condition, then, else_branch, span),
            Print { rhs, span } => self.visit_print_statement(rhs, span),
            Return {
                keyword,
                value,
                span,
            } => self.visit_return_statement(keyword, value, span),
            Var {
                name,
                initializer,
                span,
            } => self.visit_var_statement(name, initializer, span),
            While {
                condition,
                body,
                span,
            } => self.visit_while_statement(condition, body, span),
        }
    }

    fn visit_block_statement(&mut self, statements: &Vec<Stmt>, span: &span::Span) -> T;

    fn visit_class_statement(&mut self, name: &String, methods: &Vec<Stmt>, span: &span::Span)
    -> T;

    fn visit_expr_statement(&mut self, expr: &Expr, span: &span::Span) -> T;

    fn visit_function_statement(
        &mut self,
        name: &String,
        params: &Vec<String>,
        body: &Vec<Stmt>,
        span: &span::Span,
    ) -> T;

    fn visit_if_statement(
        &mut self,
        condition: &Expr,
        then: &Box<Stmt>,
        else_branch: &Option<Box<Stmt>>,
        span: &span::Span,
    ) -> T;

    fn visit_print_statement(&mut self, expr: &Expr, span: &span::Span) -> T;

    fn visit_return_statement(
        &mut self,
        keyword: &Token,
        value: &Option<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_var_statement(
        &mut self,
        name: &String,
        initializer: &Option<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_while_statement(&mut self, condition: &Expr, body: &Box<Stmt>, span: &span::Span)
    -> T;
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T {
        use Expr::*;
        match e {
            Assign {
                id,
                name,
                value,
                span,
            } => self.visit_assignment(id, name, value, span),
            Binary { op, lhs, rhs, span } => self.visit_binary(op, lhs, rhs, span),
            Call { callee, args, span } => self.visit_call(callee, args, span),
            Get { object, name, span } => self.visit_get(object, name, span),
            Grouping { expr, span } => self.visit_grouping(expr, span),
            Literal { inner, span } => self.visit_literal(inner, span),
            LogicAnd { left, right, span } => self.visit_logic_and(left, right, span),
            LogicOr { left, right, span } => self.visit_logic_or(left, right, span),
            Set {
                object,
                name,
                value,
                span,
            } => self.visit_set(object, name, value, span),
            This { id, keyword, span } => self.visit_this(id, keyword, span),
            Unary { op, rhs, span } => self.visit_unary(op, rhs, span),
            Variable { id, name, span } => self.visit_variable(id, name, span),
        }
    }

    fn visit_assignment(
        &mut self,
        id: &usize,
        name: &String,
        value: &Box<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_binary(
        &mut self,
        bin_op: &BinOp,
        lhs: &Box<Expr>,
        rhs: &Box<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_call(&mut self, callee: &Expr, args: &Vec<Expr>, span: &span::Span) -> T;

    fn visit_get(&mut self, object: &Box<Expr>, name: &String, span: &span::Span) -> T;

    fn visit_grouping(&mut self, expr: &Box<Expr>, span: &span::Span) -> T;

    fn visit_literal(&mut self, l: &LitVal, span: &span::Span) -> T;

    fn visit_logic_and(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, span: &span::Span) -> T;

    fn visit_logic_or(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, span: &span::Span) -> T;

    fn visit_set(
        &mut self,
        object: &Box<Expr>,
        name: &String,
        value: &Box<Expr>,
        span: &span::Span,
    ) -> T;

    fn visit_this(&mut self, id: &usize, keyword: &String, span: &span::Span) -> T;

    fn visit_unary(&mut self, un_op: &UnOp, rhs: &Box<Expr>, span: &span::Span) -> T;

    fn visit_variable(&mut self, id: &usize, name: &String, span: &span::Span) -> T;
}

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn print(&mut self, stmts: &Vec<Stmt>) {
        for stmt in stmts {
            println!("{}", self.visit_statement(stmt));
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
    fn visit_block_statement(&mut self, statements: &Vec<Stmt>, _: &span::Span) -> String {
        let mut result = String::new();

        result.push_str("(block");

        for stmt in statements {
            let e: &str = &(self.visit_statement(stmt));

            result.push_str(&format!(" {}", e));
        }

        result.push_str(" )");

        result
    }

    fn visit_class_statement(
        &mut self,
        name: &String,
        _methods: &Vec<Stmt>,
        _: &span::Span,
    ) -> String {
        self.parenthesize(format!("<class> {}", name).as_str(), vec![])
    }

    fn visit_expr_statement(&mut self, expr: &Expr, _: &span::Span) -> String {
        self.parenthesize("expression statement", vec![expr])
    }

    fn visit_function_statement(
        &mut self,
        name: &String,
        _params: &Vec<String>,
        _: &Vec<Stmt>,
        _: &span::Span,
    ) -> String {
        self.parenthesize(format!("<fn> {}", name).as_str(), vec![])
    }

    fn visit_if_statement(
        &mut self,
        _condition: &Expr,
        _then: &Box<Stmt>,
        _else_branch: &Option<Box<Stmt>>,
        _: &span::Span,
    ) -> String {
        //self.parenthesize("if ", exprs)
        String::from("if") // TODO:
    }

    fn visit_print_statement(&mut self, expr: &Expr, _: &span::Span) -> String {
        self.parenthesize("print", vec![expr])
    }

    fn visit_return_statement(
        &mut self,
        _: &Token,
        value: &Option<Expr>,
        _: &span::Span,
    ) -> String {
        let expr = value.as_ref().map(|e| vec![e]).unwrap_or(vec![]);
        self.parenthesize("return", expr)
    }

    fn visit_var_statement(
        &mut self,
        name: &String,
        initializer: &Option<Expr>,
        _: &span::Span,
    ) -> String {
        let exprs = match initializer {
            Some(expr) => vec![expr],
            None => vec![],
        };

        self.parenthesize(format!("var statement: {}", name).as_str(), exprs)
    }

    fn visit_while_statement(
        &mut self,
        _condition: &Expr,
        _body: &Box<Stmt>,
        _span: &span::Span,
    ) -> String {
        "while".to_string()
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_assignment(
        &mut self,
        _: &usize,
        name: &String,
        value: &Box<Expr>,
        _: &span::Span,
    ) -> String {
        self.parenthesize(format!("assignment: {}", name).as_str(), vec![value])
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

    fn visit_call(&mut self, callee: &Expr, args: &Vec<Expr>, _: &span::Span) -> String {
        let callee = self.visit_expr(callee);
        self.parenthesize(
            format!("callee: {}", callee).as_str(),
            args.iter().collect(),
        )
    }

    fn visit_get(&mut self, object: &Box<Expr>, name: &String, _: &span::Span) -> String {
        self.parenthesize(format!("get property: {}", name).as_str(), vec![object])
    }

    fn visit_grouping(&mut self, expr: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize("group", vec![expr])
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

    fn visit_logic_and(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize("and", vec![lhs, rhs])
    }

    fn visit_logic_or(&mut self, lhs: &Box<Expr>, rhs: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize("or", vec![lhs, rhs])
    }

    fn visit_set(
        &mut self,
        _: &Box<Expr>,
        name: &String,
        value: &Box<Expr>,
        _: &span::Span,
    ) -> String {
        self.parenthesize(format!("set {}", name).as_str(), vec![value])
    }

    fn visit_this(&mut self, _: &usize, keyword: &String, _: &span::Span) -> String {
        self.parenthesize(format!("this {}", keyword).as_str(), vec![])
    }

    fn visit_unary(&mut self, un_op: &UnOp, rhs: &Box<Expr>, _: &span::Span) -> String {
        self.parenthesize(un_op.to_string(), vec![rhs])
    }

    fn visit_variable(&mut self, _: &usize, name: &String, _: &span::Span) -> String {
        self.parenthesize(format!("evaluate var {}", name).as_str(), vec![])
    }
}
