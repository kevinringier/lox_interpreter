use crate::{
    ast::{self, ExprVisitor, StmtVisitor},
    span,
};

#[derive(Debug)]
pub enum Value {
    // Object do we need an object?
    Bool(bool),
    Number(f64),
    String(String),
    Nil,
}

impl Value {
    pub fn stringify(&self) -> String {
        use Value::*;
        match self {
            Bool(b) => format!("{}", b),
            Number(n) => format!("{}", n).trim_end_matches(".0").to_string(),
            String(s) => s.clone(),
            Nil => format!("nil"),
        }
    }

    fn is_truthy(&self) -> bool {
        use Value::*;
        match self {
            Nil => false,
            Bool(b) => *b,
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Bool(self_b), Bool(other_b)) => self_b == other_b,
            (Number(self_n), Number(other_n)) => {
                if self_n.is_nan() && other_n.is_nan() {
                    true
                } else {
                    self_n == other_n
                }
            }
            (String(self_s), String(other_s)) => self_s == other_s,
            (Nil, Nil) => true,
            _ => false,
        }
    }
}

// NaN == NaN == true in our implementation and therefore reflexive
impl Eq for Value {}

#[derive(Debug)]
pub struct RuntimeError {
    // TODO: add error reporting msg
    span: span::Span,
    msg: String,
}

impl RuntimeError {
    fn new(span: span::Span, msg: String) -> Self {
        Self { span, msg }
    }
}

pub struct Interpreter;

type InterpreterResult = Result<Value, RuntimeError>;

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(&self, stmts: &Vec<ast::Stmt>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute(&self, stmt: &ast::Stmt) -> Result<(), RuntimeError> {
        self.visit_stmt(stmt)
    }

    fn evaluate(&self, expr: &ast::Expr) -> Result<Value, RuntimeError> {
        self.visit_expr(expr)
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_expr_stmt(&self, expr: &ast::Expr, span: &span::Span) -> Result<(), RuntimeError> {
        // TODO: should I wrap runtime error with span info?
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_print_stmt(&self, expr: &ast::Expr, span: &span::Span) -> Result<(), RuntimeError> {
        // TODO: should I wrap runtime error with span info?
        let value = self.evaluate(expr)?;
        println!("{}", value.stringify());
        Ok(())
    }
}

impl ExprVisitor<Result<Value, RuntimeError>> for Interpreter {
    fn visit_grouping(
        &self,
        expr: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Value, RuntimeError> {
        self.visit_expr(expr)
    }

    fn visit_binary(
        &self,
        bin_op: &ast::BinOp,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Value, RuntimeError> {
        let (lhs_val, rhs_val) = match (self.visit_expr(lhs), self.visit_expr(rhs)) {
            (Err(lhs_e), Err(rhs_e)) => Err(RuntimeError::new(
                span.clone(),
                // TODO:
                format!("bin_op left and right operand error, left error: , right error: "),
            )),
            (Err(lhs_e), _) => Err(RuntimeError::new(
                span.clone(),
                format!("bin_op left operand error"),
            )),
            (_, Err(rhs_e)) => Err(RuntimeError::new(
                span.clone(),
                format!("bin_op right operand error"),
            )),
            (Ok(lhs_val), Ok(rhs_val)) => Ok((lhs_val, rhs_val)),
        }?;

        use Value::*;
        use ast::BinOp::*;
        match (bin_op, lhs_val, rhs_val) {
            (Sub, Number(l), Number(r)) => Ok(Number(l - r)),
            (Div, Number(l), Number(r)) => Ok(Number(l / r)),
            (Mul, Number(l), Number(r)) => Ok(Number(l * r)),
            (Add, Number(l), Number(r)) => Ok(Number(l + r)),
            (Add, String(l), String(r)) => Ok(String(format!("{}{}", l, r))),
            (Gt, Number(l), Number(r)) => Ok(Bool(l > r)),
            (Ge, Number(l), Number(r)) => Ok(Bool(l >= r)),
            (Lt, Number(l), Number(r)) => Ok(Bool(l < r)),
            (Le, Number(l), Number(r)) => Ok(Bool(l <= r)),
            (Eq, l, r) => Ok(Bool(l == r)),
            (Ne, l, r) => Ok(Bool(l != r)),
            (Sub | Div | Mul | Gt | Ge | Lt | Le, l, r) => {
                // TODO: add types of lhs_val and rhs_val in error msg
                Err(RuntimeError::new(
                    span.clone(),
                    format!(
                        "operands must be numeric, left operand: {:?}, right operand: {:?}",
                        l, r
                    ),
                ))
            }
            // operands must strings or numeric
            (Add, l, r) => Err(RuntimeError::new(
                // TODO: add types of lhs_val and rhs_val in error msg
                span.clone(),
                format!(
                    "operands must be numeric or strings, left operand: {:?}, right operand: {:?}",
                    l, r
                ),
            )),
            // TODO: why unreachable
            (op, l, r) => Err(RuntimeError::new(
                span.clone(),
                format!(
                    "runtime error with op: {:?}, left operand: {:?}, right operand: {:?}",
                    op, l, r
                ),
            )),
        }
    }

    fn visit_literal(&self, l: &ast::LitVal, span: &span::Span) -> Result<Value, RuntimeError> {
        use Value::*;
        use ast::LitVal;
        Ok(match l {
            LitVal::Number(n) => Number(*n),
            LitVal::String(s) => String(s.clone()),
            LitVal::True => Bool(true),
            LitVal::False => Bool(false),
            LitVal::Nil => Nil,
        })
    }

    fn visit_unary(
        &self,
        un_op: &ast::UnOp,
        rhs: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Value, RuntimeError> {
        use Value::*;
        use ast::UnOp;
        match (un_op, self.visit_expr(rhs)?) {
            (UnOp::Neg, Number(n)) => Ok(Number(-n)),
            (UnOp::Not, val) => Ok(Bool(!val.is_truthy())),
            // TODO: descriptive error msg
            _ => Err(RuntimeError::new(
                // TODO: add op and operand in error message
                span.clone(),
                format!("failed unary operation with operand"),
            )),
        }
    }
}
