use crate::{
    ast::{self, ExprVisitor, StmtVisitor},
    environment::{self, Environment},
    span,
};

#[derive(Clone, Debug)]
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.stringify())
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
    span: span::Span,
    msg: String,
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RuntimeError: {}, span_info: {}", self.msg, self.span)
    }
}

impl RuntimeError {
    fn new(span: span::Span, msg: String) -> Self {
        Self { span, msg }
    }
}

pub struct Interpreter {
    env: environment::Environment<Value>,
}

impl<'a> Interpreter {
    pub fn new() -> Self {
        Self {
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &Vec<ast::Stmt>) -> Result<(), RuntimeError> {
        for stmt in statements {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &ast::Stmt) -> Result<(), RuntimeError> {
        self.visit_statement(statement)
    }

    fn execute_block(&mut self, statements: &Vec<ast::Stmt>) -> Result<(), RuntimeError> {
        let prev_env = std::mem::replace(&mut self.env, Environment::new());
        self.env.set_enclosing_env(prev_env);

        for stmt in statements {
            self.execute(stmt)?;
        }

        // TODO: if we encounter error in executing statements, we will preempt
        // resetting the env to the parent. We should just exit.
        self.env = self.env.get_enclosing_env();

        Ok(())
    }

    fn evaluate(&mut self, expr: &ast::Expr) -> Result<Value, RuntimeError> {
        self.visit_expr(expr)
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_block_statement(
        &mut self,
        statements: &Vec<ast::Stmt>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        self.execute_block(statements)
    }

    fn visit_expr_statement(
        &mut self,
        expr: &ast::Expr,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        condition: &ast::Expr,
        then: &Box<ast::Stmt>,
        else_branch: &Option<Box<ast::Stmt>>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        if self.evaluate(condition)?.is_truthy() {
            self.execute(then)?;
        } else if let Some(else_branch) = else_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_print_statement(
        &mut self,
        expr: &ast::Expr,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        let value = self.evaluate(expr)?;
        println!("{}", value.stringify());
        Ok(())
    }

    fn visit_var_statement(
        &mut self,
        name: &String,
        initializer: &Option<ast::Expr>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        let init_val = match initializer {
            Some(e) => self.evaluate(e)?,
            None => Value::Nil,
        };

        Ok(self.env.define(name.clone(), init_val))
    }

    fn visit_while_statement(
        &mut self,
        condition: &ast::Expr,
        body: &Box<ast::Stmt>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        while self.evaluate(condition)?.is_truthy() {
            self.execute(body)?;
        }

        Ok(())
    }
}

impl ExprVisitor<Result<Value, RuntimeError>> for Interpreter {
    fn visit_assignment(
        &mut self,
        name: &String,
        value_expr: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Value, RuntimeError> {
        let r_value = self.evaluate(value_expr)?;

        match self.env.assign(name.clone(), r_value.clone()) {
            Ok(()) => Ok(r_value),
            Err(_) => Err(RuntimeError::new(
                span.clone(),
                format!("Undefined variable '{}'.", name),
            )),
        }
    }

    fn visit_binary(
        &mut self,
        bin_op: &ast::BinOp,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Value, RuntimeError> {
        let (lhs_val, rhs_val) = match (self.visit_expr(lhs), self.visit_expr(rhs)) {
            (Err(lhs_e), Err(rhs_e)) => Err(RuntimeError::new(
                span.clone(),
                format!(
                    "bin_op left and right operand error, left error: {}, right error: {}",
                    lhs_e, rhs_e
                ),
            )),
            (Err(lhs_e), _) => Err(RuntimeError::new(
                span.clone(),
                format!("bin_op left operand error: {}", lhs_e),
            )),
            (_, Err(rhs_e)) => Err(RuntimeError::new(
                span.clone(),
                format!("bin_op right operand error: {}", rhs_e),
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
                        "operands must be numeric, left operand: {}, right operand: {}",
                        l, r
                    ),
                ))
            }
            (Add, l, r) => Err(RuntimeError::new(
                span.clone(),
                format!(
                    "operands must be numeric or strings, left operand: {}, right operand: {}",
                    l, r
                ),
            )),
        }
    }

    fn visit_grouping(
        &mut self,
        expr: &Box<ast::Expr>,
        _: &span::Span,
    ) -> Result<Value, RuntimeError> {
        self.visit_expr(expr)
    }

    fn visit_literal(&mut self, l: &ast::LitVal, _: &span::Span) -> Result<Value, RuntimeError> {
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

    fn visit_logic_and(
        &mut self,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        _: &span::Span,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate(lhs)?;

        if !left.is_truthy() {
            Ok(left)
        } else {
            Ok(self.evaluate(rhs)?)
        }
    }

    fn visit_logic_or(
        &mut self,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        _: &span::Span,
    ) -> Result<Value, RuntimeError> {
        let left = self.evaluate(lhs)?;

        if left.is_truthy() {
            Ok(left)
        } else {
            Ok(self.evaluate(rhs)?)
        }
    }

    fn visit_unary(
        &mut self,
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

    fn visit_variable(&mut self, name: &String, span: &span::Span) -> Result<Value, RuntimeError> {
        match self.env.get(name) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::new(
                span.clone(),
                format!("Undefined variable '{}'.", name),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_visit_unary() {}
}
