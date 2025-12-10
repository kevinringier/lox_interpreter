use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

use itertools::izip;

use crate::{
    ast::{self, ExprVisitor, StmtVisitor},
    environment::Environment,
    span,
};

#[derive(Clone, Debug)]
pub enum Value {
    // Object do we need an object?
    Bool(bool),
    Class {
        name: String,
        // NOTE: `None` variant will be considered a default constructor.
        constructor: Option<Rc<RefCell<Value>>>,
        methods: HashMap<String, Rc<RefCell<Value>>>,
    },
    Instance {
        name: String,
        class: Box<Rc<RefCell<Value>>>,
        fields: HashMap<String, Rc<RefCell<Value>>>,
    },
    Function(FunctionType),
    Number(f64),
    String(String),
    Nil,
}

#[derive(Clone, Debug)]
pub enum FunctionType {
    BuiltIn(BuiltInFunctionType),
    UserDefined {
        f: ast::Stmt,
        closure: Rc<RefCell<Environment<Rc<RefCell<Value>>>>>,
        is_initializer: bool,
    },
}

#[derive(Clone, Debug)]
pub enum BuiltInFunctionType {
    Clock,
}

impl Value {
    fn call(
        value: Rc<RefCell<Self>>,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        use Value::*;
        match &*value.borrow() {
            Class {
                name, constructor, ..
            } => {
                let instance = Rc::new(RefCell::new(Value::Instance {
                    name: name.clone(),
                    class: Box::new(value.clone()),
                    fields: HashMap::new(),
                }));
                match constructor.as_ref() {
                    None => Ok(instance),
                    Some(f) => {
                        let binded_init = Value::bind(f.clone(), instance.clone());
                        Value::call(binded_init, interpreter, args, span)?;
                        Ok(instance)
                    }
                }
            }
            Function(f) => match f {
                FunctionType::BuiltIn(f_type) => match f_type {
                    BuiltInFunctionType::Clock => {
                        let now = SystemTime::now();
                        let duration_since_epoch =
                            now.duration_since(UNIX_EPOCH).expect("system-time error");
                        Ok(Rc::new(RefCell::new(Value::Number(
                            duration_since_epoch.as_secs_f64(),
                        ))))
                    }
                },
                FunctionType::UserDefined {
                    f,
                    closure,
                    is_initializer,
                } => match f {
                    ast::Stmt::Function { params, body, .. } => {
                        let mut env = Environment::<Rc<RefCell<Value>>>::new();
                        env.set_enclosing_env(closure.clone());
                        for (k, v) in izip!(params, args) {
                            env.define(k.clone(), v);
                        }

                        match interpreter.execute_block(&body, env) {
                            Ok(_) => Ok(if *is_initializer {
                                closure.borrow().get_at(0, &"this".to_string())
                            } else {
                                Rc::new(RefCell::new(Nil))
                            }),
                            Err(e) => match e {
                                RuntimeError::ReturnSentinel(v) => Ok(if *is_initializer {
                                    closure.borrow().get_at(0, &"this".to_string())
                                } else {
                                    v
                                }),
                                _ => Err(e)?,
                            },
                        }
                    }
                    _ => panic!("declaration should be a callable type"),
                },
            },
            _ => Err(RuntimeError::new_error(
                span.clone(),
                "Can only call functions and classes".to_string(),
            )),
        }
    }

    fn arity(&self, span: &span::Span) -> Result<usize, RuntimeError> {
        use Value::*;
        match self {
            Class { constructor, .. } => match constructor {
                Some(f) => f.borrow().arity(span),
                None => Ok(0),
            },
            Function(f_type) => match f_type {
                FunctionType::BuiltIn(f_type) => match f_type {
                    BuiltInFunctionType::Clock => Ok(0),
                },
                FunctionType::UserDefined { f, .. } => match f {
                    ast::Stmt::Function { params, .. } => Ok(params.len()),
                    _ => panic!("arity can only be invoked on callable type"),
                },
            },
            _ => Err(RuntimeError::new_error(
                span.clone(),
                "Arity only applies to functions and classes".to_string(),
            )),
        }
    }

    fn find_method(class: &Box<Rc<RefCell<Value>>>, name: &String) -> Option<Rc<RefCell<Value>>> {
        match &*class.borrow() {
            Value::Class {
                methods,
                constructor,
                ..
            } => {
                if name == "init" && constructor.is_some() {
                    Some(constructor.as_ref().unwrap().clone())
                } else {
                    methods.get(name).map(|method| method.clone())
                }
            }
            _ => panic!("Attempted to find method on a non-class type"),
        }
    }

    fn bind(method: Rc<RefCell<Value>>, instance: Rc<RefCell<Value>>) -> Rc<RefCell<Value>> {
        match &*method.borrow_mut() {
            Value::Function(f) => match f {
                FunctionType::UserDefined {
                    f,
                    closure,
                    is_initializer,
                } => {
                    let mut env = Environment::new();
                    env.define("this".to_string(), instance.clone());
                    env.set_enclosing_env(closure.clone());

                    Rc::new(RefCell::new(Value::Function(FunctionType::UserDefined {
                        f: f.clone(),
                        closure: Rc::new(RefCell::new(env)),
                        is_initializer: *is_initializer,
                    })))
                }
                _ => panic!("Trying to bind to non-method function type"),
            },
            _ => panic!("Trying to bind to non function type value"),
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

    pub fn stringify(&self) -> String {
        use Value::*;
        match self {
            Bool(b) => format!("{}", b),
            Class { name, .. } => format!("class {}", name),
            Function(f_type) => match f_type {
                FunctionType::BuiltIn(f_type) => match f_type {
                    BuiltInFunctionType::Clock => "<native fn clock>".to_string(),
                },
                FunctionType::UserDefined { f, .. } => match f {
                    ast::Stmt::Function { name, .. } => name.clone(),
                    _ => panic!("arity can only be invoked on callable type"),
                },
            },
            Instance { name, .. } => format!("<instance> {}", name),
            Number(n) => format!("{}", n).trim_end_matches(".0").to_string(),
            String(s) => s.clone(),
            Nil => format!("nil"),
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
pub enum RuntimeError {
    Error { span: span::Span, msg: String },
    ReturnSentinel(Rc<RefCell<Value>>),
}

impl std::fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error { span, msg } => {
                write!(f, "RuntimeError: {}, span_info: {}", msg, span)
            }
            _ => write!(f, "ReturnSentinel"),
        }
    }
}

impl RuntimeError {
    fn new_error(span: span::Span, msg: String) -> Self {
        Self::Error { span, msg }
    }

    fn new_return_sentinel(value: Rc<RefCell<Value>>) -> Self {
        Self::ReturnSentinel(value)
    }
}

pub struct Interpreter {
    globals: Rc<RefCell<Environment<Rc<RefCell<Value>>>>>,
    env: Rc<RefCell<Environment<Rc<RefCell<Value>>>>>,
    locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        let clock_func = Rc::new(RefCell::new(Value::Function(FunctionType::BuiltIn(
            BuiltInFunctionType::Clock,
        ))));
        globals.borrow_mut().define("clock".to_string(), clock_func);

        let env = Rc::clone(&globals);

        let locals = HashMap::new();
        Self {
            globals,
            env,
            locals,
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

    fn execute_block(
        &mut self,
        statements: &Vec<ast::Stmt>,
        enclosing_env: Environment<Rc<RefCell<Value>>>,
    ) -> Result<(), RuntimeError> {
        let prev_env = std::mem::replace(&mut self.env, Rc::new(RefCell::new(enclosing_env)));

        // NOTE: Functions and Methods have an enclosing environment on the incoming enclosing_env arguments. We don't want to
        // set it to the current environment.
        if self.env.borrow().enclosing.is_none() {
            self.env.borrow_mut().set_enclosing_env(prev_env.clone());
        }

        for stmt in statements {
            match self.execute(stmt) {
                Err(e) => match e {
                    RuntimeError::ReturnSentinel(_) => {
                        // TODO: clean this up using by acquiring a guard and cleaning up these
                        // resources when the guard goes out of scope to avoid duplication below.
                        self.env = prev_env.clone();

                        Err(e)?
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        self.env = prev_env.clone();
        Ok(())
    }

    fn evaluate(&mut self, expr: &ast::Expr) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        self.visit_expr(expr)
    }

    pub fn resolve(&mut self, id: usize, depth: usize) {
        self.locals.insert(id, depth);
    }

    fn lookup_variable(&self, name: &String, id: &usize) -> Rc<RefCell<Value>> {
        if let Some(distance) = self.locals.get(id) {
            self.env.borrow().get_at(*distance, name)
        } else {
            self.globals
                .borrow()
                .get(name)
                .expect("Expect value to exists after lookup")
        }
    }
}

impl StmtVisitor<Result<(), RuntimeError>> for Interpreter {
    fn visit_block_statement(
        &mut self,
        statements: &Vec<ast::Stmt>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        self.execute_block(statements, Environment::<Rc<RefCell<Value>>>::new())
    }

    fn visit_class_statement(
        &mut self,
        name: &String,
        methods: &Vec<ast::Stmt>,
        span: &span::Span,
    ) -> Result<(), RuntimeError> {
        self.env
            .borrow_mut()
            .define(name.clone(), Rc::new(RefCell::new(Value::Nil)));

        let mut class_methods = HashMap::new();
        let mut user_defined_constructor = None;
        for method in methods {
            match &method {
                &ast::Stmt::Function { name, .. } => {
                    let is_initializer = name == "init";
                    let m = Rc::new(RefCell::new(Value::Function(FunctionType::UserDefined {
                        f: method.clone(),
                        closure: self.env.clone(),
                        is_initializer,
                    })));

                    if is_initializer {
                        if user_defined_constructor.is_some() {
                            Err(RuntimeError::new_error(
                                span.clone(),
                                "Only one constructor can be defined per class".to_string(),
                            ))?
                        }
                        user_defined_constructor = Some(m);
                    } else {
                        class_methods.insert(name.clone(), m);
                    }
                }
                _ => panic!("Found a non-function type where a method was expected"),
            }
        }

        let class = Value::Class {
            name: name.clone(),
            constructor: user_defined_constructor,
            methods: class_methods,
        };

        self.env
            .borrow_mut()
            .assign(name.clone(), Rc::new(RefCell::new(class)))
            .expect("We define the name above, therefore the key should exist");

        Ok(())
    }

    fn visit_expr_statement(
        &mut self,
        expr: &ast::Expr,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        self.evaluate(expr)?;
        Ok(())
    }

    fn visit_function_statement(
        &mut self,
        name: &String,
        params: &Vec<String>,
        body: &Vec<ast::Stmt>,
        span: &span::Span,
    ) -> Result<(), RuntimeError> {
        let f = ast::Stmt::Function {
            name: name.clone(),
            params: params.clone(),
            body: body.clone(),
            span: span.clone(),
        };

        let closure = self.env.clone();
        let is_initializer = false;

        let user_defined_func = Rc::new(RefCell::new(Value::Function(FunctionType::UserDefined {
            f,
            closure,
            is_initializer,
        })));

        self.env
            .borrow_mut()
            .define(name.clone(), user_defined_func);

        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        condition: &ast::Expr,
        then: &Box<ast::Stmt>,
        else_branch: &Option<Box<ast::Stmt>>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        if self.evaluate(condition)?.borrow().is_truthy() {
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
        println!("{}", value.borrow().stringify());
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        _: &crate::scanner::token::Token,
        value: &Option<ast::Expr>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        let value = match value {
            Some(e) => self.evaluate(e)?,
            _ => Rc::new(RefCell::new(Value::Nil)),
        };

        Err(RuntimeError::new_return_sentinel(value))?
    }

    fn visit_var_statement(
        &mut self,
        name: &String,
        initializer: &Option<ast::Expr>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        let init_val = match initializer {
            Some(e) => self.evaluate(e)?,
            None => Rc::new(RefCell::new(Value::Nil)),
        };

        Ok(self.env.borrow_mut().define(name.clone(), init_val))
    }

    fn visit_while_statement(
        &mut self,
        condition: &ast::Expr,
        body: &Box<ast::Stmt>,
        _: &span::Span,
    ) -> Result<(), RuntimeError> {
        while self.evaluate(condition)?.borrow().is_truthy() {
            self.execute(body)?;
        }

        Ok(())
    }
}

impl ExprVisitor<Result<Rc<RefCell<Value>>, RuntimeError>> for Interpreter {
    fn visit_assignment(
        &mut self,
        id: &usize,
        name: &String,
        value_expr: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let r_value = self.evaluate(value_expr)?;

        if let Some(distance) = self.locals.get(id) {
            self.env
                .borrow_mut()
                .assign_at(*distance, name, r_value.clone());

            Ok(r_value)
        } else {
            match self
                .globals
                .borrow_mut()
                .assign(name.clone(), r_value.clone())
            {
                Ok(_) => Ok(r_value),
                Err(_) => Err(RuntimeError::new_error(
                    span.clone(),
                    format!("Undefined variable {}.", name),
                )),
            }
        }
    }

    fn visit_binary(
        &mut self,
        bin_op: &ast::BinOp,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let (lhs_val, rhs_val) = match (self.visit_expr(lhs), self.visit_expr(rhs)) {
            (Err(lhs_e), Err(rhs_e)) => Err(RuntimeError::new_error(
                span.clone(),
                format!(
                    "bin_op left and right operand error, left error: {}, right error: {}",
                    lhs_e, rhs_e
                ),
            )),
            (Err(lhs_e), _) => Err(RuntimeError::new_error(
                span.clone(),
                format!("bin_op left operand error: {}", lhs_e),
            )),
            (_, Err(rhs_e)) => Err(RuntimeError::new_error(
                span.clone(),
                format!("bin_op right operand error: {}", rhs_e),
            )),
            (Ok(lhs_val), Ok(rhs_val)) => Ok((lhs_val, rhs_val)),
        }?;

        use Value::*;
        use ast::BinOp::*;
        match (bin_op, &*lhs_val.borrow(), &*rhs_val.borrow()) {
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
                Err(RuntimeError::new_error(
                    span.clone(),
                    format!(
                        "operands must be numeric, left operand: {}, right operand: {}",
                        l, r
                    ),
                ))
            }
            (Add, l, r) => Err(RuntimeError::new_error(
                span.clone(),
                format!(
                    "operands must be numeric or strings, left operand: {}, right operand: {}",
                    l, r
                ),
            )),
        }
        .map(|value| Rc::new(RefCell::new(value)))
    }

    fn visit_call(
        &mut self,
        callee: &ast::Expr,
        arguments: &Vec<ast::Expr>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let callee = self.evaluate(callee)?;

        let mut args = vec![];
        for arg in arguments {
            args.push(self.evaluate(arg)?);
        }

        if args.len() != callee.borrow().arity(span)? {
            Err(RuntimeError::new_error(
                span.clone(),
                format!(
                    "Expected {} arguments but got {}.",
                    args.len(),
                    callee.borrow().arity(span)?
                ),
            ))?
        }

        Value::call(callee, self, args, span)
    }

    fn visit_get(
        &mut self,
        object: &Box<ast::Expr>,
        name: &String,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let value = self.evaluate(object)?;
        match &*value.borrow() {
            Value::Instance { fields, class, .. } => {
                if let Some(field) = fields.get(name) {
                    Ok(field.clone())
                } else if let Some(method) = Value::find_method(class, name) {
                    Ok(Value::bind(method.clone(), value.clone()))
                } else {
                    Err(RuntimeError::new_error(
                        span.clone(),
                        format!("Undefined property {}.", name),
                    ))
                }
            }
            _ => Err(RuntimeError::new_error(
                span.clone(),
                "Only instances have properties.".to_string(),
            ))?,
        }
    }

    fn visit_grouping(
        &mut self,
        expr: &Box<ast::Expr>,
        _: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        self.visit_expr(expr)
    }

    fn visit_literal(
        &mut self,
        l: &ast::LitVal,
        _: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        use Value::*;
        use ast::LitVal;
        Ok(Rc::new(RefCell::new(match l {
            LitVal::Number(n) => Number(*n),
            LitVal::String(s) => String(s.clone()),
            LitVal::True => Bool(true),
            LitVal::False => Bool(false),
            LitVal::Nil => Nil,
        })))
    }

    fn visit_logic_and(
        &mut self,
        lhs: &Box<ast::Expr>,
        rhs: &Box<ast::Expr>,
        _: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let left = self.evaluate(lhs)?;

        if !left.borrow().is_truthy() {
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
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        let left = self.evaluate(lhs)?;

        if left.borrow().is_truthy() {
            Ok(left)
        } else {
            Ok(self.evaluate(rhs)?)
        }
    }

    fn visit_set(
        &mut self,
        object: &Box<ast::Expr>,
        name: &String,
        value: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        match *self.evaluate(object)?.borrow_mut() {
            Value::Instance { ref mut fields, .. } => {
                let value = self.evaluate(value)?;
                fields.insert(name.clone(), value.clone());
                Ok(value)
            }
            _ => Err(RuntimeError::new_error(
                span.clone(),
                "Only instances have fields.".to_string(),
            )),
        }
    }

    fn visit_this(
        &mut self,
        id: &usize,
        keyword: &String,
        _: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        Ok(self.lookup_variable(keyword, id))
    }

    fn visit_unary(
        &mut self,
        un_op: &ast::UnOp,
        rhs: &Box<ast::Expr>,
        span: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        use Value::*;
        use ast::UnOp;
        match (un_op, &*self.visit_expr(rhs)?.borrow()) {
            (UnOp::Neg, Number(n)) => Ok(Number(-n)),
            (UnOp::Not, val) => Ok(Bool(!val.is_truthy())),
            _ => Err(RuntimeError::new_error(
                span.clone(),
                format!("failed unary operation with operand"),
            )),
        }
        .map(|value| Rc::new(RefCell::new(value)))
    }

    fn visit_variable(
        &mut self,
        id: &usize,
        name: &String,
        _: &span::Span,
    ) -> Result<Rc<RefCell<Value>>, RuntimeError> {
        Ok(self.lookup_variable(name, id))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_visit_unary() {}
}
