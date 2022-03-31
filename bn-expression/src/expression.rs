use std::fmt::{self, Write};

use super::{avalue::AValue, error::EvalError};

/// [`ExpressionOp`]s are generally going to be `enum`s and thus the trait
/// requirements should be easy to fulfill.
///
/// It's recommended that implementors also implement [`Copy`].
pub trait ExpressionOp:
    Clone + PartialEq + fmt::Debug + fmt::Display + std::hash::Hash + 'static
{
    /// Evaluate the operator with given values, for unary operators `right` will be [`AValue::Null`]
    fn eval(&self, left: &AValue, right: &AValue) -> Result<AValue, EvalError>;

    /// Returns the operator precedence (lower number = higher precedence)
    ///
    /// [`usize::MAX`] is a reserved value
    fn precedence(&self) -> usize;

    /// Return true if the operator is unary, false if binary
    fn is_unary(&self) -> bool;

    /// [`nom`]-style matcher to match any operator, or return an error
    /// message if an operator wasn't found.
    fn match_op(input: &str) -> Result<(&str, Self), &'static str>;
}

/// Expression evaluation and folding all in one, convenient class which is generic
/// over the list of available operators.
///
/// [`Expression`] supports
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Expression<Op: ExpressionOp> {
    Value(AValue),
    Binary(Op, Box<Expression<Op>>, Box<Expression<Op>>),
    Unary(Op, Box<Expression<Op>>),
    Call(Box<str>, Vec<Expression<Op>>),
}
impl<Op: ExpressionOp> Expression<Op> {
    pub fn new_call(name: &str, args: Vec<Expression<Op>>) -> Self {
        Self::Call(String::from(name).into_boxed_str(), args)
    }

    /// Construct a new [`Expression`] from a name and an slice of [`AValue`]s.
    ///
    /// Function calls which partially evaluate may find this helpful to
    /// easily construct a return expression.
    pub fn call_from_values(name: &str, mut args: Vec<AValue>) -> Self {
        let args = args.drain(..).map(|v| v.into()).collect();
        Self::new_call(name, args)
    }

    /// Fully evaluate the expression and return a value.
    pub fn eval<C: EvalContext<Op>>(&self, ctx: &mut C) -> Result<AValue, EvalError> {
        self.evaluate(false, ctx).map(|e| e.into_value().unwrap())
    }

    /// Partially evaluate the expression, leaving any named variables named.
    /// Function arguments will be evaluated and potentially
    pub fn partial_eval<C: EvalContext<Op>>(
        &self,
        ctx: &mut C,
    ) -> Result<Expression<Op>, EvalError> {
        self.evaluate(true, ctx)
    }

    /// Evalute the expression either partially or fully.
    ///
    /// A partial-evaluation will resolve as many values as possible and then return the
    /// most evaluated expression tree possible. This is basically constant-folding.
    /// A full evaluation will report an error if an expression cannot be fully evaluated
    ///
    pub fn evaluate<C: EvalContext<Op>>(
        &self,
        partial: bool,
        ctx: &mut C,
    ) -> Result<Expression<Op>, EvalError> {
        match self {
            Self::Value(v) => match v {
                AValue::Name(n) => ctx
                    .get_variable(n)
                    .map(Expression::from)?
                    .value_check(partial),
                _ => Ok(v.clone().into()),
            },
            Self::Binary(op, left, right) => {
                let left_expr = left.evaluate(partial, ctx)?.value_check(partial)?;
                let right_expr = right.evaluate(partial, ctx)?.value_check(partial)?;
                match (&left_expr, &right_expr) {
                    // Check to make sure we don't have any partials before calling eval
                    (Self::Value(lval), Self::Value(rval))
                        if lval.is_value() && rval.is_value() =>
                    {
                        op.eval(lval, rval).map(Into::into)
                    }
                    // No need to test if partial here because if not we'll never reach this branch.
                    _ => Ok(Self::Binary(
                        op.clone(),
                        Box::new(left_expr),
                        Box::new(right_expr),
                    )),
                }
            }
            Self::Unary(op, left) => {
                let left_expr = left.partial_eval(ctx)?.value_check(partial)?;
                match left_expr {
                    Self::Value(lval) if lval.is_value() => {
                        op.eval(&lval, &AValue::Null).map(Into::into)
                    }
                    // No need to test if partial here because if not we'll never reach this branch.
                    _ => Ok(Self::Unary(op.clone(), Box::new(left_expr))),
                }
            }
            Self::Call(func_name, args) => {
                let mut args_expr = Vec::new();
                for arg in args {
                    args_expr.push(arg.partial_eval(ctx)?);
                }

                let f_type = ctx.get_function_type(func_name);
                if f_type.is_unknown() {
                    return Err(EvalError::UnknownFunction {
                        name: func_name.to_string(),
                    });
                }
                // We need to eval it (!partial) but we can't (!is_eval()) so error
                if !partial && !f_type.is_eval() {
                    return Err(EvalError::DelayFunctionCalled {
                        name: func_name.to_string(),
                    });
                }
                // If everything is a value, attempt to evaluate it now
                if f_type.is_eval() && args_expr.iter().all(|v| v.as_value().is_some()) {
                    // If all the args are values, we can call the function
                    let args_vals: Vec<AValue> = args_expr
                        .drain(..)
                        .map(|e| e.into_value().unwrap())
                        .collect();
                    ctx.call(func_name, args_vals)
                } else {
                    Ok(Expression::Call(func_name.clone(), args_expr))
                }
            }
        }
    }

    /// Helper function that ensures that self is a valid value iff `partial`,
    /// otherwise just returns self.
    fn value_check(self, partial: bool) -> Result<Expression<Op>, EvalError> {
        if !partial {
            match &self {
                Self::Value(v) if v.is_value() => Ok(self),
                _ => Err(EvalError::UnresolvedValue),
            }
        } else {
            Ok(self)
        }
    }

    /// If this expression has an operator, return it
    pub fn get_op(&self) -> Option<Op> {
        match self {
            Self::Unary(op, _) => Some(op.clone()),
            Self::Binary(op, _, _) => Some(op.clone()),
            _ => None,
        }
    }

    /// Consumes this expression and returns an [`AValue`] if this expression was a value,
    /// or the original [`Expression`] otherwise.
    pub fn into_value(self) -> Result<AValue, Self> {
        match self {
            Self::Value(v) => Ok(v),
            _ => Err(self),
        }
    }

    /// Returns a reference to the inner value, or [`None`] if this expression is not a value.
    pub fn as_value(&self) -> Option<&AValue> {
        match self {
            Self::Value(v) => Some(v),
            _ => None,
        }
    }

    /// Pretty print implementation
    ///
    /// * `root` - Is this the root expression?
    /// * `parent_op` - Parent operation (if available)
    ///
    fn pretty_print_rec(
        &self,
        root: bool,
        parent_op: Option<Op>,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let mut show_parens = !root;
        match self {
            Self::Value(v) => fmt::Display::fmt(v, f),
            Self::Binary(op, lhs, rhs) => {
                if let Some(p_op) = parent_op {
                    if op.precedence() <= p_op.precedence() {
                        show_parens = false;
                    }
                }
                if show_parens {
                    f.write_char('(')?;
                }
                lhs.pretty_print_rec(false, Some(op.clone()), f)?;
                write!(f, " {} ", op)?;
                rhs.pretty_print_rec(false, None, f)?;
                if show_parens {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::Unary(op, lhs) => {
                if let Some(p_op) = parent_op {
                    if op.precedence() <= p_op.precedence() {
                        show_parens = false;
                    }
                }
                if show_parens {
                    f.write_char('(')?;
                }
                write!(f, "{} ", op)?;
                lhs.pretty_print_rec(false, Some(op.clone()), f)?;
                if show_parens {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::Call(func_name, args) => {
                write!(f, "{}(", func_name)?;
                let mut add_comma = args.len();
                for arg in args {
                    arg.pretty_print_rec(false, None, f)?;
                    if add_comma > 1 {
                        write!(f, ", ")?;
                    }
                    add_comma -= 1;
                }
                write!(f, ")")
            }
        }
    }
}
impl<T: Into<AValue>, Op: ExpressionOp> From<T> for Expression<Op> {
    fn from(v: T) -> Self {
        Expression::Value(v.into())
    }
}
impl<Op: ExpressionOp> fmt::Display for Expression<Op> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.pretty_print_rec(true, None, f)
    }
}

/// Implements the visitor pattern for expression trees.
pub trait Visitor<A, E, Op: ExpressionOp> {
    fn visit(&mut self, expr: &Expression<Op>, accum: A) -> Result<A, E>;
}

impl<'r, F, A, E, Op> Visitor<A, E, Op> for F
where
    Op: ExpressionOp,
    F: FnMut(&Expression<Op>, A) -> Result<A, E> + 'r,
{
    fn visit(&mut self, expr: &Expression<Op>, accum: A) -> Result<A, E> {
        self(expr, accum)
    }
}

impl<Op: ExpressionOp> Expression<Op> {
    /// Apply a visitor to each expression in the tree.
    ///
    /// If `top_down` is true, the visitor will visit parents first, then children,
    /// otherwise it'll visit children then parents. Function arguments are *always*
    /// visited from first to last.
    ///
    pub fn visit<A, E, V: Visitor<A, E, Op>>(
        &self,
        top_down: bool,
        accum: A,
        mut visitor: V,
    ) -> Result<A, E> {
        match self {
            Self::Value(_) => visitor.visit(self, accum),
            Self::Binary(_, left, right) => {
                let ac = accum;
                let ac = if top_down {
                    visitor.visit(self, ac)?
                } else {
                    ac
                };
                let ac = visitor.visit(left, ac)?;
                let ac = visitor.visit(right, ac)?;
                let ac = if !top_down {
                    visitor.visit(self, ac)?
                } else {
                    ac
                };
                Ok(ac)
            }
            Self::Unary(_, left) => {
                let mut ac = accum;
                if top_down {
                    ac = visitor.visit(self, ac)?;
                    ac = visitor.visit(left, ac)?;
                } else {
                    ac = visitor.visit(left, ac)?;
                    ac = visitor.visit(self, ac)?;
                }
                Ok(ac)
            }
            Self::Call(_, args) => {
                let mut ac = accum;
                if top_down {
                    ac = visitor.visit(self, ac)?;
                }
                for expr in args {
                    ac = visitor.visit(expr, ac)?;
                }
                if !top_down {
                    ac = visitor.visit(self, ac)?;
                }
                Ok(ac)
            }
        }
    }
}

pub fn new_bin_expression<Op: ExpressionOp, Lh: Into<Expression<Op>>, Rh: Into<Expression<Op>>>(
    op: Op,
    lh: Lh,
    rh: Rh,
) -> Expression<Op> {
    Expression::Binary(op, Box::new(lh.into()), Box::new(rh.into()))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FunctionType {
    /// Function is unknown
    Unknown,
    /// Function may be called immediately
    Eval,
    /// Function is known but cannot be called immediately.
    /// This will cause an error during a "concrete" evaluation
    Delay,
}
impl FunctionType {
    pub fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
    pub fn is_eval(&self) -> bool {
        matches!(self, Self::Eval)
    }
    pub fn is_runtime(&self) -> bool {
        matches!(self, Self::Delay)
    }
}

pub trait EvalContext<Op: ExpressionOp> {
    /// Returns the value of the variable
    ///
    /// For a "partial" evaluation the returned value may be a [`AValue::Name`]
    /// in which case it may be resolved again later.
    fn get_variable(&mut self, name: &str) -> Result<AValue, EvalError>;

    /// Evaluate a function
    fn call(&mut self, name: &str, _args: Vec<AValue>) -> Result<Expression<Op>, EvalError> {
        Err(EvalError::UnknownFunction { name: name.into() })
    }

    /// Get the type of a function
    fn get_function_type(&self, _name: &str) -> FunctionType {
        FunctionType::Unknown
    }
}
