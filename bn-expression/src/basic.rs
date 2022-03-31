use std::collections::HashMap;
use std::{cmp::Ordering, fmt};

use nom::InputTake;

use crate::{enum_to_from_str, error::EvalError, expression::EvalContext, AValue, ExpressionOp};

/// Example [`ExpressionOp`] implementation which can also be used to help
/// with other implementations.
///
/// Other implementations of [`ExpressionOp`] may use [`BasicOp`] to simplify
/// the implementation of the [`ExpressionOp::eval`] function. See the example below.
/// ```rust
/// use bn_expression::{AValue, ExpressionOp, EvalError, enum_to_from_str};
/// use bn_expression::basic::BasicOp;
/// use std::fmt;
///
/// #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// enum MyOp {
///     Add,
///     Sub,
/// }
/// impl MyOp {
///     enum_to_from_str!(
///         MyOp; fn as_str(); fn from_str();
///         {
///             MyOp::Add => "+",
///             MyOp::Sub => "-",
///         }
///     );
/// }
/// impl fmt::Display for MyOp {
///     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
///         f.write_str(self.as_str())
///     }
/// }
/// impl ExpressionOp for MyOp {
///     fn eval(&self, left: &AValue, right: &AValue) -> Result<AValue, EvalError> {
///         match self {
///             MyOp::Add => BasicOp::Add.eval(left, right),
///             MyOp::Sub => BasicOp::Sub.eval(left, right),
///         }
///     }
///     fn precedence(&self) -> usize {
///         match self {
///             Self::Add | Self::Sub => 1,
///         }
///     }
///     fn is_unary(&self) -> bool {
///         false
///     }
///     fn match_op(input: &str) -> Result<(&str, Self), &'static str> {
///         use nom::InputTake;
///         // Ensure there's enough input
///         if input.len() >= 1 {
///             let (remain, used) = input.take_split(1);
///             if let Some(op) = Self::from_str(used) {
///                 return Ok((remain, op));
///             }
///         }
///         Err("no operator found")
///     }
///     
/// }
/// ```
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum BasicOp {
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    LAnd,
    LOr,
    LXor,
    BAnd,
    BOr,
    BXor,
    Not,
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Shl,
    Shr,
}
impl BasicOp {
    enum_to_from_str!(
        BasicOp; pub fn as_expr_str(); pub fn from_expr_str();
        {
            BasicOp::Add => "+",
            BasicOp::Sub => "-",
            BasicOp::Mul => "*",
            BasicOp::Div => "/",
            BasicOp::IDiv => "//",
            BasicOp::Mod => "%",
            BasicOp::LAnd => "&&",
            BasicOp::LOr => "||",
            BasicOp::LXor => "^^",
            BasicOp::BAnd => "&",
            BasicOp::BOr => "|",
            BasicOp::BXor => "^",
            BasicOp::Not => "!",
            BasicOp::Equal => "==",
            BasicOp::NotEqual => "!=",
            BasicOp::LessThan => "<",
            BasicOp::LessThanEq => "<=",
            BasicOp::GreaterThan => ">",
            BasicOp::GreaterThanEq => ">=",
            BasicOp::Shl => "<<",
            BasicOp::Shr => ">>",
        }
    );
}
impl fmt::Display for BasicOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_expr_str())
    }
}
impl ExpressionOp for BasicOp {
    fn eval(&self, left: &AValue, right: &AValue) -> Result<AValue, EvalError> {
        fn helper_cmp(
            left: &AValue,
            right: &AValue,
            test: impl Fn(Ordering) -> bool,
        ) -> Result<AValue, EvalError> {
            Ok(AValue::from(left.op_cmp(right).map(test).unwrap_or(false)))
        }
        match self {
            Self::Add => left.apply_math(right, "+", |lh, rh| lh + rh),
            Self::Sub => left.apply_math(right, "-", |lh, rh| lh - rh),
            Self::Mul => left.apply_math(right, "*", |lh, rh| lh * rh),
            Self::Div => left.apply_math(right, "/", |lh, rh| lh / rh),
            Self::IDiv => left.apply_int_math(right, "//", |lh, rh| lh / rh),
            Self::Mod => left.apply_int_math(right, "%", |lh, rh| lh % rh),
            Self::LAnd => Ok((left.is_truthy() && right.is_truthy()).into()),
            Self::LOr => Ok((left.is_truthy() || right.is_truthy()).into()),
            Self::LXor => Ok((left.is_truthy() ^ right.is_truthy()).into()),
            Self::BAnd => left.apply_int_math(right, "&&", |lh, rh| lh & rh),
            Self::BOr => left.apply_int_math(right, "||", |lh, rh| lh | rh),
            Self::BXor => left.apply_int_math(right, "^^", |lh, rh| lh ^ rh),
            Self::Not => Ok((!left.is_truthy()).into()),
            Self::Equal => helper_cmp(left, right, |f| f.is_eq()),
            Self::NotEqual => helper_cmp(left, right, |f| f.is_ne()),
            Self::LessThan => helper_cmp(left, right, |f| f.is_lt()),
            Self::LessThanEq => helper_cmp(left, right, |f| f.is_le()),
            Self::GreaterThan => helper_cmp(left, right, |f| f.is_gt()),
            Self::GreaterThanEq => helper_cmp(left, right, |f| f.is_ge()),
            Self::Shl => left.apply_int_math(right, "<<", |lh, rh| lh << rh),
            Self::Shr => left.apply_int_math(right, ">>", |lh, rh| lh >> rh),
        }
    }

    /// Returns the operator precedence (lower number = higher precedence)
    fn precedence(&self) -> usize {
        match self {
            Self::Mul | Self::Div | Self::Mod | Self::IDiv => 1,
            Self::Add | Self::Sub => 2,
            Self::BAnd | Self::BOr | Self::BXor => 4,
            Self::LAnd | Self::LOr | Self::LXor => 5,
            Self::Shl | Self::Shr => 6,
            Self::LessThan
            | Self::LessThanEq
            | Self::GreaterThan
            | Self::GreaterThanEq
            | Self::Equal
            | Self::NotEqual => 7,
            Self::Not => 8,
        }
    }

    fn is_unary(&self) -> bool {
        matches!(self, Self::Not)
    }

    fn match_op(input: &str) -> Result<(&str, Self), &'static str> {
        // All our operators have either 2 or 1 characters, so try parsing
        // strings with those sizes into Self.
        for op_length in [2, 1] {
            if input.len() >= op_length {
                let (remain, used) = input.take_split(op_length);
                if let Some(op) = Self::from_expr_str(used) {
                    return Ok((remain, op));
                }
            }
        }
        Err("failed to parse op")
    }
}

pub struct BasicEvaluator {
    pub variables: HashMap<String, AValue>,
    /// If true, allow partial evaluation
    pub partial: bool,
}
impl BasicEvaluator {
    pub fn new(partial: bool) -> Self {
        Self {
            variables: HashMap::new(),
            partial,
        }
    }
}
impl<Op: ExpressionOp> EvalContext<Op> for BasicEvaluator {
    fn get_variable(&mut self, name: &str) -> Result<AValue, EvalError> {
        self.variables
            .get(name)
            .map(Clone::clone)
            .or_else(|| {
                if self.partial {
                    Some(AValue::name(name))
                } else {
                    None
                }
            })
            .ok_or_else(|| EvalError::unknown_variable(name))
    }
}

pub mod functions {
    use crate::{AValue, EvalError, Expression, ExpressionOp};

    pub fn call_max<Op: ExpressionOp>(
        name: &str,
        args: Vec<AValue>,
    ) -> Result<Expression<Op>, EvalError> {
        let result = args
            .iter()
            .map(|v| v.as_f64())
            .reduce(|a, b| a.zip(b).map(|x| f64::max(x.0, x.1)))
            .flatten()
            .map(|v| AValue::from(v).into())
            .unwrap_or_else(|| Expression::<Op>::call_from_values(name, args));
        Ok(result)
    }

    pub fn call_min<Op: ExpressionOp>(
        name: &str,
        args: Vec<AValue>,
    ) -> Result<Expression<Op>, EvalError> {
        let result = args
            .iter()
            .map(|v| v.as_f64())
            .reduce(|a, b| a.zip(b).map(|x| f64::min(x.0, x.1)))
            .flatten()
            .map(|v| AValue::from(v).into())
            .unwrap_or_else(|| Expression::<Op>::call_from_values(name, args));
        Ok(result)
    }
}
