use std::error::Error as StdError;
use std::ops::Range;

use crate::{AValue, EvalError, Expression, ExpressionOp};

pub struct ArgumentHelper<'a> {
    pub function: &'a str,
    pub args: Vec<AValue>,
}
impl<'a> ArgumentHelper<'a> {
    pub fn new(name: &'a str, args: Vec<AValue>) -> Self {
        Self {
            function: name,
            args,
        }
    }

    /// Assert that there are between `min` and `max` arguments.
    ///
    /// # Arguments
    /// * `min` - minimum number of arguments
    /// * `max` - maximum number of arguments (inclusive)
    ///
    pub fn assert_count(&self, min: usize, max: usize) -> Result<(), EvalError> {
        let ct = self.args.len();
        if min <= ct && ct <= max {
            Ok(())
        } else {
            let msg = if min == max {
                format!("expected {} args, got {}", min, ct)
            } else {
                format!("expected between {} and {} args, got {}", min, max, ct)
            };
            Err(EvalError::call_error(self.function, msg))
        }
    }

    pub fn map_arg<F, T, E>(&self, index: usize, f: F) -> Result<T, EvalError>
    where
        F: FnOnce(&AValue) -> Result<T, E>,
        E: StdError + 'static,
    {
        self.args
            .get(index)
            .ok_or_else(|| EvalError::invalid_arg(self.function, index))
            .and_then(|v| f(v).map_err(EvalError::other))
    }

    pub fn map_arg_opt<F, T>(&self, index: usize, f: F) -> Result<T, EvalError>
    where
        F: FnOnce(&AValue) -> Option<T>,
    {
        self.args
            .get(index)
            .and_then(f)
            .ok_or_else(|| EvalError::invalid_arg(self.function, index))
    }

    pub fn return_input<Op: ExpressionOp>(self) -> Expression<Op> {
        Expression::call_from_values(self.function, self.args)
    }

    pub fn take(self) -> Vec<AValue> {
        self.args
    }

    pub fn args_as_f64(&self, range: Range<usize>) -> Result<Box<[f64]>, EvalError> {
        let mut result = Vec::new();
        for i in range {
            let v2 = self
                .args
                .get(i)
                .and_then(|v| v.as_f64())
                .ok_or_else(|| EvalError::invalid_arg(self.function, i))?;
            result.push(v2);
        }
        Ok(result.into_boxed_slice())
    }
}
