use std::{cmp::Ordering, fmt, hash::Hash};

use crate::error::EvalError;

#[derive(Clone, Debug)]
pub enum AValue {
    /// Variable name, function name, etc.
    Name(String),
    /// String surrounded by quotes
    String(String),
    /// All numbers are floats yeah?
    Num(f64),
    /// Null
    Null,
}
impl AValue {
    pub fn name<S: AsRef<str>>(name: S) -> Self {
        Self::Name(String::from(name.as_ref()))
    }

    pub fn string<S: Into<String>>(s: S) -> Self {
        Self::String(s.into())
    }

    /// Returns true if the value is a String or Num
    pub fn is_value(&self) -> bool {
        !self.is_name()
    }

    pub fn is_name(&self) -> bool {
        matches!(self, Self::Name(_))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Name(_) => false,
            Self::String(s) => !s.is_empty(),
            Self::Num(v) => *v != 0f64,
            Self::Null => false,
        }
    }

    pub fn into_num(&self) -> Option<Self> {
        match self {
            Self::Name(_) | Self::String(_) | Self::Null => None,
            Self::Num(_) => Some(self.clone()),
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Num(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::Num(_) | Self::Null => None,
            Self::Name(s) => Some(s),
            Self::String(s) => Some(s),
        }
    }

    pub fn op_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Self::Name(lh), Self::Name(rh)) => Some(lh.cmp(rh)),
            (Self::String(lh), Self::String(rh)) => Some(lh.cmp(rh)),
            (Self::Num(lh), Self::Num(rh)) => lh.partial_cmp(rh),
            _ => None,
        }
    }

    /// Helper function to do math. Takes an operator symbol (for error result)
    /// and a function to apply the math.
    pub fn apply_math(
        &self,
        rhs: &Self,
        op: &'static str,
        func: impl Fn(f64, f64) -> f64,
    ) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Num(lh), Self::Num(rh)) => Ok(func(*lh, *rh).into()),
            _ => Err(EvalError::IncompatibleTypes {
                op: op.into(),
                lhs: self.type_name(),
                rhs: rhs.type_name(),
            }),
        }
    }

    pub fn apply_int_math(
        &self,
        rhs: &Self,
        op: &'static str,
        func: impl Fn(i64, i64) -> i64,
    ) -> Result<Self, EvalError> {
        match (self, rhs) {
            (Self::Num(lh), Self::Num(rh)) => {
                let r = func(*lh as i64, *rh as i64) as f64;
                Ok(r.into())
            }
            _ => Err(EvalError::IncompatibleTypes {
                op: op.into(),
                lhs: self.type_name(),
                rhs: rhs.type_name(),
            }),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Name(_) => "Name",
            Self::String(_) => "String",
            Self::Num(_) => "Num",
            Self::Null => "Null",
        }
    }
}
impl fmt::Display for AValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Name(v) => f.write_str(v),
            Self::String(v) => write!(f, "\"{}\"", v),
            Self::Num(v) => write!(f, "{}", v),
            Self::Null => write!(f, "null"),
        }
    }
}
impl Default for AValue {
    fn default() -> Self {
        Self::Null
    }
}

impl From<f64> for AValue {
    fn from(v: f64) -> Self {
        Self::Num(v)
    }
}
impl From<i64> for AValue {
    fn from(v: i64) -> Self {
        Self::Num(v as f64)
    }
}
impl From<bool> for AValue {
    fn from(v: bool) -> Self {
        Self::Num(if v { 1f64 } else { 0f64 })
    }
}
impl PartialEq for AValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Name(l0), Self::Name(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Num(l0), Self::Num(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl Hash for AValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Self::Name(v) => v.hash(state),
            Self::String(v) => v.hash(state),
            Self::Num(v) => v.to_bits().hash(state),
            Self::Null => state.write_u32(0),
        }
    }
}
