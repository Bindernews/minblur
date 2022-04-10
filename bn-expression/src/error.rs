use std::error::Error as StdError;
use std::fmt;

use nom::error::VerboseErrorKind;
use thiserror::Error;

use crate::parse::{ErrType, Position, Span};

pub type BoxError = Box<dyn StdError + 'static>;

/// Error type for expression evaluation
///
/// If in doubt use either [`EvalError::CallError`] or, if passing through
/// a nested error type, [`EvalError::Other`].
#[derive(Error, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize), serde(tag = "kind"))]
pub enum EvalError {
    /// A value was unresolved during a full eval, thus preventing the evaluation.
    #[error("Unresolved value during eval()")]
    UnresolvedValue,

    /// Returned by `EvalContext::get_variable()` to indicate the variable name cannot
    /// be resolved to a value.
    #[error("Unknown variable name: {name}")]
    UnknownVariable { name: String },

    /// Returned by `evaluate()` to indicate an unknown function.
    #[error("Unknown function: {name}")]
    UnknownFunction { name: String },

    /// Returned by `Expression::evaluate()`, this indicates a full evaluation was
    /// requested, but the expression tried to call a function only available during runtime.
    #[error("The function '{name}' cannot be evaluated in this context")]
    DelayFunctionCalled { name: String },

    /// Invalid argument
    #[error("invalid argument {arg_index} to function '{function}'")]
    InvalidArgument { function: String, arg_index: usize },

    /// Generic error for when a function failed
    #[error("function '{function}' failed => {message:?}")]
    CallError { function: String, message: String },

    /// Returned by `AValue::eval()` when trying to perform an operation on incompatible types.
    #[error("Can't perform '{op}' on {lhs} and {rhs}")]
    IncompatibleTypes {
        op: String,
        lhs: &'static str,
        rhs: &'static str,
    },

    /// Error-chain for parser errors
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error("{0}")]
    #[cfg_attr(feature = "serialize", serde(serialize_with = "serialize_other"))]
    Other(BoxError),
}
impl EvalError {
    pub fn unknown_variable(name: &str) -> Self {
        Self::UnknownVariable {
            name: name.to_string(),
        }
    }
    pub fn unknown_function(name: &str) -> Self {
        Self::UnknownFunction {
            name: name.to_string(),
        }
    }
    pub fn delay_function_called(name: &str) -> Self {
        Self::DelayFunctionCalled {
            name: name.to_string(),
        }
    }
    pub fn call_error<M: Into<String>>(function: &str, message: M) -> Self {
        Self::CallError {
            function: function.to_string(),
            message: message.into(),
        }
    }
    pub fn invalid_arg(function: &str, index: usize) -> Self {
        Self::InvalidArgument {
            function: function.to_string(),
            arg_index: index,
        }
    }
    pub fn other<E: StdError + 'static>(e: E) -> Self {
        let error = Box::new(e) as Box<dyn StdError + 'static>;
        match error.downcast::<Self>() {
            Ok(e2) => *e2,
            Err(e2) => Self::Other(e2),
        }
    }

    /// See [`StdError`] `::downcast()`
    pub fn downcast<E: StdError + 'static>(self) -> Result<Box<E>, Self> {
        match self {
            Self::Other(e) => match e.downcast::<E>() {
                Ok(e2) => Ok(e2),
                Err(e2) => Err(Self::Other(e2)),
            },
            _ => Err(self),
        }
    }

    /// See [`StdError`] `::downcast_ref()`
    pub fn downcast_ref<E: StdError + 'static>(&self) -> Option<&E> {
        if let Self::Other(e) = self {
            e.downcast_ref::<E>()
        } else {
            None
        }
    }

    /// See [`StdError`] `::downcast_mut()`
    pub fn downcast_mut<E: StdError + 'static>(&mut self) -> Option<&mut E> {
        if let Self::Other(e) = self {
            e.downcast_mut::<E>()
        } else {
            None
        }
    }
}

#[cfg(feature = "serialize")]
fn serialize_other<S: serde::Serializer>(error: &BoxError, se: S) -> Result<S::Ok, S::Error> {
    se.serialize_str(&format!("{:?}", error))
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct ParseError {
    /// Position in the input of the error
    pub position: Position,
    /// Hint about what caused the error
    pub hint: String,
    /// Character at the location of the error (if available)
    pub near: char,
    /// Cause of this error
    pub cause: Option<Box<ParseError>>,
}
impl ParseError {
    fn display(&self, f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
        write!(f, "{0:indent$}at {1} ", "", self.position, indent = indent)?;
        if !self.near.is_control() && !self.near.is_whitespace() {
            write!(f, "near '{0}' ", self.near)?;
        }
        writeln!(f, "=> {0}", self.hint)?;
        if let Some(cause) = &self.cause {
            cause.display(f, indent + 2)?;
        }
        Ok(())
    }
}
impl From<ErrType<'_>> for ParseError {
    fn from(error: ErrType) -> Self {
        Self::try_from(error.errors.as_slice()).unwrap()
    }
}
impl TryFrom<&[(Span<'_>, VerboseErrorKind)]> for ParseError {
    type Error = ();
    fn try_from(errors: &[(Span<'_>, VerboseErrorKind)]) -> Result<Self, Self::Error> {
        let e0 = errors.last().ok_or(())?;
        // Use the nearest character or a zero-width space if no character
        let near = e0.0.chars().next().unwrap_or('\0');
        // Try to grab the hint, ignoring most Nom errors because they're not very helpful
        // unless it's the final one.
        let hint = match e0.1 {
            VerboseErrorKind::Context(s) => Some(s.to_string()),
            VerboseErrorKind::Char(c) => Some(format!("expected '{}'", c)),
            VerboseErrorKind::Nom(kind) => {
                if errors.len() == 1 {
                    Some(format!("{:?}", kind))
                } else {
                    None
                }
            }
        };
        // If we got hint, great, otherwise try parsing the next error down the chain
        if let Some(hint) = hint {
            let position = Position::from_span(&e0.0);
            let cause = Self::try_from(&errors[..errors.len() - 1])
                .map(Box::new)
                .ok();
            Ok(Self {
                position,
                hint,
                near,
                cause,
            })
        } else {
            Self::try_from(&errors[..errors.len() - 1])
        }
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display(f, 0)
    }
}
