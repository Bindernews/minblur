use std::{
    error::Error as StdError,
    fmt::{self, Debug, Display, Formatter},
};

use bn_expression::{EvalError, ParseError};
pub use dynamic_error::*;

use crate::{
    compact_from,
    parser::{ArcSource, Source},
};
pub type TResult<T> = Result<T, CompileError>;

#[derive(thiserror::Error, Debug, strum::IntoStaticStr)]
pub enum CompileError {
    /// Group many errors together into one
    Many(Vec<CompileError>),
    /// Located error specific to a compiler pass
    PassError(ArcSource, PassError),
    /// Generic wrapper for errors generated by a macro
    MacroError(ArcSource, Box<dyn DynamicError>),
}
impl CompileError {
    pub fn macro_error<E: DynamicError>(source: Source, err: E) -> Self {
        Self::MacroError(source.into(), Box::new(err))
    }

    pub fn many(mut errors: Vec<CompileError>) -> Self {
        let mut new_errors = Vec::new();
        for e in errors.drain(..) {
            e.append_to(&mut new_errors);
        }
        Self::Many(new_errors)
    }

    /// Append this error to the array of errors, flattening
    /// if this error is an [`CompileError::Many`].
    pub fn append_to(self, errors: &mut Vec<CompileError>) {
        match self {
            Self::Many(mut e) => errors.append(&mut e),
            _ => errors.push(self),
        }
    }

    pub fn into_many(self) -> Vec<CompileError> {
        match self {
            Self::Many(v) => v,
            _ => {
                vec![self]
            }
        }
    }

    pub fn into_inner(self) -> Box<dyn StdError + 'static> {
        match self {
            Self::PassError(_, e) => Box::new(e),
            Self::MacroError(_, e) => e.upcast(),
            _ => Box::new(self),
        }
    }

    pub fn get_source(&self) -> Option<ArcSource> {
        match self {
            Self::Many(errors) if errors.len() == 1 => errors[0].get_source(),
            Self::PassError(source, _) => Some(source.clone()),
            Self::MacroError(source, _) => Some(source.clone()),
            _ => None,
        }
    }

    /// Print an error with an attached source
    fn print_with_source<E>(f: &mut Formatter<'_>, source: &ArcSource, error: &E) -> fmt::Result
    where
        E: Display,
    {
        write!(f, "Error {source}")?;
        let prefix = "| ";
        let msg = format!("{}", error);
        for line in msg.lines() {
            writeln!(f, "{}{}", prefix, line)?;
        }
        Ok(())
    }
}
impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Many(errors) => {
                for error in errors {
                    writeln!(f, "{}", error)?;
                }
            }
            Self::PassError(source, error) => {
                Self::print_with_source(f, source, error)?;
            }
            Self::MacroError(source, error) => {
                Self::print_with_source(f, source, error)?;
            }
        }
        Ok(())
    }
}
#[cfg(feature = "json")]
impl super::format::FormatJson for CompileError {
    fn format_json(&self) -> serde_json::Value {
        use serde_json::json;
        match self {
            CompileError::Many(errors) => {
                serde_json::Value::Array(errors.iter().map(|e| e.format_json()).collect())
            }
            CompileError::PassError(source, error) => {
                json!({ "source": source, "error": error.format_json() })
            }
            CompileError::MacroError(source, error) => {
                json!({ "source": source, "error": error.format_json() })
            }
        }
    }
}
impl DynamicError for CompileError {}

#[derive(thiserror::Error, Debug, strum::IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
#[cfg_attr(
    feature = "serialize",
    derive(serde::Serialize),
    serde(tag = "kind", content = "c")
)]
pub enum PassError {
    /// Returned when a macro is registered with a name that's already registered
    /// by a different macro.
    #[error("Macro name already registered: {name}")]
    DuplicateMacroName { name: String },
    #[error("unknown label '{name}'")]
    UnknownLabel { name: String },
    #[error("Unknown compiler option: {key}")]
    UnknownOption { key: String },
    #[error("Option '{key}' given invalid value '{value}'. {help}")]
    OptionValueError {
        key: String,
        value: String,
        help: String,
    },
    #[error("unknown macro {name:?}")]
    UnknownMacro { name: String },
    #[error("recursive macro {name:?}")]
    RecrusiveMacro { name: String },
    /// Recursive expansion for a const-expr
    #[error("recursive constant expansion {name:?}")]
    RecrusiveConstantExpansion { name: String },
    #[error("Invalid instruction '{0}': {1}")]
    InvalidInstruction(String, String),

    /// Useful for NYI features
    #[error("this compiler does not support the feature {feature}")]
    UnsupportedFeature { feature: String },
    #[error("unsupported extension type {name}")]
    UnsupportedExtensionType { name: String },

    // Parser errors
    /// nom syntax error
    #[error("syntax error {0}")]
    Syntax(#[from] ParseError),
    /// When converting tokens to statements, got unexpected block type
    #[error("unexpected block type; got {0} expected {1}")]
    BlockStackUnexpectedType(String, String),
    /// When converting tokens to statements, tried to pop empty block stack
    #[error("Too many .endif or .endmacro directives")]
    BlockStackEmpty,
    /// The parser has encountered an unexpected token (token, reason)
    #[error("Unexpected token '{0}': {1}")]
    UnexpectedToken(String, String),
    /// Parser expected an EoS token
    #[error("expected end of statement")]
    ExpectedEndOfStatement(),

    // Label errors
    #[error("invalid statement during label assignment {0}")]
    LabelAssignmentInvalidStatement(String),
    #[error("duplicate label name '{0}'")]
    DuplicateLabelName(String),

    /// Error when parsing a const-expression
    #[error("parsing const expression '{expression}': {error}")]
    ConstExprParse {
        expression: String,
        error: ParseError,
    },
    /// Unknown quick-const expression key
    #[error("unknown quick-const key in expression \"${0}:{1}\"")]
    UnknownQuickConstKey(String, String),
    /// Error when evaluating a const-expression
    #[error("Evaluating const expression: {0}")]
    ConstExprEval(#[from] bn_expression::EvalError),
    #[error(transparent)]
    #[cfg_attr(feature = "serialize", serde(serialize_with = "format_debug"))]
    IO(#[from] std::io::Error),
}
impl PassError {
    pub fn unknown_label<S: Into<String>>(label: S) -> Self {
        Self::UnknownLabel { name: label.into() }
    }

    pub fn with_source(self, source: &Source) -> CompileError {
        CompileError::PassError(source.clone().into(), self)
    }
    pub fn option_value_error(key: &str, value: &str, help: &str) -> Self {
        Self::OptionValueError {
            key: key.into(),
            value: value.into(),
            help: help.into(),
        }
    }
    pub fn from_eval_error(expr: &str, error: EvalError) -> Self {
        match error {
            EvalError::Parse(error) => Self::ConstExprParse {
                expression: expr.into(),
                error,
            },
            _ => Self::ConstExprEval(error),
        }
    }
    pub fn error_name(&self) -> &'static str {
        self.into()
    }
}
impl DynamicError for PassError {}

#[cfg(feature = "serialize")]
fn format_debug<T: Debug, S: serde::Serializer>(item: &T, ser: S) -> Result<S::Ok, S::Error> {
    ser.serialize_str(&format!("{:?}", item))
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct DisplayOption<T>(pub Option<T>);
impl<T: Display> fmt::Display for DisplayOption<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
compact_from! {
    impl[T] From[Option<T>] for DisplayOption<T> => |v| { Self(v) }
    impl[T] From[DisplayOption<T>] for Option<T> => |v| { v.0 }
}

mod dynamic_error {
    use std::error::Error as StdError;

    use downcast_rs::{impl_downcast, Downcast};

    use crate::compiler::format::*;

    pub trait DynamicErrorCommon: StdError + Downcast + FormatCli + 'static {
        fn upcast(self: Box<Self>) -> Box<dyn StdError + 'static>;
    }

    /// Auto-impl `upcast`
    impl<T: StdError + Downcast + 'static> DynamicErrorCommon for T {
        fn upcast(self: Box<Self>) -> Box<dyn StdError + 'static> {
            self
        }
    }

    /// An error trait with extra features to support type-casting
    /// and additional formatting options for error output.
    #[cfg(feature = "json")]
    pub trait DynamicError: DynamicErrorCommon + FormatJson {}
    #[cfg(not(feature = "json"))]
    pub trait DynamicError: DynamicErrorCommon {}

    impl_downcast!(DynamicError);

    // Automatically box anything implementing DynamicError
    impl<E: DynamicError> From<E> for Box<dyn DynamicError> {
        fn from(v: E) -> Box<dyn DynamicError> {
            Box::new(v)
        }
    }

    impl DynamicError for bn_expression::EvalError {}
    impl DynamicError for bn_expression::ParseError {}
}
