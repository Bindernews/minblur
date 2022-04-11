/// Name of "current instruction" variable
pub const VAR_COUNTER: &str = "counter";
/// Name of function that gives label destination (also quick-expression)
pub const FUNC_LABEL_DEST: &str = "label";
/// Function that returns 1 if a constant is defined
pub const FUNC_DEFINED: &str = "defined";
/// Constant prefix character
pub const CONST_PREFIX: char = '$';

pub const OPTION_LABEL_MODE: &str = "label_mode";

pub const SOURCE_AUTO_LABEL: &str = "<builtin:auto-label>";

/// Automatic label suffix used internally to manage duplicate label names
pub const LABEL_SUFFIX_START: char = '%';
