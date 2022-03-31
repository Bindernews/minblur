use bn_expression::enum_to_from_str;

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

/// Enum to help with converting things to and from strings.
///
/// There are macros for easy conversion of one enum to
/// another, so this should help reduce duplication.
pub enum ConstNames {
    Read,
    Write,
    Draw,
    Print,
    DrawFlush,
    PrintFlush,
    GetLink,
    Control,
    Radar,
    Sensor,
    Jump,

    Max,
    Min,
    Angle,
    Len,
    Noise,
    Abs,
    Log,
    Log10,
    Floor,
    Ceil,
    Sqrt,
    Rand,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
}

impl ConstNames {
    enum_to_from_str!(
        ConstNames; pub fn name(); pub fn from_name();
        {
            Self::Read => "read",
            Self::Write => "write",
            Self::Draw => "draw",
            Self::Print => "print",
            Self::DrawFlush => "drawflush",
            Self::PrintFlush => "printflush",
            Self::GetLink => "getlink",
            Self::Control => "control",
            Self::Radar => "radar",
            Self::Sensor => "sensor",
            Self::Jump => "jump",
            Self::Max => "max",
            Self::Min => "min",
            Self::Angle => "angle",
            Self::Len => "len",
            Self::Noise => "noise",
            Self::Abs => "abs",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Sqrt => "sqrt",
            Self::Rand => "rand",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
        }
    );
}
