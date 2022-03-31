use std::convert::TryInto;
use std::fmt;
use std::{convert::TryFrom, str::FromStr};

use bn_expression::basic::BasicOp;
use bn_expression::{AValue, Expression};

use crate::common::{
    macros::{each_ref, enum_variant_call},
    string_cache::*,
};
use crate::compiler::PassError;

/// Represents an instruction argument, or in general a compiler "value"
///
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum InstValue {
    EnumName(CaString),
    Value(AValue),
    Const(CaString),
    ConstExpr(Expression<BasicOp>),
    /// A "quick" constexpr in the form key:value, shorthand for certain functions
    QuickConstExpr(CaString, CaString),
}
impl InstValue {
    pub fn new_name<S: AsRef<str>>(name: S) -> Self {
        Self::Value(AValue::name(name))
    }
    pub fn new_num<N: Into<f64>>(value: N) -> Self {
        Self::Value(AValue::Num(value.into()))
    }
    pub fn new_expr(expr: Expression<BasicOp>) -> Self {
        Self::ConstExpr(expr)
    }
    pub fn new_quick(key: impl AsRef<str>, arg: impl AsRef<str>, cache: &StringCache) -> Self {
        Self::QuickConstExpr(cache.get(key.as_ref()), cache.get(arg.as_ref()))
    }

    /// Return true if this value can be converted to the given enum
    pub fn as_enum<E: FromStr>(&self) -> Option<E> {
        match self {
            Self::EnumName(v) => E::from_str(v.as_str()).ok(),
            Self::Value(AValue::Name(v)) => E::from_str(v).ok(),
            _ => None,
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::Value(AValue::String(s)) => Some(s),
            _ => None,
        }
    }

    /// Returns [`Some`] if this value is anything that can be turned into a string directly,
    /// [`None`] if this isn't a string, name, etc.
    pub fn as_any_string(&self) -> Option<&str> {
        match self {
            Self::EnumName(s) => Some(s.as_str()),
            Self::Value(AValue::Name(s)) => Some(s),
            Self::Value(AValue::String(s)) => Some(s),
            _ => None,
        }
    }

    pub fn into_value(self) -> Option<AValue> {
        match self {
            Self::Value(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_const_expr(&self) -> bool {
        matches!(self, Self::Const(_) | Self::QuickConstExpr(_, _))
    }

    /// Returns true iff this value is a name (NOT an enum name)
    pub fn is_name(&self) -> bool {
        matches!(self, Self::Value(AValue::Name(_)))
    }

    pub fn is_concrete(&self) -> bool {
        matches!(self, Self::Value(_) | Self::EnumName(_))
    }

    /// Returns true iff this value is a number
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Value(AValue::Num(_)))
    }
    /// Returns true iff this value is a quoted string
    pub fn is_string(&self) -> bool {
        matches!(self, Self::Value(AValue::String(_)))
    }
}
impl From<AValue> for InstValue {
    fn from(v: AValue) -> Self {
        Self::Value(v)
    }
}
impl fmt::Display for InstValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Self::EnumName(v) => f.write_str(v.as_str()),
            Self::Const(v) => write!(f, "${}", &v),
            Self::ConstExpr(v) => write!(f, "${{{}}}", &v),
            Self::Value(v) => write!(f, "{}", v),
            Self::QuickConstExpr(k, v) => write!(f, "${}:{}", k, v),
        }
    }
}

pub trait ResolveValues {
    fn resolve_values<E>(&mut self, resolver: impl ValueResolver<E>) -> Result<(), E>;
}
pub trait ValueResolver<E> {
    fn resolve(&mut self, value: InstValue) -> Result<InstValue, E>;
}

impl ResolveValues for &mut [InstValue] {
    fn resolve_values<E>(&mut self, mut re: impl ValueResolver<E>) -> Result<(), E> {
        for v in self.iter_mut() {
            *v = re.resolve(v.clone())?;
        }
        Ok(())
    }
}

impl<F, E> ValueResolver<E> for F
where
    F: FnMut(InstValue) -> Result<InstValue, E>,
{
    fn resolve(&mut self, value: InstValue) -> Result<InstValue, E> {
        self(value)
    }
}

pub trait IInstruction: ResolveValues + fmt::Debug + std::hash::Hash + PartialEq {
    fn check_output(&self) -> Result<(), PassError>;
    fn result(&self) -> Option<&InstValue> {
        None
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        None
    }
}

/// Helper macro to implement common things for instruction variants.
///
/// ## "derive" blocks
/// - `FromTuple`:
///     Helper to implement Into<(...)> and From<(...)> to make it easy to
///     convert a tuple into the given instruction.
/// - `ToFromSliceBlock`:
///     Helper to convert the type to and from [InstValue; N], [&InstValue; N], [&mut InstValue; N].
///     Eventually these may replace the tuple code entirely.
/// - `ToFromSliceFields`:
///     Same as `ToFromSliceBlock` but auto-generates code for field accesses, DRY
/// - `ResolveValues`:
///     Implement the `ResolveValues` trait, uses `ToFromSlice*`.
/// - `Accessors`:
///     Intended for structs that store args as a fixed-size array, just some syntax
///     sugar for writing `pub fn foo() -> &<type> {...}` and `pub fn foo_mut() -> &mut <type> {...}`
/// - `CheckOutput`:
///     Each `Instruction` variant MUST have a `CheckOutput` method, this makes it easy to
///     ensure they all have the same signature.
///
/// See the various instruction structs for usage examples.
macro_rules! impl_instruction_variant {
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive ToFromSliceArray (
            $ar_name:ident;
            [$($a_ix:ident),+]
        ) $($tail:tt)*
    ) => {
        impl $dest_type {
            pub fn into_array($self) -> [$slice_ty; $slice_len] { $self.$ar_name }
            pub fn as_array(& $self) -> [& $slice_ty; $slice_len] {
                each_ref!(& $self.$ar_name; $($a_ix),+)
            }
            pub fn as_array_mut(&mut $self) -> [&mut $slice_ty; $slice_len] {
                each_ref!(&mut $self.$ar_name; $($a_ix),+)
            }
        }
        impl From<[$slice_ty; $slice_len]> for $dest_type {
            #[allow(clippy::redundant_field_names)]
            fn from($args: [$slice_ty; $slice_len]) -> Self { Self { $ar_name: $args } }
        }
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len],
            derive IntoSlice () $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive ToFromSliceFields (
            fields { $($name:ident),* }
        ) $($tail:tt)*
    ) => {
        impl $dest_type {
            pub fn into_array($self) -> [$slice_ty; $slice_len] {
                [ $( $self.$name ),* ]
            }
            pub fn as_array(& $self) -> [& $slice_ty; $slice_len] {
                [ $( & $self.$name ),* ]
            }
            pub fn as_array_mut(&mut $self) -> [&mut $slice_ty; $slice_len] {
                [ $( &mut $self.$name ),* ]
            }
        }
        impl From<[$slice_ty; $slice_len]> for $dest_type {
            fn from($args: [$slice_ty; $slice_len]) -> Self {
                let [ $( $name ),* ] = $args;
                Self { $( $name ),* }
            }
        }
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len],
            derive IntoSlice () $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive IntoSlice () $($tail:tt)*
    ) => {
        impl From<$dest_type> for [$slice_ty; $slice_len] {
            fn from(v: $dest_type) -> [$slice_ty; $slice_len] { v.into_array() }
        }
        impl<'z> From<&'z $dest_type> for [&'z $slice_ty; $slice_len] {
            fn from(v: &'z $dest_type) -> [&'z $slice_ty; $slice_len] { v.as_array() }
        }
        impl<'z> From<&'z mut $dest_type> for [&'z mut $slice_ty; $slice_len] {
            fn from(v: &'z mut $dest_type) -> [&'z mut $slice_ty; $slice_len] { v.as_array_mut() }
        }
        impl TryFrom<Vec<$slice_ty>> for $dest_type {
            type Error = Vec<$slice_ty>;
            fn try_from($args: Vec<$slice_ty>) -> Result<$dest_type, Self::Error> {
                let array: [$slice_ty; $slice_len] = $args.try_into()?;
                Ok(Self::from(array))
            }
        }
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len], $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive ResolveValues ()
        $($tail:tt)*
    ) => {
        impl ResolveValues for $dest_type {
            #[allow(clippy::reversed_empty_ranges)]
            fn resolve_values<E>(&mut $self, mut re: impl ValueResolver<E>) -> Result<(), E> {
                let value_refs: [&mut $slice_ty; $slice_len] = $self.into();
                for i in 0..$slice_len {
                    *value_refs[i] = re.resolve(value_refs[i].clone())?;
                }
                Ok(())
            }
        }
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len], $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive Display (
            tag: $instr_name:literal,
        ) $($tail:tt)*
    ) => {
        impl std::fmt::Display for $dest_type {
            fn fmt(& $self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
                write!(f, "{}", $instr_name)?;
                for arg in $self.as_array() {
                    write!(f, " {}", arg)?;
                }
                Ok(())
            }
        }
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len], $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
        derive Accessors ( $($tokens:tt)+ )
        $($tail:tt)*
    ) => {
        $crate::common::macros::impl_accessors!($dest_type; $self; $($tokens)+);
        impl_instruction_variant!($dest_type, $args, $self, [$slice_ty; $slice_len], $($tail)*);
    };
    (
        $dest_type:ty, $args:ident, $self:ident, [$slice_ty:ty; $slice_len:literal],
    ) => {
    };
}

macro_rules! value_check {
    // Alternate "multi-check" function which also allows renaming things
    (
        $self:ident; check_many_fn $inst_value:ty => $($checks:tt),+
    ) => {
        |values: &[(&str, &$inst_value)]| -> Result<(), PassError> {
            for (name, val) in values {
                if !($( value_check!(@check val; $checks) ||)+ false) {
                    return Err( value_check!($self; error name => $($checks),+) );
                }
            }
            Ok(())
        }
    };
    // Main "check" function
    (
        $self:ident; check $name:literal $field:expr => $($checks:tt),+
        $(; $($tail:tt)*)?
    ) => {
        if !($( value_check!(@check $field; $checks) ||)+ false) {
            return Err( value_check!($self; error $name => $($checks),+) );
        }
        value_check!($self; $($($tail)*)?)
    };
    // Main error function
    (
        $self:ident; error $field:expr => $($checks:tt),+
    ) => {
        PassError::InvalidInstruction(
            format!("{}", $self),
            format!("field {} must be one of {:?}", $field, [$( value_check!(@error $self; $checks) ),+]),
        )
    };

    (@check $field:expr; { is_name() }) => { $field.is_name() };
    (@error $field:expr; { is_name() }) => { "name" };
    (@check $field:expr; { is_enum<$enum_type:ty>() }) => { $field.as_enum::<$enum_type>().is_some() };
    (@error $field:expr; { is_enum<$enum_type:ty>() }) => { stringify!(enum $enum_type) };
    (@check $field:expr; { is_number() }) => { $field.is_number() };
    (@error $field:expr; { is_number() }) => { "number" };
    (@check $field:expr; { is_string() }) => { $field.is_string() };
    (@error $field:expr; { is_string() }) => { "string" };
    (@check $field:expr; { x }) => { false };
    (@check $field:expr; { x }) => { "" };
    // @check and @error base cases
    (@check $field:expr; {}) => { false };
    (@error $field:expr; {}) => {};
    // Main base case
    ($self:ident;) => {};
}

// Convnience macro for transposing arrays into tuples
macro_rules! zip_it {
    ([$($a:expr),+], [$($b:expr),+] $(,)?) => {
        [$( ($a, $b) ),+]
    };
}

macro_rules! impl_instruction_enum {
    (
        $mtype:ty
    ) => {
        impl $mtype {
            pub fn into_value(self, scache: &StringCache) -> InstValue {
                InstValue::EnumName(scache.get(<&str>::from(self)))
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Instruction {
    Read(Box<InstructionRead>),
    Write(Box<InstructionWrite>),
    Draw(Box<InstructionDraw>),
    Print(InstructionPrint),
    DrawFlush(InstructionDrawFlush),
    PrintFlush(InstructionPrintFlush),
    GetLink(InstructionGetLink),
    Control(Box<InstructionControl>),
    Radar(Box<InstructionRadar>),
    Sensor(Box<InstructionSensor>),
    Set(InstructionSet),
    Op(Box<InstructionOp>),
    End(InstructionEnd),
    Jump(Box<InstructionJump>),
    UnitBind(InstructionUnitBind),
    UnitControl(Box<InstructionUnitControl>),
    UnitRadar(Box<InstructionUnitRadar>),
    UnitLocate(Box<InstructionUnitLocate>),
}
crate::enum_from_variants! {
    Instruction;
    Read(Box<InstructionRead>),
    Write(Box<InstructionWrite>),
    Draw(Box<InstructionDraw>),
    Print(InstructionPrint),
    DrawFlush(InstructionDrawFlush),
    PrintFlush(InstructionPrintFlush),
    GetLink(InstructionGetLink),
    Control(Box<InstructionControl>),
    Radar(Box<InstructionRadar>),
    Sensor(Box<InstructionSensor>),
    Set(InstructionSet),
    Op(Box<InstructionOp>),
    End(InstructionEnd),
    Jump(Box<InstructionJump>),
    UnitBind(InstructionUnitBind),
    UnitControl(Box<InstructionUnitControl>),
    UnitRadar(Box<InstructionUnitRadar>),
    UnitLocate(Box<InstructionUnitLocate>),
}
crate::enum_from_variants! {
    Instruction;
    Read(InstructionRead) => |v| Self::Read(Box::new(v)),
    Write(InstructionWrite) => |v| Self::Write(Box::new(v)),
    Draw(InstructionDraw) => |v| Self::Draw(Box::new(v)),
    Control(InstructionControl) => |v| Self::Control(Box::new(v)),
    Radar(InstructionRadar) => |v| Self::Radar(Box::new(v)),
    Sensor(InstructionSensor) => |v| Self::Sensor(Box::new(v)),
    Op(InstructionOp) => |v| Self::Op(Box::new(v)),
    Jump(InstructionJump) => |v| Self::Jump(Box::new(v)),
    UnitControl(InstructionUnitControl) => |v| Self::UnitControl(Box::new(v)),
    UnitRadar(InstructionUnitRadar) => |v| Self::UnitRadar(Box::new(v)),
    UnitLocate(InstructionUnitLocate) => |v| Self::UnitLocate(Box::new(v)),
}

// Call the same function on different variants of the enum
macro_rules! instruction_variant_call {
    (
        $self:ident; |$arg:ident| => $code:block
    ) => {
        enum_variant_call!(match $self; {
                Instruction::Read($arg), Instruction::Write($arg),
                Instruction::Draw($arg), Instruction::Print($arg), Instruction::DrawFlush($arg),
                Instruction::PrintFlush($arg), Instruction::GetLink($arg),
                Instruction::Control($arg), Instruction::Radar($arg), Instruction::Sensor($arg),
                Instruction::Set($arg), Instruction::Op($arg), Instruction::End($arg),
                Instruction::Jump($arg), Instruction::UnitBind($arg), Instruction::UnitControl($arg),
                Instruction::UnitRadar($arg), Instruction::UnitLocate($arg),
            }
            ($arg) => $code
        )
    }
}

impl Instruction {
    /// Checks the syntax of the instruction to help prevent typos and other simple errors.
    pub fn check_output(&self) -> Result<(), PassError> {
        instruction_variant_call!(self; |v| => { v.check_output() })
    }

    pub fn generate(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        instruction_variant_call!(self; |v| => { fmt::Display::fmt(v, f) })
    }

    pub fn result(&mut self) -> Option<&InstValue> {
        instruction_variant_call!(self; |v| => { v.result() })
    }
    pub fn result_mut(&mut self) -> Option<&mut InstValue> {
        instruction_variant_call!(self; |v| => { v.result_mut() })
    }

    pub fn kind(&self) -> InstructionKind {
        instruction_variant_call!(self; |v| => { InstructionKind::from(v) })
    }

    pub fn try_from_kind(
        kind: InstructionKind,
        args: Vec<InstValue>,
    ) -> Result<Self, Vec<InstValue>> {
        kind.create_with_args(args)
    }
}
impl ResolveValues for Instruction {
    fn resolve_values<E>(&mut self, re: impl ValueResolver<E>) -> Result<(), E> {
        instruction_variant_call!(self; |v| => { v.resolve_values(re) })
    }
}
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionRead {
    pub result: InstValue,
    pub cell: InstValue,
    pub index: InstValue,
}
impl_instruction_variant!(InstructionRead, args, self, [InstValue; 3],
    derive ToFromSliceFields (
        fields { result, cell, index }
    )
    derive ResolveValues ()
    derive Display (
        tag: "read",
    )
);
impl IInstruction for InstructionRead {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "result" self.result => {is_name()};
            check "cell" self.cell => {is_name()};
            check "index" self.index => {is_name()},{is_number()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.result)
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.result)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionWrite {
    pub value: InstValue,
    pub cell: InstValue,
    pub index: InstValue,
}
impl_instruction_variant!(InstructionWrite, args, self, [InstValue; 3],
    derive ToFromSliceFields (
        fields { value, cell, index }
    )
    derive ResolveValues ()
    derive Display (
        tag: "write",
    )
);
impl IInstruction for InstructionWrite {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "value" self.value => {is_name()}, {is_number()};
            check "cell" self.cell => {is_name()};
            check "index" self.index => {is_name()},{is_number()};
        );
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionPrint {
    pub msg: InstValue,
}
impl_instruction_variant!(InstructionPrint, args, self, [InstValue; 1],
    derive ToFromSliceFields (
        fields { msg }
    )
    derive ResolveValues ()
    derive Display (
        tag: "print",
    )
);
impl IInstruction for InstructionPrint {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "msg" self.msg => {is_name()}, {is_string()}, {is_number()};
        );
        Ok(())
    }
}

/// The "draw" instruction
///
/// Examples
/// ```text
/// draw clear 1 2 3 0 0 0
/// draw color 1 2 3 255 0 0
/// draw stroke 1 0 0 0 0 0
/// draw line 1 2 3 4 0 0
/// draw rect 1 2 3 4 0 0
/// draw lineRect 1 2 3 4 0 0
/// draw poly 1 2 3 4 5 0
/// draw linePoly 1 2 3 4 5 0
/// draw triangle 1 2 3 4 5 6
/// draw image 1 2 @copper 32 3 6
/// ```
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionDraw {
    pub args: [InstValue; 7],
}
impl_instruction_variant!(InstructionDraw, args, self, [InstValue; 7],
    derive Accessors (
        /// Reference to the draw mode (enum: DrawMode)
        fn mode() -> InstValue { self.args[0] }
        /// Mutable refernce to the `DrawMode` enum
        mut fn mode_mut() ;
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5,a6])
    derive ResolveValues ()
    derive Display (
        tag: "draw",
    )
);
impl IInstruction for InstructionDraw {
    fn check_output(&self) -> Result<(), PassError> {
        let mode = self.mode().as_enum::<DrawMode>();
        if mode.is_none() {
            return Err(value_check!(self; error "mode" => {is_enum<DrawMode>()}));
        }
        let check_num_name =
            value_check!(self; check_many_fn InstValue => {is_name()}, {is_number()});
        let [_, a1, a2, a3, a4, a5, a6] = &self.args;

        match mode.unwrap() {
            DrawMode::Clear => {
                check_num_name(&zip_it!(["r", "g", "b"], [a1, a2, a3],))?;
            }
            DrawMode::Color => {
                check_num_name(&zip_it!(["r", "g", "b", "a"], [a1, a2, a3, a4],))?;
            }
            DrawMode::Stroke => {
                check_num_name(&[("width", a1)])?;
            }
            DrawMode::Line => {
                check_num_name(&zip_it!(["x1", "y1", "x2", "y2"], [a1, a2, a3, a4],))?;
            }
            DrawMode::Rect | DrawMode::LineRect => {
                check_num_name(&zip_it!(["x", "y", "w", "h"], [a1, a2, a3, a4],))?;
            }
            DrawMode::Poly | DrawMode::LinePoly => {
                check_num_name(&zip_it!(
                    ["x", "y", "sides", "radius", "rotation"],
                    [a1, a2, a3, a4, a5],
                ))?;
            }
            DrawMode::Triangle => {
                check_num_name(&zip_it!(
                    ["x1", "y1", "x2", "y2", "x3", "y3"],
                    [a1, a2, a3, a4, a5, a6],
                ))?;
            }
            DrawMode::Image => {
                check_num_name(&zip_it!(["x", "y", "size", "rotation"], [a1, a2, a4, a5],))?;
                value_check!(self; check "image" a3 => {is_name()});
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionDrawFlush {
    pub to: InstValue,
}
impl_instruction_variant!(InstructionDrawFlush, args, self, [InstValue; 1],
    derive ToFromSliceFields (
        fields { to }
    )
    derive ResolveValues ()
    derive Display (
        tag: "drawflush",
    )
);
impl IInstruction for InstructionDrawFlush {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self; check "to" self.to => {is_name()});
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionPrintFlush {
    pub to: InstValue,
}
impl_instruction_variant!(InstructionPrintFlush, args, self, [InstValue; 1],
    derive ToFromSliceFields (
        fields { to }
    )
    derive ResolveValues ()
    derive Display (
        tag: "printflush",
    )
);
impl IInstruction for InstructionPrintFlush {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self; check "to" self.to => {is_name()});
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionGetLink {
    pub result: InstValue,
    pub index: InstValue,
}
impl_instruction_variant!(InstructionGetLink, args, self, [InstValue; 2],
    derive ToFromSliceFields (
        fields { result, index }
    )
    derive ResolveValues ()
    derive Display (
        tag: "getlink",
    )
);
impl IInstruction for InstructionGetLink {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "result" self.result => {is_name()};
            check "index" self.index => {is_name()}, {is_number()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.result)
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.result)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionControl {
    pub args: [InstValue; 6],
}
impl_instruction_variant!(InstructionControl, args, self, [InstValue; 6],
    derive Accessors (
        /// Reference to the `ControlMode`
        fn mode() -> InstValue { self.args[0] }
        /// Mutable reference to the `ControlMode`
        mut fn mode_mut();
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5])
    derive ResolveValues ()
    derive Display (
        tag: "control",
    )
);
impl IInstruction for InstructionControl {
    fn check_output(&self) -> Result<(), PassError> {
        let [mode, what, a2, a3, a4, _] = &self.args;
        let mode = mode.as_enum::<ControlMode>();
        if mode.is_none() {
            return Err(value_check!(self; error "mode" => {is_enum<ControlMode>()}));
        }
        value_check!(self; check "what" what => {is_name()});
        let check_num_name =
            value_check!(self; check_many_fn InstValue => {is_name()}, {is_number()});

        match mode.unwrap() {
            ControlMode::Enabled => {
                check_num_name(&[("enabled", a2)])?;
            }
            ControlMode::Shoot => {
                check_num_name(&[("x", a2), ("y", a3), ("shoot", a4)])?;
            }
            ControlMode::ShootP => {
                check_num_name(&[("unit", a2), ("shoot", a3)])?;
            }
            ControlMode::Configure => {
                check_num_name(&[("configure", a2)])?;
            }
            ControlMode::Color => {
                check_num_name(&[("r", a2), ("g", a3), ("b", a4)])?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionRadar {
    pub args: [InstValue; 7],
}
impl_instruction_variant!(InstructionRadar, args, self, [InstValue; 7],
    derive Accessors (
        /// RadarTarget
        fn target0() -> InstValue { self.args[0] }
        mut fn target0_mut();
        /// RadarTarget
        fn target1() -> InstValue { self.args[1] }
        mut fn target1_mut();
        /// RadarTarget
        fn target2() -> InstValue { self.args[2] }
        mut fn target2_mut();
        /// RadarSort
        fn sort() -> InstValue { self.args[3] }
        mut fn sort_mut();
        /// Radar from what?
        fn source() -> InstValue { self.args[4] }
        mut fn source_mut();
        /// 1 or 0 for ascending or descending
        fn order() -> InstValue { self.args[5] }
        mut fn order_mut();
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5,a6])
    derive ResolveValues ()
    derive Display (
        tag: "radar",
    )
);
impl IInstruction for InstructionRadar {
    fn check_output(&self) -> Result<(), PassError> {
        let check_targets = value_check!(self; check_many_fn InstValue => {is_enum<RadarTarget>()});
        check_targets(&zip_it!(
            ["target0", "target1", "target2"],
            [self.target0(), self.target1(), self.target2()],
        ))?;
        value_check!(self;
            check "sort" self.sort() => {is_enum<RadarSort>()};
            check "order" self.order() => {is_name()}, {is_number()};
            check "result" self.result().unwrap() => {is_name()};
            check "source" self.source() => {is_name()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.args[6])
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.args[6])
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionSensor {
    pub result: InstValue,
    pub obj: InstValue,
    pub what: InstValue,
}
impl_instruction_variant!(InstructionSensor, args, self, [InstValue; 3],
    derive ToFromSliceFields (
        fields { result, obj, what }
    )
    derive ResolveValues ()
    derive Display (
        tag: "sensor",
    )
);
impl IInstruction for InstructionSensor {
    fn check_output(&self) -> Result<(), PassError> {
        let check_name = value_check!(self; check_many_fn InstValue => {is_name()});
        check_name(&zip_it!(
            ["result", "obj", "what"],
            [&self.result, &self.obj, &self.what],
        ))
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.result)
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.result)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionSet {
    pub result: InstValue,
    pub value: InstValue,
}
impl_instruction_variant!(InstructionSet, args, self, [InstValue; 2],
    derive ToFromSliceFields (
        fields { result, value }
    )
    derive ResolveValues ()
    derive Display (
        tag: "set",
    )
);
impl IInstruction for InstructionSet {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "result" self.result => {is_name()};
            check "value" self.value => {is_name()}, {is_number()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.result)
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.result)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionOp {
    /// OpSymbol
    pub op: InstValue,
    pub result: InstValue,
    pub left: InstValue,
    pub right: InstValue,
}
impl_instruction_variant!(InstructionOp, args, self, [InstValue; 4],
    derive ToFromSliceFields (
        fields { op, result, left, right }
    )
    derive ResolveValues ()
    derive Display (
        tag: "op",
    )
);
impl IInstruction for InstructionOp {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "op" self.op => {is_enum<OpSymbol>()};
            check "result" self.result => {is_name()};
            check "left" self.left => {is_name()}, {is_number()};
            check "right" self.right => {is_name()}, {is_number()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.result)
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.result)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionEnd {}
impl_instruction_variant!(InstructionEnd, args, self, [InstValue; 0],
    derive ToFromSliceFields (
        fields {}
    )
    derive ResolveValues ()
    derive Display (
        tag: "end",
    )
);
impl IInstruction for InstructionEnd {
    fn check_output(&self) -> Result<(), PassError> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionJump {
    pub dest: InstValue,
    /// JumpSymbol
    pub op: InstValue,
    pub left: InstValue,
    pub right: InstValue,
}
impl_instruction_variant!(InstructionJump, args, self, [InstValue; 4],
    derive ToFromSliceFields (
        fields { dest, op, left, right }
    )
    derive ResolveValues ()
    derive Display (
        tag: "jump",
    )
);
impl IInstruction for InstructionJump {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "op" self.op => {is_enum<JumpSymbol>()};
            check "dest" self.dest => {is_number()};
            check "left" self.left => {is_name()}, {is_number()};
            check "right" self.right => {is_name()}, {is_number()};
        );
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionUnitBind {
    pub what: InstValue,
}
impl_instruction_variant!(InstructionUnitBind, args, self, [InstValue; 1],
    derive ToFromSliceFields (
        fields { what }
    )
    derive ResolveValues ()
    derive Display (
        tag: "ubind",
    )
);
impl IInstruction for InstructionUnitBind {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "what" self.what => {is_name()};
        );
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionUnitControl {
    pub args: [InstValue; 6],
}
impl_instruction_variant!(InstructionUnitControl, args, self, [InstValue; 6],
    derive Accessors (
        /// Get `UnitControlMode`
        fn mode() -> InstValue { self.args[0] }
        mut fn mode_mut();
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5])
    derive ResolveValues ()
    derive Display (
        tag: "ucontrol",
    )
);
impl IInstruction for InstructionUnitControl {
    fn check_output(&self) -> Result<(), PassError> {
        let [mode, a1, a2, a3, a4, a5] = &self.args;
        let mode = mode.as_enum::<UnitControlMode>();
        if mode.is_none() {
            return Err(value_check!(self; error "mode" => {is_enum<UnitControlMode>()}));
        }
        let check_num_name =
            value_check!(self; check_many_fn InstValue => {is_name()}, {is_number()});

        let check_n = |names: &[&str]| -> Result<(), PassError> {
            let values = [a1, a2, a3, a4, a5];
            for i in 0..names.len() {
                check_num_name(&[(names[i], values[i])])?;
            }
            Ok(())
        };

        match mode.unwrap() {
            UnitControlMode::Idle | UnitControlMode::Stop | UnitControlMode::Pathfind => (),
            UnitControlMode::Move => check_n(&["x", "y"])?,
            UnitControlMode::Approach => check_n(&["x", "y", "radius"])?,
            UnitControlMode::Boost => check_n(&["enable"])?,
            UnitControlMode::Target => check_n(&["x", "y", "shoot"])?,
            UnitControlMode::TargetP => check_n(&["unit", "shoot"])?,
            UnitControlMode::ItemDrop => check_n(&["to", "amount"])?,
            UnitControlMode::ItemTake => check_n(&["from", "item", "amount"])?,
            UnitControlMode::PayDrop => (),
            UnitControlMode::PayTake => check_n(&["takeUnits"])?,
            UnitControlMode::Mine => check_n(&["x", "y"])?,
            UnitControlMode::Flag => check_n(&["flag"])?,
            UnitControlMode::Build => check_n(&["x", "y", "block", "rotation", "config"])?,
            UnitControlMode::GetBlock => check_n(&["x", "y", "type", "building"])?,
            UnitControlMode::Within => check_n(&["x", "y", "radius", "result"])?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionUnitRadar {
    pub args: [InstValue; 7],
}
impl_instruction_variant!(InstructionUnitRadar, args, self, [InstValue; 7],
    derive Accessors (
        /// RadarTarget
        fn target0() -> InstValue { self.args[0] }
        mut fn target0_mut();
        /// RadarTarget
        fn target1() -> InstValue { self.args[1] }
        mut fn target1_mut();
        /// RadarTarget
        fn target2() -> InstValue { self.args[2] }
        mut fn target2_mut();
        /// RadarSort
        fn sort() -> InstValue { self.args[3] }
        mut fn sort_mut();
        /// unknown value
        fn unk1() -> InstValue { self.args[4] }
        mut fn unk1_mut();
        /// 1 or 0 for ascending or descending
        fn order() -> InstValue { self.args[5] }
        mut fn order_mut();
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5,a6])
    derive ResolveValues ()
    derive Display (
        tag: "uradar",
    )
);
impl IInstruction for InstructionUnitRadar {
    fn check_output(&self) -> Result<(), PassError> {
        let check_targets = value_check!(self; check_many_fn InstValue => {is_enum<RadarTarget>()});
        let [tgt0, tgt1, tgt2, _, _, _, _] = &self.args;
        check_targets(&zip_it!(
            ["target0", "target1", "target2"],
            [tgt0, tgt1, tgt2],
        ))?;
        value_check!(self;
            check "sort" self.sort() => {is_enum<RadarSort>()};
            check "order" self.order() => {is_name()}, {is_number()};
            check "result" self.result().unwrap() => {is_name()};
        );
        Ok(())
    }
    fn result(&self) -> Option<&InstValue> {
        Some(&self.args[6])
    }
    fn result_mut(&mut self) -> Option<&mut InstValue> {
        Some(&mut self.args[6])
    }
}

/// The ulocate instruction
#[derive(Debug, Clone, PartialEq, Hash)]
pub struct InstructionUnitLocate {
    pub args: [InstValue; 8],
}
impl_instruction_variant!(InstructionUnitLocate, args, self, [InstValue; 8],
    derive Accessors (
        /// enum LocateKind
        fn kind() -> InstValue { self.args[0] }
        mut fn kind_mut();
        /// The building group, default to core if unused (enum BuildingGroup)
        fn group() -> InstValue { self.args[1] }
        mut fn group_mut();
        /// boolean if we're locating a building, undefined otherwise
        fn enemy() -> InstValue { self.args[2] }
        mut fn enemy_mut();
        /// Ore type if we're locating ore
        fn ore() -> InstValue { self.args[3] }
        mut fn ore_mut();
        /// Output: located X
        fn out_x() -> InstValue { self.args[4] }
        mut fn out_x_mut();
        /// Output: located Y
        fn out_y() -> InstValue { self.args[5] }
        mut fn out_y_mut();
        /// Output: boolean, if found or not
        fn found() -> InstValue { self.args[6] }
        mut fn found_mut();
        /// Output: building type if searching for building
        fn building() -> InstValue { self.args[7] }
        mut fn building_mut();
    )
    derive ToFromSliceArray (args; [a0,a1,a2,a3,a4,a5,a6,a7])
    derive ResolveValues ()
    derive Display (
        tag: "ulocate",
    )
);
impl IInstruction for InstructionUnitLocate {
    fn check_output(&self) -> Result<(), PassError> {
        value_check!(self;
            check "kind" self.kind() => {is_enum<LocateKind>()};
            check "group" self.group() => {is_enum<BuildingGroup>()};
            // TODO check for enemy based on group
        );
        let check_name = value_check!(self; check_many_fn InstValue => {is_name()});
        check_name(&zip_it!(
            ["ore", "out_x", "out_y", "found", "building"],
            [
                self.ore(),
                self.out_x(),
                self.out_y(),
                self.found(),
                self.building()
            ],
        ))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum InstructionKind {
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
    Set,
    Op,
    End,
    Jump,
    UnitBind,
    UnitControl,
    UnitRadar,
    UnitLocate,
}
crate::build_enum_match_alt!{
    instruction_kind_match;
    InstructionKind;
    name => {
        Read: "read",
        Write: "write",
        Draw: "draw",
        Print: "print",
        DrawFlush: "drawflush",
        PrintFlush: "printflush",
        GetLink: "getlink",
        Control: "control",
        Radar: "radar",
        Sensor: "sensor",
        Set: "set",
        Op: "op",
        End: "end",
        Jump: "jump",
        UnitBind: "ubind",
        UnitControl: "ucontrol",
        UnitRadar: "uradar",
        UnitLocate: "ulocate",
    }
    instruction => {
        Read: InstructionRead,
        Write: InstructionWrite,
        Draw: InstructionDraw,
        Print: InstructionPrint,
        DrawFlush: InstructionDrawFlush,
        PrintFlush: InstructionPrintFlush,
        GetLink: InstructionGetLink,
        Control: InstructionControl,
        Radar: InstructionRadar,
        Sensor: InstructionSensor,
        Set: InstructionSet,
        Op: InstructionOp,
        End: InstructionEnd,
        Jump: InstructionJump,
        UnitBind: InstructionUnitBind,
        UnitControl: InstructionUnitControl,
        UnitRadar: InstructionUnitRadar,
        UnitLocate: InstructionUnitLocate,
    }
}
impl InstructionKind {
    pub fn create(&self) -> Instruction {
        let mut args = Vec::new();
        for _ in 0..self.arg_count() {
            args.push(AValue::Null.into());
        }
        self.create_with_args(args).unwrap()
    }

    pub fn create_with_args(&self, args: Vec<InstValue>) -> Result<Instruction, Vec<InstValue>> {
        macro_rules! callback {
            ($_k:ident; $ty:path) => { <$ty>::try_from(args).map(Into::into) };
        }
        instruction_kind_match!(instruction; match self into with callback)
    }

    pub fn arg_count(&self) -> usize {
        match self {
            Self::Read => 3,
            Self::Write => 3,
            Self::Draw => 7,
            Self::Print => 1,
            Self::DrawFlush => 1,
            Self::PrintFlush => 1,
            Self::GetLink => 2,
            Self::Control => 6,
            Self::Radar => 7,
            Self::Sensor => 3,
            Self::Set => 2,
            Self::Op => 4,
            Self::End => 0,
            Self::Jump => 4,
            Self::UnitBind => 1,
            Self::UnitControl => 6,
            Self::UnitRadar => 7,
            Self::UnitLocate => 8,
        }
    }

    pub fn name(&self) -> &'static str {
        instruction_kind_match!(name; match self into)
    }

    pub fn from_name(name: &str) -> Option<Self> {
        instruction_kind_match!(name; match name from)
    }
}

// Generate From<&T> impls to convert the various instruction types
// to InstructionKind
macro_rules! ik_cb_from {
    ( [$([$k:ident; $v:ident])*] ) => {
        $(
            impl From<& $v> for InstructionKind {
                fn from(_: & $v) -> Self { <Self>::$k }
            }
            impl From<& Box<$v>> for InstructionKind {
                fn from(_: & Box<$v>) -> Self { <Self>::$k }
            }
        )*
    };
}
instruction_kind_match!{instruction; pairs with ik_cb_from}




#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum OpSymbol {
    #[strum(to_string = "add", serialize = "+")]
    Add,
    #[strum(to_string = "sub", serialize = "-")]
    Sub,
    #[strum(to_string = "mul")]
    Mul,
    #[strum(to_string = "div")]
    Div,
    #[strum(to_string = "idiv")]
    IDiv,
    #[strum(to_string = "mod")]
    Mod,
    #[strum(to_string = "pow")]
    Pow,
    #[strum(to_string = "land")]
    LAnd,
    #[strum(to_string = "equal")]
    Equal,
    #[strum(to_string = "notEqual")]
    NotEqual,
    #[strum(to_string = "lessThan")]
    LessThan,
    #[strum(to_string = "lessThanEq")]
    LessThanEq,
    #[strum(to_string = "greaterThan")]
    GreaterThan,
    #[strum(to_string = "greaterThanEq")]
    GreaterThanEq,
    #[strum(to_string = "strictEqual")]
    StrictEqual,
    #[strum(to_string = "shl", serialize = "<<")]
    Shl,
    #[strum(to_string = "shr", serialize = ">>")]
    Shr,
    #[strum(to_string = "or")]
    BOr,
    #[strum(to_string = "and")]
    BAnd,
    #[strum(to_string = "xor")]
    BXor,
    #[strum(to_string = "flip")]
    Flip,
    #[strum(to_string = "max")]
    Max,
    #[strum(to_string = "min")]
    Min,
    #[strum(to_string = "angle")]
    Angle,
    #[strum(to_string = "len")]
    Len,
    #[strum(to_string = "noise")]
    Noise,
    #[strum(to_string = "abs")]
    Abs,
    #[strum(to_string = "log")]
    Log,
    #[strum(to_string = "log10")]
    Log10,
    #[strum(to_string = "floor")]
    Floor,
    #[strum(to_string = "ceil")]
    Ceil,
    #[strum(to_string = "sqrt")]
    Sqrt,
    #[strum(to_string = "rand")]
    Rand,
    #[strum(to_string = "sin")]
    Sin,
    #[strum(to_string = "cos")]
    Cos,
    #[strum(to_string = "tan")]
    Tan,
    #[strum(to_string = "asin")]
    Asin,
    #[strum(to_string = "acos")]
    Acos,
    #[strum(to_string = "atan")]
    Atan,
}
impl_instruction_enum!(OpSymbol);
impl TryFrom<BasicOp> for OpSymbol {
    type Error = ();
    fn try_from(value: BasicOp) -> Result<Self, Self::Error> {
        crate::try_enum_convert!(
            BasicOp, OpSymbol, value, ();
            { Add, Sub, Mul, Div, IDiv, Mod, BAnd, BOr, BXor, Equal, NotEqual, LessThan,
                LessThanEq, GreaterThan, GreaterThanEq, LAnd, Shl, Shr, }
            { Not => Flip, }
        )
    }
}
impl fmt::Display for OpSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name: &'static str = self.into();
        f.write_str(name)
    }
}
impl OpSymbol {
    pub fn try_from_basic_op(op: BasicOp) -> Option<OpSymbol> {
        Self::try_from(op).ok()
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum JumpSymbol {
    #[strum(to_string = "equal")]
    Equal,
    #[strum(to_string = "notEqual")]
    NotEqual,
    #[strum(to_string = "lessThan")]
    LessThan,
    #[strum(to_string = "lessThanEq")]
    LessThanEq,
    #[strum(to_string = "greaterThan")]
    GreaterThan,
    #[strum(to_string = "greaterThanEq")]
    GreaterThanEq,
    #[strum(to_string = "strictEqual")]
    StrictEqual,
    #[strum(to_string = "always")]
    Always,
}
impl_instruction_enum!(JumpSymbol);
impl TryFrom<OpSymbol> for JumpSymbol {
    type Error = ();
    fn try_from(value: OpSymbol) -> Result<Self, Self::Error> {
        crate::try_enum_convert!(
            OpSymbol, JumpSymbol, value, ();
            { Equal, NotEqual, LessThan, LessThanEq, GreaterThan, GreaterThanEq, StrictEqual, }
            {}
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum DrawMode {
    #[strum(to_string = "clear")]
    Clear,
    #[strum(to_string = "color")]
    Color,
    #[strum(to_string = "stroke")]
    Stroke,
    #[strum(to_string = "line")]
    Line,
    #[strum(to_string = "rect")]
    Rect,
    #[strum(to_string = "lineRect")]
    LineRect,
    #[strum(to_string = "poly")]
    Poly,
    #[strum(to_string = "linePoly")]
    LinePoly,
    #[strum(to_string = "triangle")]
    Triangle,
    #[strum(to_string = "image")]
    Image,
}
impl_instruction_enum!(DrawMode);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum ControlMode {
    #[strum(to_string = "enabled")]
    Enabled,
    #[strum(to_string = "shoot")]
    Shoot,
    #[strum(to_string = "shootp")]
    ShootP,
    #[strum(to_string = "configure")]
    Configure,
    #[strum(to_string = "color")]
    Color,
}
impl_instruction_enum!(ControlMode);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum RadarTarget {
    #[strum(to_string = "any")]
    Any,
    #[strum(to_string = "enemy")]
    Enemy,
    #[strum(to_string = "ally")]
    Ally,
    #[strum(to_string = "player")]
    Player,
    #[strum(to_string = "attacker")]
    Attacker,
    #[strum(to_string = "flying")]
    Flying,
    #[strum(to_string = "boss")]
    Boss,
    #[strum(to_string = "ground")]
    Ground,
}
impl_instruction_enum!(RadarTarget);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum RadarSort {
    #[strum(to_string = "distance")]
    Distance,
    #[strum(to_string = "health")]
    Health,
    #[strum(to_string = "shield")]
    Shield,
    #[strum(to_string = "armor")]
    Armor,
    #[strum(to_string = "maxHealth")]
    MaxHealth,
}
impl_instruction_enum!(RadarSort);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum UnitControlMode {
    #[strum(to_string = "idle")]
    Idle,
    #[strum(to_string = "stop")]
    Stop,
    #[strum(to_string = "move")]
    Move,
    #[strum(to_string = "approach")]
    Approach,
    #[strum(to_string = "boost")]
    Boost,
    #[strum(to_string = "pathfind")]
    Pathfind,
    #[strum(to_string = "target")]
    Target,
    #[strum(to_string = "targetp")]
    TargetP,
    #[strum(to_string = "itemDrop")]
    ItemDrop,
    #[strum(to_string = "itemTake")]
    ItemTake,
    #[strum(to_string = "payDrop")]
    PayDrop,
    #[strum(to_string = "payTake")]
    PayTake,
    #[strum(to_string = "mine")]
    Mine,
    #[strum(to_string = "flag")]
    Flag,
    #[strum(to_string = "build")]
    Build,
    #[strum(to_string = "getBlock")]
    GetBlock,
    #[strum(to_string = "within")]
    Within,
}
impl_instruction_enum!(UnitControlMode);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum LocateKind {
    #[strum(to_string = "ore")]
    Ore,
    #[strum(to_string = "building")]
    Building,
    #[strum(to_string = "spawn")]
    Spawn,
    #[strum(to_string = "damaged")]
    Damaged,
}
impl_instruction_enum!(LocateKind);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, strum::EnumString, strum::IntoStaticStr)]
pub enum BuildingGroup {
    #[strum(to_string = "core")]
    Core,
    #[strum(to_string = "storage")]
    Storage,
    #[strum(to_string = "generator")]
    Generator,
    #[strum(to_string = "turret")]
    Turret,
    #[strum(to_string = "factory")]
    Factory,
    #[strum(to_string = "repair")]
    Repair,
    #[strum(to_string = "rally")]
    Rally,
    #[strum(to_string = "battery")]
    Battery,
    #[strum(to_string = "resupply")]
    Resupply,
    #[strum(to_string = "reactor")]
    Reactor,
}
impl_instruction_enum!(BuildingGroup);
