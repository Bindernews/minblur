macro_rules! enum_variant_call {
    (
        match $match_expr:expr;
        { $($arm:pat),+ , }
        ($v_arg:ident) => $code:block
    ) => {
        match $match_expr {
            $(
                $arm => $code
            ),+
        }
    };
}

/// Turns &[T; <n>] into [&T; <n>]
macro_rules! each_ref {
    ($ar:expr; $($idx:ident),+) => {
        {
            let [$($idx),+] = $ar;
            [$($idx),+]
        }
    };
}

/// Wrap a parser function call in a lambda
macro_rules! prcall {
    ($($exprs:tt)+) => {
        |inp| { $($exprs)+ (inp) }
    };
}

//impl
#[macro_export]
macro_rules! compact_from {
    (
        impl[$($type_args:tt)*] From[$src_type:path]
        for $dst_type:path => |$src:ident| $code:block
        $($tail:tt)*
    ) => {
        impl< $($type_args)* >
        From<$src_type> for $dst_type {
            fn from($src : $src_type) -> $dst_type $code
        }
        $crate::compact_from!{$($tail)*}
    };
    (
        impl$([$($type_args:tt)*])? TryFrom[$src_type:path]
        for $dst_type:path where Error = [$error_type:ty] => |$src:ident| $code:block
        $($tail:tt)*
    ) => {
        impl$(< $($type_args)* >)?
        TryFrom<$src_type> for $dst_type {
            type Error = $error_type;
            fn try_from($src : $src_type) -> Result<Self, Self::Error> $code
        }
        $crate::compact_from!{$($tail)*}
    };
    () => {};
}

/// Helper to implement `&T` and `&mut T` accessor functions.
///
///
#[macro_export]
macro_rules! impl_accessors {
    (
        $dest_type:ty; $self:ident;
        $($tail:tt)*
    ) => {
        impl $dest_type {
            $crate::impl_accessors!{@ $self; $($tail)*}
        }
    };
    (@ $self:ident;
        $(#[$attr:meta])*
        fn $get_name:ident() -> $ret_type:ty { $get_expr:expr }
        $(
            $(#[$attr_mut:meta])*
            mut fn $get_mut_name:ident ()
        )?
        ;
        $($tail:tt)*
    ) => {
        $(#[$attr])*
        pub fn $get_name(& $self) -> & $ret_type { & $get_expr }
        $(
            $(#[$attr_mut])*
            pub fn $get_mut_name(&mut $self) -> &mut $ret_type { &mut $get_expr }
        )?
        $crate::impl_accessors!{@ $self; $($tail)*}
    };
    (@ $self:ident;
        $(#[$attr:meta])*
        fn $get_name:ident() -> Option[$ret_type:ty] $get_ref:block
        $(
            $(#[$attr_mut:meta])*
            mut fn $get_mut_name:ident ()
        )?
        ;
        $($tail:tt)*
    ) => {
        $(#[$attr])*
        pub fn $get_name(& $self) -> Option<& $ret_type> $get_ref
        $(
            $(#[$attr_mut])*
            pub fn $get_mut_name(&mut $self) -> Option<&mut $ret_type> $get_ref
        )?
        $crate::impl_accessors!{@ $self; $($tail)*}
    };
    (@ $self:ident;) => {};
}

#[macro_export]
macro_rules! __parse_enum_map {
    ($callback:path; $cb_args:tt $tail:tt) => {
        $crate::__parse_enum_map!{@1 ($callback; $cb_args) [] $tail}
    };
    // Match two single arguments (reduces recursion count)
    (@1 $args:tt [$($a:tt)*] { $k1:ident, $k2:ident $(, $($tail:tt)*)? }) => {
        $crate::__parse_enum_map!{@1 $args [$($a)* [$k1; $k1] [$k2; $k2]] { $($($tail)*)? }}
    };
    // Match one single argument
    (@1 $args:tt [$($a:tt)*] { $k:ident $(, $($tail:tt)*)? }) => {
        $crate::__parse_enum_map!{@1 $args [$($a)* [$k; $k]] { $($($tail)*)? }}
    };
    // Match one double argument
    (@1 $args:tt [$($a:tt)*] { $k:ident => $v:ident $(, $($tail:tt)*)? }) => {
        $crate::__parse_enum_map!{@1 $args [$($a)* [$k; $v]] { $($($tail)*)? }}
    };
    // End case
    (@1 ($callback:path; [$($cb_args:tt)*]) [$($a:tt)*] { }) => {
        $callback!{$($cb_args)* [$($a)*]}
    };
}

/// Generates a macro that will generate `match` statements
/// to either convert to or from the enum.
///
/// # Example
/// ```rust
/// enum Abc {
///     A,
///     B,
///     C,
/// }
///
/// minblur_lib::build_enum_match!{
///     abc_props;
///     Abc;
///     name => {
///         A: "alpha",
///         B: "beta",
///         C: "charlie",
///     }
///     size => {
///         A: 1,
///         B: 3,
///         C: 5,
///     }
/// }
/// impl Abc {
///     pub fn size(&self) -> usize {
///         abc_props!(size; match self into)
///     }
///     pub fn name(&self) -> &'static str {
///         abc_props!(name; match self into)
///     }
///     pub fn from_name(s: &str) -> Option<Self> {
///         abc_props!(name; match s from)
///     }
/// }
/// ```
#[macro_export]
macro_rules! build_enum_match {
    (
        $macro_name:ident;
        $enum_name:path;
        $($tail:tt)*
    ) => {
        $crate::build_enum_match!{@1 ($macro_name; $enum_name;) [] $($tail)* }
    };
    //@1 Build up name => data groups
    (@1 $args:tt [$($a:ident => $b:tt;)*]
        $name:ident => {
            $($k:ident: $v:tt),+ $(,)?
        }
        $($tail:tt)*
    ) => {
        $crate::build_enum_match!{@1 $args
            [$($a => $b;)* $name => [$([$k;$v])+];] $($tail)*
        }
    };
    // Enum maps
    (@1 $args:tt $data:tt
        $name:ident [enum] => { $($enum_args:tt)* }
        $($tail:tt)*
    ) => {
        $crate::__parse_enum_map!{
            $crate::build_enum_match;
            [@enum_parse_callback $args $data $name [$($tail)*]]
            { $($enum_args)* }
        }
    };
    (@enum_parse_callback $args:tt [$($a:ident => $b:tt;)*] $name:ident [$($tail:tt)*]
        $new_data:tt
    ) => {
        $crate::build_enum_match!{@1 $args
            [$($a => $b;)* $name => $new_data;] $($tail)*
        }
    };
    // Multi-keys
    (@1 $args:tt $data:tt
        ($($names:ident),+ $(,)?) => {
            $($k:ident: ($($v:tt),+)),+ $(,)?
        }
        $($tail:tt)*
    ) => {
        $crate::build_enum_match!{@multi $args [$($tail)*] $data
            ($($names,)+) => { $([ $([$k;$v])+ ])+ }
        }
    };
    // Multi many-case
    (@multi $args:tt $tail:tt [$($a:ident => $b:tt;)*]
        ($name0:ident, $($names:ident,)+) => {
            $([$pair0:tt $($pairs:tt)+])+
        }
    ) => {
        $crate::build_enum_match!{@multi $args $tail
            [$($a => $b;)* $name0 => [$($pair0)+];]
            ($($names,)+) => { $([$($pairs)+])+ }
        }
    };
    // Multi base-case
    (@multi $args:tt [$($tail:tt)*] [$($a:ident => $b:tt;)*]
        ($name0:ident,) => {
            $([$pair0:tt])+
        }
    ) => {
        $crate::build_enum_match!{@1 $args
            [$($a => $b;)* $name0 => [$($pair0)+];]
            $($tail)*
        }
    };
    //@1 base case
    (@1 $args:tt [$($a:ident => $b:tt;)*]) => {
        $crate::build_enum_match!{@2 [$] $args [$($a => $b;)*]}
    };
    //@2 Generate macro
    (@2 [$dd:tt] ($macro_name:ident; $enum_name:path;)
        [$($name:ident => $data:tt;)+]
    ) => {
        /// See [`crate::enum_match_output`] for usage
        macro_rules! $macro_name {
        $(
            ($name; $dd ($dd extra:tt)*) => {
                $crate::enum_match_output!{($enum_name; $dd ($dd extra)*) $data}
            };
        )+
        }
    };
}

#[macro_export]
macro_rules! enum_match_output {
    (
        ($enum_type:path; match $input:ident into with $callback:ident)
        [$([$k:ident; $v:tt])*]
    ) => {
        {
            use $enum_type as _enum_type;
            match $input { $(_enum_type::$k{..} => $callback!($k; $v)),* }
        }
    };
    (($enum_type:path; match $input:ident into)  [$([$k:ident; $v:expr])*]) => {
        {
            use $enum_type as _enum_type;
            match $input { $(_enum_type::$k{..} => $v,)+ }
        }
    };
    (($enum_type:path; match $input:ident from)  [$([$k:ident; $v:expr])*]) => {
        match $input { $($v => Some(<$enum_type>::$k),)+ _ => None }
    };
    (($enum_type:path; pairs with $callback:ident)  [$([$k:ident; $v:tt])*]) => {
        $callback!{ [$([$k; $v])*] }
    };

    // impl From[Self] for $newtype
    (($enum_type:path; impl From[Self] for $dst_type:path)  [$([$k:ident; $v:ident])*]) => {
        $crate::enum_match_output!{@from ($enum_type; $dst_type;) [$([$k; $v])*] }
    };
    // impl From[$newtype] for Self
    (($enum_type:path; impl From[$src_type:path] for Self)  [$([$k:ident; $v:ident])*]) => {
        $crate::enum_match_output!{@from ($src_type; $enum_type;) [$([$v; $k])*] }
    };
    // impl From<> helper
    (@from ($src_type:path; $dst_type:path;) [$([$k:ident; $v:ident])*]) => {
        impl From<$src_type> for $dst_type {
            fn from(value: $src_type) -> $dst_type {
                use $src_type as _src_ty;
                match value { $(_src_ty::$k{..} => <$dst_type>::$v,)* }
            }
        }
    };

    // impl TryFrom[Self] for $newtype
    (($enum_type:path; impl TryFrom[Self] for $dst_type:path)  [$([$k:ident; $v:ident])*]) => {
        $crate::enum_match_output!{@try_from ($enum_type; $dst_type;)
            [$([$k; $v])*] }
    };
    // impl TryFrom[$newtype] for Self
    (($enum_type:path; impl TryFrom[$src_type:path] for Self)  [$([$k:ident; $v:ident])*]) => {
        $crate::enum_match_output!{@try_from ($src_type; $enum_type;)
            [$([$v; $k])*] }
    };
    // impl TryFrom<> helper
    (@try_from ($src_type:path; $dst_type:path;)  [$([$k:ident; $v:ident])*]) => {
        impl std::convert::TryFrom<$src_type> for $dst_type {
            type Error = ();
            fn try_from(value: $src_type) -> Result<Self, Self::Error> {
                use $src_type as _src_ty;
                #[allow(unreachable_patterns)]
                match value {
                    $(_src_ty::$k => Ok(<$dst_type>::$v),)*
                    _ => Err(()),
                }
            }
        }
    };
}

///
/// ```rust
/// use std::convert::TryFrom;
///
/// #[derive(Debug, Copy, Clone, PartialEq, Eq)]
/// pub enum Math1 { Add, Sub, Mul, IDiv }
///
/// #[derive(Debug, Copy, Clone, PartialEq, Eq)]
/// pub enum Math2 { Add, Mul, Div }
///
/// // The [symmetric] option will also implement TryFrom<Math1> for Math2 {}.
/// minblur_lib::enum_try_from!{
///     impl TryFrom[Math2] for Math1 [symmetric];
///     {
///         Add, Mul, Div => IDiv
///     }
/// }
///
/// assert_eq!(Math1::try_from(Math2::Add).ok(), Some(Math1::Add));
/// assert_eq!(Math1::try_from(Math2::Div).ok(), Some(Math1::IDiv));
/// assert_eq!(Math2::try_from(Math1::IDiv).ok(), Some(Math2::Div));
/// ```
#[macro_export]
macro_rules! enum_try_from {
    (
        impl TryFrom[$src_type:path] for $dst_type:path $([$symmetric:ident])?;
        { $($tail:tt)* }
    ) => {
        $crate::enum_try_from!{@1
            (TryFrom; $src_type; $dst_type; $($symmetric)?;)
            [] { $($tail)* }
        }
    };
    (
        impl From[$src_type:path] for $dst_type:path $([$symmetric:ident])?;
        { $($tail:tt)* }
    ) => {
        $crate::enum_try_from!{@1
            (From; $src_type; $dst_type; $($symmetric)?;)
            [] { $($tail)* }
        }
    };
    // Recursion to build argument tree
    (@1 $args:tt [$($a:tt)*] { $k:ident => $v:ident $(, $($tail:tt)*)? }) => {
        $crate::enum_try_from!{@1 $args [$($a)* [$k; $v]] { $($($tail)*)? }}
    };
    (@1 $args:tt [$($a:tt)*] { $k:ident $(, $($tail:tt)*)? }) => {
        $crate::enum_try_from!{@1 $args [$($a)* [$k; $k]] { $($($tail)*)? }}
    };
    (@1 $args:tt [$($a:tt)*] { }) => {
        $crate::enum_try_from!{@2 $args [$($a)*] }
    };
    // Outputs
    (
        @2 ($from_type:ident; $src_type:path; $dst_type:path; symmetric;)
        [$([$k:ident; $v:ident])*]
    ) => {
        $crate::enum_match_output!{ ($src_type; impl $from_type[Self] for $dst_type)
            [$([$k; $v])*] }
        $crate::enum_match_output!{ ($src_type; impl TryFrom[$dst_type] for Self)
            [$([$k; $v])*] }
    };
    (
        @2 ($from_type:ident; $src_type:path; $dst_type:path; ;)
        [$([$k:ident; $v:ident])*]
    ) => {
        $crate::enum_match_output!{ ($src_type; impl $from_type[Self] for $dst_type)
            [$([$k; $v])*] }
    };
}

///
/// ```rust
/// pub enum Command {
///     Read(CRead),
///     Write(CWrite),
///     Draw(Box<CDraw>),
///     Move(Box<CMove>),
/// }
/// minblur_lib::enum_from_variants! {
///     Command;
///     Read(CRead),
///     Write(CWrite),
///     Draw(Box<CDraw>),
///     Move(Box<CMove>),
///     Draw(CDraw) => |v| Self::Draw(Box::new(v)),
///     Move(CMove) => |v| Self::Move(Box::new(v)),
/// }
///
/// pub struct CRead { pub data: String }
/// pub struct CWrite { pub data: String }
/// pub struct CDraw { pub data: [u8; 200] }
/// pub struct CMove { pub data: [u8; 200] }
///
/// #[test]
/// pub fn make_commands() {
///     let read1 = Command::Read(CRead { data: "left".to_string() });
///     let write1: Command = CWrite { data: "right".to_string() }.into();
///     let draw1 = Command::from(CDraw { data: [3u8; 200] });
///     let move1 = Command::from(Box::new(CMove { data: [5u8; 200 ] }));
/// }
/// ```
#[macro_export]
macro_rules! enum_from_variants {
    (
        $enum_type:ty;
        $variant:ident ( $from_type:ty ) => |$arg:ident| $code:expr,
        $($tail:tt)*
    ) => {
        impl From<$from_type> for $enum_type {
            fn from($arg: $from_type) -> $enum_type { $code }
        }
        $crate::enum_from_variants!($enum_type; $($tail)*);
    };
    (
        $enum_type:ty ;
        $enum_variant:ident ( $from_type:ty ),
        $($tail:tt)*
    ) => {
        impl From<$from_type> for $enum_type {
            fn from(v: $from_type) -> $enum_type { <$enum_type>::$enum_variant(v) }
        }
        $crate::enum_from_variants!($enum_type; $($tail)*);
    };
    (
        $enum_type:ty ;
    ) => {
    }
}

// Export macro to crate
pub(crate) use {each_ref, enum_variant_call, impl_accessors, prcall};
