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
///     { len, name }
///     {
///         A => { len: 3, name: "A" }
///         B => { name: "B", len: 4 }
///         C => { name: "C", len: 5 }
///     }
/// }
/// impl Abc {
///     pub fn len(&self) -> usize {
///         abc_props!(len; match self into)
///     }
///     pub fn name(&self) -> &'static str {
///         abc_props!(name; match self into)
///     }
///     pub fn from_name(s: &str) -> Option<Self> {
///         abc_props!(name; match s from)
///     }
/// }
/// ```
/// 
#[macro_export]
macro_rules! build_enum_match {
    (
        $macro_name:tt; $enum_name:path;
        { $($f_name:ident),+ $(,)? }
        {$(
            $variant:ident => { $($k:ident: $v:expr),+ $(,)? } $(,)?
        )+}
    ) => {
        // Convert enum-style definitions into `[key; variant; value]` triplets
        $crate::build_enum_match!{@
            [$]; $macro_name; $enum_name;
            { $($f_name),+ }
            [$($([$k; $variant; $v])+)+]
        }
    };
    (@
        [$dd:tt]; $macro_name:tt; $enum_name:path;
        { $($f_name:ident),+ }
        $tail:tt
    ) => {
        macro_rules! $macro_name {
            ($dd field:ident; $dd ($dd extra:tt)*) => {
                $macro_name!{@ ($enum_name; $dd ($dd extra)*) $dd field [] $tail}
            };
            $(
            (@
                $dd _args:tt $f_name [$dd ([$dd a:ident; $dd b:expr])*]
                [[$f_name; $dd k:ident; $dd v:expr] $dd ($dd tail:tt)*]
            ) => {
                $macro_name!{@ $dd _args $f_name [$dd ([$dd a;$dd b])* [$dd k;$dd v]] [$dd ($dd tail)*]}
            };
            (@
                $dd _args:tt $f_name [$dd ([$dd a:ident; $dd b:expr])*]
                [[$dd _other:ident; $dd k:ident; $dd v:expr] $dd ($dd tail:tt)*]
            ) => {
                $macro_name!{@ $dd _args $f_name [$dd ([$dd a;$dd b])*] [$dd ($dd tail)*]}
            };
            (@
                $dd _args:tt $f_name [$dd ([$dd k:ident; $dd v:expr])*] []
            ) => {
                $crate::enum_match_output!{$dd _args $f_name [$dd ([$dd k; $dd v])*]}
            };
            )+
            
        }
    };
}

// This is the "source" for build_enum_match macro generator.
//
// How to convert the source:
// 1. Copy-paste source to new location
// 2. Replace all "$" with "$dd "
// 3. Replace all "macro_name" with "$macro_name", "f_name" with "$f_name",
//    ENUM_NAME with "$enum_name" and TAIL with $tail
// 4. Replace all "f_name" with "$f_name"
// 5. Surround the @ variants in appropriate $( )+ repetition
// 6. Profit!
/*
macro_rules! macro_name {
    // Entry (do not repeat)
    ($field:ident; $($extra:tt)*) => {
        $macro_name!{@ (ENUM_NAME; $($extra)*) $field [] TAIL}
    };
    // Repeat these
    (@
        $_args:tt f_name [$([$a:ident; $b:tt])*]
        [[f_name; $k:ident; $v:tt] $($tail:tt)*]
    ) => {
        macro_name!{@ $_args f_name [$([$a;$b])* [$k;$v]] [$($tail)*]}
    };
    (@
        $_args:tt f_name [$([$a:ident; $b:tt])*]
        [[$_other:ident; $k:ident; $v:tt] $($tail:tt)*]
    ) => {
        macro_name!{@ $_args f_name [$([$a;$b])*] [$($tail)*]}
    };
    (@
        $_args:tt f_name [$([$k:ident; $v:tt])*] []
    ) => {
        $crate::enum_match_output!{$_args f_name [$([$k; $v])*]}
    };
}
*/


/*
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
/// minblur_lib::build_enum_match2!{
///     abc_props;
///     Abc;
///     { name, len }
///     {
///         A => { name: "A", len: 3 }
///         B => { name: "B", len: 4 }
///         C => { name: "C", len: 5 }
///     }
/// }
/// impl Abc {
///     pub fn len(&self) -> usize {
///         abc_props!(len; match self into)
///     }
///     pub fn name(&self) -> &'static str {
///         abc_props!(name; match self into)
///     }
///     pub fn from_name(s: &str) -> Option<Self> {
///         abc_props!(name; match s from)
///     }
/// }
/// ```
/// 
#[macro_export]
macro_rules! build_enum_match2 {
    (
        $macro_name:tt; $enum_name:path;
        { $($f_name:ident),+ $(,)? }
        {$(
            $variant:ident => { $($k:ident: $v:tt),+ $(,)? } $(,)?
        )+}
    ) => {
        $crate::build_enum_match2!{@1
            ($macro_name; $enum_name;)
            { $($f_name),+ } [$([$f_name])+]
            [$($([$k; $variant; $v])+)+]
        }
    };
    (@1 $_args:tt
        { $($f_name:ident),+ }
        $other_names:tt
        $tail:tt
    ) => {
        $crate::build_enum_match2!{@2 [$] $_args
            { $($f_name $other_names),+ }
            $tail
        }
    };
    (@2
        [$dd:tt] ($macro_name:tt; $enum_name:path;)
        { $($f_name:ident [$([$other_names:ident])+]),+ }
        $tail:tt
    ) => {
        macro_rules! $macro_name {
            ($dd field:ident; $dd ($dd extra:tt)*) => {
                $macro_name!{@p1 ($enum_name; $dd ($dd extra)*) $dd field $tail}
            };
            $(
                (@p1 $dd _args:tt $f_name [$dd ($($dd $other_names:tt)+)+]) => {
                    $macro_name!{@p2 $dd _args $f_name [$dd ($dd $f_name)+]}
                };
                (@p2 $dd _args:tt $f_name [$dd ([$f_name; $dd k:ident; $dd v:tt])*]) => {
                    $macro_name!{@p3 $dd _args $f_name [$dd ([$dd k; $dd v])*]}
                };
            )+
            (@p1 $dd _args:tt $dd unknown_name:ident $dd tail:tt) => {
                std::compile_error!(concat!("Available fields: ", stringify!($($f_name),+)))
            };
            (@p3 $dd ($dd tail:tt)*) => {
                $crate::enum_match_output!{$dd ($dd tail)*}
            };
        }
    };
}
*/

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
/// minblur_lib::build_enum_match_alt!{
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
macro_rules! build_enum_match_alt {
    (
        $macro_name:ident;
        $enum_name:path;
        $($tail:tt)*
    ) => {
        $crate::build_enum_match_alt!{@ [$] $macro_name; $enum_name; $($tail)* }
    };
    (@
        [$dd:tt]
        $macro_name:ident;
        $enum_name:path;
        $(
            $name:ident => {
                $($k:ident: $v:tt),+ $(,)?
            }
        )+
    ) => {
        macro_rules! $macro_name {
            $(
                ($name; $dd ($dd extra:tt)*) => {
                    $crate::enum_match_output!{
                        ($enum_name; $dd ($dd extra)*) $name [$([$k; $v])+] }
                };
            )+
        }
    };
}

#[macro_export]
macro_rules! enum_match_output {
    (
        ($enum_type:path; match $input:ident into with $callback:ident)
        $f_name:ident [$([$k:ident; $v:tt])*]
    ) => {
        match $input { $(<$enum_type>::$k => $callback!($k; $v)),* }
    };
    (
        ($enum_type:path; match $input:ident into)
        $f_name:ident [$([$k:ident; $v:expr])*]
    ) => {
        match $input { $(<$enum_type>::$k => $v,)+ }
    };
    (
        ($enum_type:path; match $input:ident from)
        $f_name:ident [$([$k:ident; $v:expr])*]
    ) => {
        match $input { $($v => Some(<$enum_type>::$k),)+ _ => None }
    };
    (
        ($enum_type:path; pairs with $callback:ident)
        $f_name:ident [$([$k:ident; $v:tt])*]
    ) => {
        $callback!{ [$([$k; $v])*] }
    }
}


/// Implements a `match` statement to help convert from one enum type to another.
///
/// Both types of enums must be "variant-less", basically just a group of names.
/// This macro generates a `match` statement, and thus can be used in functions.
///
/// # Example
/// ```rust
/// use std::convert::TryFrom;
/// pub enum Math1 { Add, Sub, Mul, IDiv }
/// pub enum Math2 { Add, Mul, Div }
/// impl TryFrom<Math2> for Math1 {
///     type Error = ();
///     fn try_from(value: Math2) -> Result<Self, Self::Error> {
///         // <from type>, <to type>, <match value>, <error value>
///         minblur_lib::try_enum_convert!(Math2, Math1, value, ();
///             // Since these have the same names they don't need to be repeated
///             { Add, Mul }
///             // The second block is for values with different names
///             { Div => IDiv }
///         )
///     }
/// }
/// ```
#[macro_export]
macro_rules! try_enum_convert {
    (
        $src_type:ty,
        $dst_type:ty,
        $in_value:expr,
        $err_value:expr ;
        { $( $same_name:ident ),* $(,)? }
        { $( $src_name:ident => $dst_name:ident ),* $(,)? }
    ) => {
        #[allow(unreachable_patterns)]
        match $in_value {
            $( <$src_type>::$same_name => Ok(<$dst_type>::$same_name), )*
            $( <$src_type>::$src_name => Ok(<$dst_type>::$dst_name), )*
            _ => Err($err_value),
        }
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

/// Returns nth identifier
#[allow(unused)]
macro_rules! take_n {
    ( 0 ($a0:expr)) => { $a0 };
    ( 0 ($a0:expr, $($rest:expr),*)) => { $a0 };
    ( 1 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 0 ($($rest),*)) };
    ( 2 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 1 ($($rest),*)) };
    ( 3 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 2 ($($rest),*)) };
    ( 4 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 3 ($($rest),*)) };
    ( 5 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 4 ($($rest),*)) };
    ( 6 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 5 ($($rest),*)) };
    ( 7 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 6 ($($rest),*)) };
    ( 8 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 7 ($($rest),*)) };
    ( 9 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 8 ($($rest),*)) };
    (10 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!( 9 ($($rest),*)) };
    (11 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!(10 ($($rest),*)) };
    (12 ($a0:expr, $($rest:expr),*)) => { $crate::take_n!(11 ($($rest),*)) };
}

#[allow(unused)]
macro_rules! expand_tuple {
    (($a0:tt)) => {
        ($a0, (), (), (), (), (), (), (), (), ())
    };
    (($a0:tt,$a1:tt)) => {
        ($a0, $a1, (), (), (), (), (), (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt)) => {
        ($a0, $a1, $a2, (), (), (), (), (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt)) => {
        ($a0, $a1, $a2, $a3, (), (), (), (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, (), (), (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt,$a5:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, $a5, (), (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt,$a5:tt,$a6:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, $a5, $a6, (), (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt,$a5:tt,$a6:tt,$a7:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, (), ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt,$a5:tt,$a6:tt,$a7:tt,$a8:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, ())
    };
    (($a0:tt,$a1:tt,$a2:tt,$a3:tt,$a4:tt,$a5:tt,$a6:tt,$a7:tt,$a8:tt,$a9:tt)) => {
        ($a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9)
    };
}

// Export macro to crate
pub(crate) use {
    each_ref, enum_variant_call, impl_accessors, prcall,
};
