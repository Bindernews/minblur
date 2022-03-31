
/// Helps implementing `as_str()` and `from_str()` methods
/// for enums. See [`basic::BasicOp`] for an example.
#[macro_export]
macro_rules! enum_to_from_str {
    (
        $etype:ty;
        $vis_to:vis fn $fn_to_str:ident ();
        $vis_from:vis fn $fn_from_str:ident ();
        {
            $( $pattern:path => $string:literal ),+ $(,)?
        }
    ) => {
        $vis_to fn $fn_to_str(&self) -> &'static str {
            match self {
                $( $pattern => $string ),+
            }
        }
        $vis_from fn $fn_from_str(input: &str) -> Option<$etype> {
            match input {
                $( $string => Some( $pattern ), )+
                _ => None,
            }
        }
    };
}

#[macro_export]
macro_rules! build_expression {
    ( $op_type:ty;
        (bin $op_str:expr, $lhs:tt, $rhs:tt $(,)? )
    ) => {
        $crate::Expression::<$op_type>::Binary(
            <$op_type>::match_op($op_str).unwrap().1,
            Box::new($crate::build_expression!($op_type; $lhs)),
            Box::new($crate::build_expression!($op_type; $rhs)),
        )
    };
    ( $op_type:ty;
        (un $op_str:expr, $lhs:tt $(,)? )
    ) => {
        $crate::Expression::<$op_type>::Unary(
            <$op_type>::match_op($op_str).unwrap().1,
            Box::new($crate::build_expression!($op_type; $lhs)),
        )
    };
    ( $op_type:ty;
        (name $ex:expr $(,)? )
    ) => {
        $crate::Expression::<$op_type>::Value(AValue::name($ex))
    };
    ( $op_type:ty;
        (str $ex:expr $(,)? )
    ) => {
        $crate::Expression::<$op_type>::Value(AValue::string($ex))
    };
    // Assume that literals are just numbers
    ( $op_type:ty;
        $num_value:expr
    ) => {
        $crate::Expression::<$op_type>::from(AValue::from($num_value as f64))
    };
    // Unwrap sub-expressions
    ( $op_type:ty;
        ($sub:tt)
    ) => {
        $crate::build_expression!($op_type; $sub)
    }
}
