pub trait TupleToArray<A> {
    fn to_array(self) -> A;
    fn from_array(ar: A) -> Self;
}

// NOTE: I'm using Rust Analyzer with VSCode to manually expand the macro
// since it seems to have some hygine errors otherwise.

// use crate::common::macros::n_tuple;
// macro_rules! tmp_a {
//     (0 ) => {a0 };
//     (1 ) => {a1 };
//     (2 ) => {a2 };
//     (3 ) => {a3 };
//     (4 ) => {a4 };
//     (5 ) => {a5 };
//     (6 ) => {a6 };
//     (7 ) => {a7 };
//     (8 ) => {a8 };
//     (9 ) => {a9 };
//     (10) => {a10};
//     (11) => {a11};
// }
// macro_rules! impl_tuple_to_array {
//     (
//         $the_trait:path, $t_ty:ty, $self:ident ;
//         $(
//             $array_size:literal: $($idx:literal),+ ;
//         )+
//     ) => {
//         $(
//             impl<$t_ty> $the_trait<[$t_ty; $array_size]> for n_tuple!($t_ty; $($idx),+) {
//                 fn to_array($self) -> [$t_ty; $array_size] {
//                     [$( $self.$idx ),+]
//                 }
//                 fn from_array(ar: [$t_ty; $array_size]) -> Self {
//                     let [ $(tmp_a!($idx)),+ ] = ar;
//                     ( $(tmp_a!($idx)),+ )
//                 }
//             }
//         )+
//     };
// }
// impl_tuple_to_array! {
//     TupleToArray, T, self;
//     1 : 0;
//     2 : 0, 1;
//     3 : 0, 1, 2;
//     4 : 0, 1, 2, 3;
//     5 : 0, 1, 2, 3, 4;
//     6 : 0, 1, 2, 3, 4, 5;
//     7 : 0, 1, 2, 3, 4, 5, 6;
//     8 : 0, 1, 2, 3, 4, 5, 6, 7;
//     9 : 0, 1, 2, 3, 4, 5, 6, 7, 8;
//     10: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9;
//     11: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10;
// }

// Recursive expansion of impl_tuple_to_array! macro
// ==================================================

impl<T> TupleToArray<[T; 1]> for (T,) {
    fn to_array(self) -> [T; 1] {
        [self.0]
    }
    fn from_array(ar: [T; 1]) -> Self {
        let [a0] = ar;
        (a0,)
    }
}
impl<T> TupleToArray<[T; 2]> for (T, T) {
    fn to_array(self) -> [T; 2] {
        [self.0, self.1]
    }
    fn from_array(ar: [T; 2]) -> Self {
        let [a0, a1] = ar;
        (a0, a1)
    }
}
impl<T> TupleToArray<[T; 3]> for (T, T, T) {
    fn to_array(self) -> [T; 3] {
        [self.0, self.1, self.2]
    }
    fn from_array(ar: [T; 3]) -> Self {
        let [a0, a1, a2] = ar;
        (a0, a1, a2)
    }
}
impl<T> TupleToArray<[T; 4]> for (T, T, T, T) {
    fn to_array(self) -> [T; 4] {
        [self.0, self.1, self.2, self.3]
    }
    fn from_array(ar: [T; 4]) -> Self {
        let [a0, a1, a2, a3] = ar;
        (a0, a1, a2, a3)
    }
}
impl<T> TupleToArray<[T; 5]> for (T, T, T, T, T) {
    fn to_array(self) -> [T; 5] {
        [self.0, self.1, self.2, self.3, self.4]
    }
    fn from_array(ar: [T; 5]) -> Self {
        let [a0, a1, a2, a3, a4] = ar;
        (a0, a1, a2, a3, a4)
    }
}
impl<T> TupleToArray<[T; 6]> for (T, T, T, T, T, T) {
    fn to_array(self) -> [T; 6] {
        [self.0, self.1, self.2, self.3, self.4, self.5]
    }
    fn from_array(ar: [T; 6]) -> Self {
        let [a0, a1, a2, a3, a4, a5] = ar;
        (a0, a1, a2, a3, a4, a5)
    }
}
impl<T> TupleToArray<[T; 7]> for (T, T, T, T, T, T, T) {
    fn to_array(self) -> [T; 7] {
        [self.0, self.1, self.2, self.3, self.4, self.5, self.6]
    }
    fn from_array(ar: [T; 7]) -> Self {
        let [a0, a1, a2, a3, a4, a5, a6] = ar;
        (a0, a1, a2, a3, a4, a5, a6)
    }
}
impl<T> TupleToArray<[T; 8]> for (T, T, T, T, T, T, T, T) {
    fn to_array(self) -> [T; 8] {
        [
            self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7,
        ]
    }
    fn from_array(ar: [T; 8]) -> Self {
        let [a0, a1, a2, a3, a4, a5, a6, a7] = ar;
        (a0, a1, a2, a3, a4, a5, a6, a7)
    }
}
impl<T> TupleToArray<[T; 9]> for (T, T, T, T, T, T, T, T, T) {
    fn to_array(self) -> [T; 9] {
        [
            self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7, self.8,
        ]
    }
    fn from_array(ar: [T; 9]) -> Self {
        let [a0, a1, a2, a3, a4, a5, a6, a7, a8] = ar;
        (a0, a1, a2, a3, a4, a5, a6, a7, a8)
    }
}
impl<T> TupleToArray<[T; 10]> for (T, T, T, T, T, T, T, T, T, T) {
    fn to_array(self) -> [T; 10] {
        [
            self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7, self.8, self.9,
        ]
    }
    fn from_array(ar: [T; 10]) -> Self {
        let [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] = ar;
        (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    }
}
impl<T> TupleToArray<[T; 11]> for (T, T, T, T, T, T, T, T, T, T, T) {
    fn to_array(self) -> [T; 11] {
        [
            self.0, self.1, self.2, self.3, self.4, self.5, self.6, self.7, self.8, self.9, self.10,
        ]
    }
    fn from_array(ar: [T; 11]) -> Self {
        let [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] = ar;
        (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    }
}
