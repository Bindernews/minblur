mod builtin_macros;
pub(crate) mod common;
pub mod compiler;
pub mod parser;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
