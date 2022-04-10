mod avalue;
pub mod basic;
pub mod error;
pub mod expression;
mod macros;
pub mod parse;
pub mod util;

pub use avalue::AValue;
pub use error::{EvalError, ParseError};
pub use expression::*;

#[cfg(test)]
pub mod test {
    use std::fmt;

    use super::{
        basic::{BasicEvaluator, BasicOp},
        expression::*,
        AValue,
    };
    use crate::{
        build_expression,
        error::{EvalError, ParseError},
        parse::{match_name_basic, ExpressionParser, IExpressionParser},
    };

    macro_rules! new_expr {
        ($tokens:tt) => { build_expression!(BasicOp; $tokens) };
    }

    type ExpressionB = Expression<BasicOp>;

    fn parse_expression(input: &str) -> Result<ExpressionB, ParseError> {
        ExpressionParser::new(match_name_basic, match_name_basic)
            .parse_expression(input)
            .map_err(ParseError::from)
    }
    fn eval_expression(input: &str) -> Result<AValue, EvalError> {
        let expr = parse_expression(input)?;
        expr.eval(&mut BasicEvaluator::new(false))
    }
    fn _fmt_err_debug<T: fmt::Debug, E: fmt::Debug>(result: Result<T, E>) -> String {
        format!("{:?}", result.unwrap_err())
    }
    fn fmt_err_display<T: fmt::Debug, E: fmt::Display>(result: Result<T, E>) -> String {
        format!("{}", result.unwrap_err())
    }

    #[test]
    fn test_basic_value() {
        let inp = "5";
        let exp = Expression::from(5i64);
        let res = parse_expression(inp);
        assert_eq!(exp, res.unwrap());
    }

    #[test]
    fn test_add() {
        let inp = "5 + 3";
        let exp = new_expr!((bin "+", 5i64, 3i64));
        // let exp = new_bin(eop!(+), 5i64, 3i64);
        let res = parse_expression(inp);
        assert_eq!(exp, res.unwrap())
    }

    #[test]
    fn test_add_sub() {
        let inp = "5 + 3 - 2";
        // Note that higher-precedence operators are processed first, meaning
        // they're DEEPER in the expression tree.
        let exp = new_expr!((bin "-", (bin "+", 5, 3), 2));
        assert_eq!(parse_expression(inp).unwrap(), exp);
    }

    #[test]
    fn test_order_of_operations() {
        let inp = "5 + 5 * (3 - 1)";
        let exp = new_expr!((bin "+", 5, (bin "*", 5, (bin "-", 3, 1))));
        assert_eq!(parse_expression(inp).unwrap(), exp);
    }

    #[test]
    fn test_math_ops() {
        let inp = "(6 // 5 == 1) && (6/5 < 2) && (1 << 1 == 2) && (2 >> 1 == 1) && (5 % 2 == 1)";
        assert!(eval_expression(inp).unwrap().is_truthy());
    }

    #[test]
    fn match_name() {
        let input = "abc";
        assert_eq!(parse_expression(input).unwrap(), AValue::name("abc").into());
    }

    #[test]
    fn test_bad_adds() {
        assert_eq!(
            format!("{:?}", eval_expression("\"abc\" + 5").unwrap_err()),
            "IncompatibleTypes { op: \"+\", lhs: \"String\", rhs: \"Num\" }"
        );
    }

    // #[test]
    fn _test_add_strings() {
        let inp = "\"abc\" + \"def\"";
        assert_eq!(
            eval_expression(inp).unwrap(),
            AValue::String("abcdef".into())
        );
    }

    #[test]
    fn test_display() {
        let inp = "((3 + 5) - 6) * (7 / (4 * radians(180))) == (!5)";
        assert_eq!(
            format!("{}", parse_expression(inp).unwrap()),
            "(3 + 5 - 6) * (7 / (4 * radians(180))) == (! 5)"
        );
    }

    #[test]
    fn test_eval_1() {
        let inp = "5 + 5 * (3 - 1) == 15";
        assert!(eval_expression(inp).unwrap().is_truthy());
    }

    #[test]
    fn test_bad_operator() {
        let inp = "5 = 7";
        assert_eq!(
            fmt_err_display(parse_expression(inp)),
            "at 1:3 near '=' => unexpected token\n  at 1:3 near '=' => Eof\n"
        );
    }

    #[test]
    fn too_many_parens() {
        let input = "(a + b))";
        assert_eq!(
            fmt_err_display(parse_expression(input)),
            "at 1:8 near ')' => unexpected token\n  at 1:8 near ')' => Eof\n"
        );
    }

    #[test]
    fn too_few_parens() {
        let input = "(a + b";
        assert_eq!(
            fmt_err_display(parse_expression(input)),
            "at 1:7 => expected ')'\n"
        );
    }

    #[test]
    fn test_partial_application() {
        let inp = "5 + 7 + x";
        let exp = new_expr!((bin "+", 12, (name "x")));
        let res = parse_expression(inp)
            .unwrap()
            .partial_eval(&mut BasicEvaluator::new(true))
            .unwrap();
        assert_eq!(exp, res);
    }
}
