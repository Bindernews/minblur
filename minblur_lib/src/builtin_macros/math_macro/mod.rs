use std::rc::Rc;

use bn_expression::EvalError;
use bn_expression::ParseError;

use crate::compiler::macros::prelude::*;

// Import statement module
mod statement;
use statement::*;

#[derive(Debug, thiserror::Error, strum::IntoStaticStr)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum MathError {
    #[error(transparent)]
    Parse(#[from] ParseError),
    #[error("invalid jump op '{0}'")]
    InvalidJumpOp(DisplayOption<MathOp>),
    #[error(transparent)]
    EvalError(#[from] EvalError),
    #[error("failed to create instruction \"{kind}\"")]
    InstructionCreate { kind: String },
    #[error("'{name}' does not return a value")]
    NoReturnFunction { name: &'static str },
    #[error("'{name}' returns a value, but the result isn't used")]
    ReturnUnused { name: &'static str },
}
impl MathError {}
impl DynamicError for MathError {}

#[derive(Debug)]
pub struct MathMacro {
    src_name: Rc<String>,
}

impl MathMacro {
    pub fn new() -> Self {
        Self {
            src_name: Rc::new("math!".into()),
        }
    }
}

impl MacroHandler for MathMacro {
    fn generate(
        &mut self,
        mut ctx: CompilerEnv,
        _source: Source,
        _name: &str,
        input: &str,
    ) -> MacroResult {
        let statements = parse::Parser::run_parse_content(input)?;
        let mut new_tokens = Vec::new();
        for statement in statements.iter() {
            let mut toks = statement.generate(&mut ctx, &self.src_name)?;
            new_tokens.append(&mut toks);
        }
        Ok(new_tokens)
    }
}

mod parse {
    use bn_expression::parse::{assert_eof, ExpressionParser, IExpressionParser, Position};
    use nom::combinator::{cut, value};
    use nom::multi::{many0, many1};
    use nom::sequence::pair;
    use nom::Finish;
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::space0,
        combinator::map,
        multi::separated_list1,
        sequence::{delimited, preceded, tuple},
        Parser as NomParser,
    };

    use super::statement::{AssignmentOp, Expression, MathOp};
    use super::*;
    use crate::parser::common::{
        identifier_basic, match_identifier_basic_const, parse_identifier_basic_const,
        statement_end, MyResult, Span,
    };

    pub struct Parser {}
    impl Parser {
        fn match_assignment_op(input: Span) -> MyResult<Span> {
            delimited(
                space0,
                alt((
                    tag("="),
                    tag("+="),
                    tag("-="),
                    tag("*="),
                    tag("/="),
                    tag("//="),
                )),
                space0,
            )(input)
        }

        fn parse_assign_start(input: Span) -> MyResult<(String, AssignmentOp)> {
            pair(
                parse_identifier_basic_const,
                map(Self::match_assignment_op, |s| {
                    AssignmentOp::from_str(*s).unwrap()
                }),
            )(input)
        }

        fn parse_expression(input: Span) -> MyResult<Expression> {
            let expr_p = expr_parser();
            let (input, taken) = expr_p.match_expression(input)?;
            let expr = expr_p.parse_expression(taken).map_err(nom::Err::Error)?;
            Ok((input, expr))
        }

        fn parse_assign_statement(input: Span) -> MyResult<AssignStatement> {
            pair(
                alt((
                    Self::parse_assign_start,
                    value(("".to_string(), AssignmentOp::Set), space0),
                )),
                cut(Self::parse_expression),
            )
            .map(
                |args: ((String, AssignmentOp), Expression)| AssignStatement {
                    pos: Position::from_span(&input),
                    assignee: args.0 .0,
                    op: args.0 .1,
                    expression: args.1,
                },
            )
            .parse(input)
        }

        fn parse_statement(input: Span) -> MyResult<MathStatement> {
            map(Self::parse_assign_statement, Into::into)(input)
        }

        fn statement_end_space(input: Span) -> MyResult<()> {
            preceded(space0, statement_end).map(|_| ()).parse(input)
        }

        fn parse_math_content(input: Span) -> MyResult<Vec<MathStatement>> {
            tuple((
                // Consume starting space
                value((), many0(Self::statement_end_space)),
                separated_list1(
                    many1(Self::statement_end_space),
                    preceded(space0, Self::parse_statement),
                ),
                // Consume ending space
                value((), many0(Self::statement_end_space)),
                // Assert all input consumed
                assert_eof,
            ))
            .map(|(_, a, _, _)| a)
            .parse(input)
        }

        pub fn run_parse_content(input: &str) -> Result<Vec<MathStatement>, MathError> {
            Self::parse_math_content(Span::new(input))
                .finish()
                .map(|a| a.1)
                .map_err(|e| {
                    // println!("ERROR: {}", &e);
                    ParseError::from(e).into()
                })
        }
    }

    fn expr_parser() -> impl IExpressionParser<MathOp> {
        ExpressionParser::new(match_identifier_basic_const, identifier_basic)
    }
}

#[cfg(test)]
pub mod test {
    use bn_expression::AValue;

    use super::MathMacro;
    use crate::{
        common::string_cache::*,
        compiler::instruction::*,
        compiler::{CompilerEnv, EnvMode, OsSystemApi},
        parser::{Statement, StatementData},
    };

    fn new_op_instr(op_type: &str, out_name: &str, left: AValue, right: AValue) -> StatementData {
        StatementData::new_instr(InstructionOp {
            op: InstValue::EnumName(CaString::from(op_type)),
            result: InstValue::new_name(out_name),
            left: InstValue::from(left),
            right: InstValue::from(right),
        })
    }

    fn new_compiler() -> CompilerEnv {
        let mut comp = CompilerEnv::new(OsSystemApi::new());
        comp.add_macro(EnvMode::Global, &["m"], MathMacro::new())
            .unwrap();
        comp
    }

    fn assert_tokens_eq(expected: &[StatementData], actual: &[Statement]) {
        assert_eq!(expected.len(), actual.len());
        for i in 0..expected.len() {
            assert_eq!(expected[i], actual[i].data);
        }
    }

    fn assert_tokens_eq_call(exp: &[StatementData], comp: &mut CompilerEnv, input: &str) {
        assert_tokens_eq(exp, &comp.call_macro_simple("m", input).unwrap())
    }

    #[test]
    fn test_single_value() {
        let mut comp = new_compiler();
        let inp = "x = 5";
        let exp = &[StatementData::new_instr(InstructionSet {
            result: InstValue::new_name("x"),
            value: InstValue::new_num(5f64),
        })];
        assert_tokens_eq_call(exp, &mut comp, inp);
    }

    #[test]
    fn test_plus_equal() {
        let mut comp = new_compiler();
        let inp = "x += 5";
        let exp = &[new_op_instr("add", "x", AValue::name("x"), 5f64.into())];
        assert_tokens_eq_call(exp, &mut comp, inp);
    }

    #[test]
    fn test_basic_math() {
        let mut comp = new_compiler();
        let inp = "x = y * 3 + (2.5 - 1)";
        let exp = &[
            new_op_instr("mul", "__t0", AValue::name("y"), 3f64.into()),
            new_op_instr("add", "x", AValue::name("__t0"), 1.5f64.into()),
        ];
        assert_tokens_eq(exp, &comp.call_macro_simple("m", inp).unwrap());
    }

    #[test]
    fn test_multi_line() {
        let mut comp = new_compiler();
        let inp = "\ny = pos // 20 \n x = pos % 20";
        let exp = &[
            new_op_instr("idiv", "y", AValue::name("pos"), 20f64.into()),
            new_op_instr("mod", "x", AValue::name("pos"), 20f64.into()),
        ];
        assert_tokens_eq(exp, &comp.call_macro_simple("m", inp).unwrap());
    }

    #[test]
    fn test_multi_line_const() {
        let mut comp = new_compiler();
        comp.add_define(EnvMode::Pass, "COLS", InstValue::new_num(20f64));
        let inp = "y = pos // $COLS \n x = pos % $COLS";
        let exp = &[
            new_op_instr("idiv", "y", AValue::name("pos"), 20f64.into()),
            new_op_instr("mod", "x", AValue::name("pos"), 20f64.into()),
        ];
        assert_tokens_eq(exp, &comp.call_macro_simple("m", inp).unwrap());
    }
}
