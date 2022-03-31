use instruction::{parse_instruction_arg_string, InstructionParser};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, tag_no_case, take_until},
    character::complete::{char, one_of},
    combinator::{cut, eof, map, peek, recognize, value},
    error::context,
    multi::{many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, Needed, Parser, Slice,
};

use super::{
    common::{
        assert_input_consumed, identifier, identifier_basic, parse_string, sp0, sp0_nl, sp1,
        span_string, ErrType, MyResult, Span,
    },
    consts::*,
    statement::*,
};
use crate::common::{macros::prcall, string_cache::*};
use crate::compiler::instruction::{InstValue, Instruction};

/// Match a comment to the end of the line and return ()
fn eol_comment(input: Span) -> MyResult<Span> {
    recognize(pair(char(COMMENT_CHAR), is_not("\n\r")))(input)
}

/// Match a block comment. Does NOT support nested comments.
fn block_comment(input: Span) -> MyResult<Span> {
    recognize(tuple((
        tag(COMMENT_BEGIN),
        take_until(COMMENT_END),
        tag(COMMENT_END),
    )))(input)
}

/// Match a pair of balanced `open` and `close` characters, returns the entire contents as a string.
///
fn balanced_braces<'a>(open: char, close: char) -> impl FnMut(Span<'a>) -> MyResult<Span<'a>> {
    let balanced = move |input: Span<'a>| -> MyResult<Span<'a>> {
        let mut depth = 1;
        for (i, c) in input.chars().enumerate() {
            if c == open {
                depth += 1;
            }
            if c == close {
                depth -= 1;
            }
            if depth == 0 {
                let remain = input.slice(i..);
                let result = input.slice(0..i);
                return Ok((remain, result));
            }
        }
        Err(nom::Err::<ErrType>::Incomplete(Needed::Unknown))
    };
    delimited(char(open), balanced, char(close))
}

/// Match pair of common balanced braces, returning the contents
///
fn balanced_braces_common(input: Span) -> MyResult<Span> {
    alt((
        balanced_braces('{', '}'),
        balanced_braces('(', ')'),
        balanced_braces('[', ']'),
    ))(input)
}

/// Parse an argument list (arg1, arg2, arg3...)
fn argument_list(input: Span) -> MyResult<Vec<Span>> {
    delimited(
        char('('),
        separated_list0(char(','), delimited(sp0_nl, identifier, sp0_nl)),
        char(')'),
    )(input)
}

/// Parse a condition statement that lasts until the end of the line
fn condition_eol_or_comment(input: Span) -> MyResult<Span> {
    is_not("\r\n#")(input)
}

/// Parse until the end of the line, or a line comment
/// TODO: allow backslash to escape EOL
fn until_eol_or_comment(input: Span) -> MyResult<Span> {
    is_not("\r\n#")(input)
}

/// Parse until either a comment, semicolon, or newline
fn until_end_of_statement(input: Span) -> MyResult<Span> {
    terminated(
        sp0,
        peek(alt((value((), one_of(";#\r\n")), value((), eof)))),
    )(input)
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Token {
    pub line: u32,
    pub column: u32,
    pub data: TokenData,
}
impl Token {
    pub fn new(line: u32, column: u32, data: TokenData) -> Self {
        Self { line, column, data }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum TokenData {
    DirectiveToken(DirectiveToken),
    Instruction(Instruction),
    Label(Label),
    MacroCall {
        name: CaString,
        content: String,
    },
    OutputComment(String),
    /// Comment, EOL, etc.
    Skip,
    /// End of a statement (newline or ;)
    EndOfStatement,
}
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum DirectiveToken {
    Define { key: String, value: InstValue },
    Include { path: String },
    LoadExtension { ext_type: String, path: String },
    InlineExtension { ext_type: String, code: String },
    BeginMacro { name: String, args: Vec<String> },
    EndMacro,
    Option { key: String, value: String },
    If { cond: String },
    Else,
    ElseIf { cond: String },
    EndIf,
}

pub struct TokenParser {
    pub string_cache: StringCache,
    instr_parser: InstructionParser,
}
impl TokenParser {
    pub fn new(string_cache: &StringCache) -> Self {
        Self {
            string_cache: string_cache.clone(),
            instr_parser: InstructionParser::new(string_cache),
        }
    }
}
impl<'a> TokenParser {
    fn parse_token_instruction(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        map(
            terminated(
                context("instruction", prcall!(self.instr_parser.parse_instruction)),
                until_end_of_statement,
            ),
            TokenData::Instruction,
        )(input)
    }

    fn parse_token_macro_call(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        fn trim_right_mut(mut s: String) -> String {
            s.truncate(s.trim_end().len());
            s
        }

        map(
            pair(
                terminated(identifier_basic, char('!')),
                alt((
                    preceded(sp0, balanced_braces_common),
                    preceded(sp1, until_eol_or_comment),
                )),
            ),
            |f| TokenData::MacroCall {
                name: self.string_cache.get(*f.0),
                content: trim_right_mut(span_string(&f.1)),
            },
        )(input)
    }

    fn parse_token_label(input: Span) -> MyResult<TokenData> {
        map(terminated(identifier_basic, char(':')), |f| {
            TokenData::Label(Label::new(*f))
        })
        .parse(input)
    }

    fn parse_token_output_comment(input: Span) -> MyResult<TokenData> {
        preceded(peek(tag("#!")), eol_comment)
            .map(|s| TokenData::OutputComment(s.to_string()))
            .parse(input)
    }

    fn parse_token_skip(input: Span) -> MyResult<TokenData> {
        map(alt((sp1, eol_comment, block_comment)), |_| TokenData::Skip)(input)
    }

    fn parse_token_end_of_statement(input: Span) -> MyResult<TokenData> {
        map(one_of(";\r\n"), |_| TokenData::EndOfStatement)(input)
    }

    /// Parse a directive. Directives begin with "." and
    pub fn parse_directive_token(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        let map_define = tuple((
            value((), pair(tag_no_case(DIRECTIVE_DEFINE), sp1)),
            identifier_basic,
            value((), sp1),
            |inp| parse_instruction_arg_string(&self.string_cache, inp),
        ))
        .map(|opt: ((), Span, (), InstValue)| DirectiveToken::Define {
            key: span_string(&opt.1),
            value: opt.3,
        });

        let parse_include = preceded(preceded(tag_no_case(DIRECTIVE_INCLUDE), sp1), parse_string);
        let map_include = map(parse_include, |s| DirectiveToken::Include {
            path: span_string(&s),
        });

        let parse_macro = preceded(
            preceded(tag_no_case(DIRECTIVE_MACRO), sp1),
            tuple((identifier_basic, sp0, argument_list)),
        );
        let map_macro = map(parse_macro, |tup: (Span, Span, Vec<Span>)| {
            DirectiveToken::BeginMacro {
                name: span_string(&tup.0),
                args: tup.2.iter().map(span_string).collect(),
            }
        });

        let map_end_macro = value(DirectiveToken::EndMacro, tag_no_case(DIRECTIVE_ENDMACRO));

        let parse_option = preceded(
            preceded(tag_no_case(DIRECTIVE_OPTION), sp1),
            separated_pair(identifier_basic, sp1, parse_string),
        );
        let map_option = map(parse_option, |opt| DirectiveToken::Option {
            key: span_string(&opt.0),
            value: span_string(&opt.1),
        });

        let parse_if = preceded(
            preceded(tag_no_case(DIRECTIVE_IF), sp1),
            condition_eol_or_comment,
        );
        let map_if = map(parse_if, |f| DirectiveToken::If {
            cond: span_string(&f),
        });

        let map_else = value(DirectiveToken::Else, tag_no_case(DIRECTIVE_ELSE));

        let map_elseif = map(
            preceded(
                preceded(tag_no_case(DIRECTIVE_ELSEIF), sp1),
                condition_eol_or_comment,
            ),
            |cond| DirectiveToken::ElseIf {
                cond: span_string(&cond),
            },
        );

        let map_endif = value(DirectiveToken::EndIf, tag_no_case(DIRECTIVE_ENDIF));

        let alt_directives = alt((
            map_define,
            map_include,
            map_macro,
            map_end_macro,
            map_option,
            map_if,
            map_else,
            map_elseif,
            map_endif,
        ));

        preceded(
            char(DIRECTIVE_PREFIX),
            context("invalid directive", cut(alt_directives)),
        )
        .map(TokenData::DirectiveToken)
        .parse(input)
    }

    /// Parse a single line (or multi-line block in some cases)
    pub fn parse_next_token(&self, input: Span<'a>) -> MyResult<'a, Token> {
        context(
            "expected label, instruction, or directive",
            alt((
                prcall!(self.parse_directive_token),
                Self::parse_token_label,
                prcall!(self.parse_token_macro_call),
                prcall!(self.parse_token_instruction),
                Self::parse_token_output_comment,
                Self::parse_token_skip,
                Self::parse_token_end_of_statement,
            )),
        )
        .map(|tok: TokenData| Token::new(input.location_line(), input.get_column() as u32, tok))
        .parse(input)
    }

    /// Parse and tokenize an entire assembly file.
    pub fn parse_all_tokens(&self, input: Span<'a>) -> MyResult<'a, Vec<Token>> {
        let (input, output) = many1(prcall!(self.parse_next_token))(input)?;
        let (input, _) = assert_input_consumed(prcall!(self.parse_next_token)).parse(input)?;
        Ok((input, output))
    }

    pub fn parse_str_input(&self, input: &'a str) -> Result<Vec<Token>, ErrType<'a>> {
        self.parse_all_tokens(Span::new(input))
            .finish()
            .map(|out| out.1)
    }
}

pub mod instruction {
    use bn_expression::basic::BasicOp;
    use bn_expression::parse::{match_name_basic, ExpressionParser, IExpressionParser};
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        character::complete::char,
        combinator::{cut, map},
        error::{context, make_error, ErrorKind as NomErrorKind},
        sequence::{delimited, preceded, separated_pair},
        InputTakeAtPosition, Parser,
    };

    use crate::common::macros::prcall;
    use crate::common::string_cache::StringCache;
    use crate::common::tuple_helper::TupleToArray;
    use crate::compiler::instruction::*;
    use crate::parser::common::{
        identifier_basic, parse_value, parse_value_no_string, sp1, ErrType, MyResult, Span,
    };

    // Since the macro is local-only we don't have to worry about full-use-paths.
    macro_rules! fn_parse_instr {
        (
            $self:ident;
            $fn_name:ident,
            $map_type:ty,
            $name:literal,
            ( $($arg:ident),+ $(,)? )
        ) => {
            pub fn $fn_name (&$self, input: Span<'a>) -> MyResult<'a, $map_type> {
                preceded(
                    tag($name),
                    map(
                        nom::sequence::tuple(( $(prcall!($self.$arg), )+ )),
                        |v| <$map_type>::from(v.to_array()),
                    )
                )(input)
            }
        };
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct InstructionParser {
        string_cache: StringCache,
    }
    impl InstructionParser {
        pub fn new(cache: &StringCache) -> Self {
            Self {
                string_cache: cache.clone(),
            }
        }
    }
    impl<'a> Parser<Span<'a>, Instruction, ErrType<'a>> for InstructionParser {
        fn parse(&mut self, input: Span<'a>) -> MyResult<'a, Instruction> {
            self.parse_instruction(input)
        }
    }
    impl<'a> InstructionParser {
        fn_parse_instr!(self; parse_i_read, InstructionRead, "read", 
            ( next_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_write, InstructionWrite, "write",
            ( next_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_draw, InstructionDraw, "draw",
            ( parse_enum_arg, next_arg, next_arg, next_arg, next_arg, next_arg, next_arg )
        );
        fn_parse_instr!(self; parse_i_print, InstructionPrint, "print",
            ( next_arg_string ) );
        fn_parse_instr!(self; parse_i_draw_flush, InstructionDrawFlush, "drawflush",
            ( next_arg ) );
        fn_parse_instr!(self; parse_i_print_flush, InstructionPrintFlush, "printflush",
            ( next_arg ) );
        fn_parse_instr!(self; parse_i_get_link, InstructionGetLink, "getlink",
            ( next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_control, InstructionControl, "control",
            ( parse_enum_arg, next_arg, next_arg, next_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_radar, InstructionRadar, "radar",
            (
                parse_enum_arg, parse_enum_arg, parse_enum_arg,
                parse_enum_arg, next_arg, next_arg, next_arg,
            )
        );
        fn_parse_instr!(self; parse_i_sensor, InstructionSensor, "sensor",
            ( next_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_set, InstructionSet, "set",
            ( next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_op, InstructionOp, "op",
            ( parse_enum_arg, next_arg, next_arg, next_arg ) );
        pub fn parse_i_end(&self, input: Span<'a>) -> MyResult<'a, InstructionEnd> {
            map(tag("end"), |_| InstructionEnd {})(input)
        }
        fn_parse_instr!(self; parse_i_jump, InstructionJump, "jump",
            ( next_arg, parse_enum_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_unit_bind, InstructionUnitBind, "ubind",
            ( next_arg ) );
        fn_parse_instr!(self; parse_i_unit_control, InstructionUnitControl, "ucontrol",
            ( parse_enum_arg, next_arg, next_arg, next_arg, next_arg, next_arg ) );
        fn_parse_instr!(self; parse_i_unit_radar, InstructionUnitRadar, "uradar",
            (
                parse_enum_arg, parse_enum_arg, parse_enum_arg,
                parse_enum_arg, next_arg, next_arg, next_arg,
            )
        );
        fn_parse_instr!(self; parse_i_unit_locate, InstructionUnitLocate, "ulocate",
            (
                parse_enum_arg, parse_enum_arg,
                next_arg, next_arg, next_arg, next_arg, next_arg, next_arg,
            )
        );

        pub fn parse_instruction(&self, input: Span<'a>) -> MyResult<'a, Instruction> {
            alt((
                map(prcall!(self.parse_i_read), Into::into),
                map(prcall!(self.parse_i_write), Into::into),
                // These are before print and draw to make sure the longer names are properly detected
                map(prcall!(self.parse_i_draw_flush), Into::into),
                map(prcall!(self.parse_i_print_flush), Into::into),
                map(prcall!(self.parse_i_print), Into::into),
                map(prcall!(self.parse_i_draw), Into::into),
                map(prcall!(self.parse_i_get_link), Into::into),
                map(prcall!(self.parse_i_control), Into::into),
                map(prcall!(self.parse_i_radar), Into::into),
                map(prcall!(self.parse_i_sensor), Into::into),
                map(prcall!(self.parse_i_set), Into::into),
                map(prcall!(self.parse_i_op), Into::into),
                map(prcall!(self.parse_i_end), Into::into),
                map(prcall!(self.parse_i_jump), Into::into),
                map(prcall!(self.parse_i_unit_bind), Into::into),
                map(prcall!(self.parse_i_unit_control), Into::into),
                map(prcall!(self.parse_i_unit_radar), Into::into),
                map(prcall!(self.parse_i_unit_locate), Into::into),
            ))(input)
        }

        /// Wrap an enum to parse either a constant or name
        pub fn parse_enum_arg(&self, input: Span<'a>) -> MyResult<'a, InstValue> {
            preceded(
                sp1,
                alt((
                    |inp| parse_instruction_value_const(&self.string_cache, inp),
                    map(match_name, |s| {
                        InstValue::EnumName(self.string_cache.get(*s))
                    }),
                )),
            )(input)
        }

        fn next_arg(&self, input: Span<'a>) -> MyResult<'a, InstValue> {
            preceded(
                sp1,
                alt((
                    |inp| parse_instruction_value_const(&self.string_cache, inp),
                    map(parse_value_no_string, InstValue::Value),
                )),
            )(input)
        }

        fn next_arg_string(&self, input: Span<'a>) -> MyResult<'a, InstValue> {
            preceded(
                sp1,
                alt((
                    |inp| parse_instruction_value_const(&self.string_cache, inp),
                    map(parse_value, InstValue::Value),
                )),
            )(input)
        }
    }

    //
    // Helper functions
    //

    /// Match an instruction name
    pub fn match_name(input: Span) -> MyResult<Span> {
        input.split_at_position1_complete(
            |c| !(c.is_alphanumeric() || c == '_'),
            NomErrorKind::AlphaNumeric,
        )
    }

    pub fn parse_instruction_value_const<'a>(
        string_cache: &'_ StringCache,
        input: Span<'a>,
    ) -> MyResult<'a, InstValue> {
        let parse_const_expression = |input| -> MyResult<'a, InstValue> {
            let take_cond = |c: char| (c != '}' && c != '\r' && c != '\n');
            delimited(char('{'), take_while1(take_cond), cut(char('}')))
                .and_then(|input| {
                    // TODO: better error conversion, maybe custom error type?
                    let expr = new_expr_parser()
                        .parse_expression(input)
                        .map_err(nom::Err::Failure)?;
                    Ok(("".into(), InstValue::new_expr(expr)))
                })
                .parse(input)
        };

        let parse_quick_const = map(
            separated_pair(match_name, tag(":"), identifier_basic),
            |(key, value)| InstValue::QuickConstExpr(string_cache.get(*key), (*value).into()),
        );

        preceded(
            char('$'),
            alt((
                // Const in ${}
                context("const expression", parse_const_expression),
                // "quick const-expr" <quick-name> ":" <quick-value>
                context("quick-const expression", parse_quick_const),
                // Simple const name
                map(identifier_basic, |v| InstValue::Const((*v).into())),
                // Error b/c we have a $ but we failed to parse a const or constexpr
                |inp| Err(nom::Err::Error(make_error(inp, NomErrorKind::Fail))),
            )),
        )
        .parse(input)
    }

    fn new_expr_parser() -> impl IExpressionParser<BasicOp> {
        ExpressionParser::<BasicOp, _, _>::new(match_name_basic, match_name_basic)
    }

    pub fn parse_instruction_arg_string<'a>(
        string_cache: &'_ StringCache,
        input: Span<'a>,
    ) -> MyResult<'a, InstValue> {
        alt((
            |inp| parse_instruction_value_const(string_cache, inp),
            map(parse_value, InstValue::Value),
        ))
        .parse(input)
    }
}

#[cfg(test)]
pub mod test {
    use insta;
    use nom::Finish;

    use super::*;

    fn parse_into_tokens<'a>(
        string_cache: &StringCache,
        inp: &'a str,
    ) -> Result<(Span<'a>, Vec<Token>), ErrType<'a>> {
        TokenParser::new(string_cache)
            .parse_all_tokens(Span::new(inp))
            .finish()
    }

    #[test]
    fn directive_macro_begin() {
        let string_cache = StringCache::new();
        let inp = ".macro abc(arg1, arg2 ) ";
        let out = parse_into_tokens(&string_cache, inp).unwrap().1;
        insta::assert_debug_snapshot!(out);
    }

    #[test]
    fn multiline_basic() {
        let string_cache = StringCache::new();
        let inp = "start: \n  set r1 0\n  op add r2 r1 5\n  m! x = r2 * 3\n  op mul y r1 5\n";
        let out = parse_into_tokens(&string_cache, inp).unwrap().1;
        insta::assert_debug_snapshot!(out);
    }
}
