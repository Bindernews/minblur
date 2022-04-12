use bn_expression::basic::BasicOp;
use bn_expression::parse::{match_name_basic, nom_fail, ExpressionParser, IExpressionParser};

use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while, take_while1},
    character::complete::{char, one_of},
    combinator::{cut, map, peek, value},
    error::context,
    multi::{many0, many1, separated_list0},
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    Finish, Parser,
};

use super::{common::*, consts::*, statement::*};
use crate::common::{macros::prcall, string_cache::*};
use crate::compiler::instruction::{InstValue, Instruction, InstructionKind};

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

/// Returns the span up until a statement terminator
/// TODO: allow backslash to escape EOL
fn find_end_of_statement(input: Span) -> MyResult<Span> {
    take_while(|c: char| !";#\r\n".contains(c)).parse(input)
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

#[derive(Debug, Clone)]
pub struct ParseContext {
    pub string_cache: StringCache,
}
impl ParseContext {
    pub fn new(cache: StringCache) -> Self {
        Self {
            string_cache: cache,
        }
    }
}

pub struct TokenParser {
    pub string_cache: StringCache,
    ctx: ParseContext,
}
impl TokenParser {
    pub fn new(string_cache: &StringCache) -> Self {
        Self {
            string_cache: string_cache.clone(),
            ctx: ParseContext::new(string_cache.clone()),
        }
    }
}
impl<'a> TokenParser {
    fn parse_token_instruction(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        let parse_instr = |inp| parse_instruction(&self.ctx, inp);
        map(parse_instr, TokenData::Instruction)(input)
    }

    fn parse_token_macro_call(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        fn trim_right_mut(mut s: String) -> String {
            s.truncate(s.trim_end().len());
            s
        }

        pair(
            terminated(identifier_mlog, char('!')),
            alt((
                preceded(sp0, balanced_braces_common),
                preceded(sp1, find_end_of_statement),
            )),
        )
        .map(|f| TokenData::MacroCall {
            name: self.string_cache.get(*f.0),
            content: trim_right_mut(span_string(&f.1)),
        })
        .parse(input)
    }

    fn parse_token_label(input: Span) -> MyResult<TokenData> {
        map(terminated(identifier_mlog, char(':')), |f| {
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
        map(one_of(";#\r\n"), |_| TokenData::EndOfStatement)(input)
    }

    /// Parse a directive. Directives begin with "." and
    pub fn parse_token_directive(&self, input: Span<'a>) -> MyResult<'a, TokenData> {
        fn match_dir_name<'b>(name: &'static str) -> impl Parser<Span<'b>, (), ErrType<'b>> {
            value((), pair(tag_no_case(name), sp0))
        }

        let parse_define = tuple((
            match_dir_name(DIRECTIVE_DEFINE),
            identifier_mlog,
            value((), sp1),
            |inp| parse_instruction_arg_string(&self.ctx, inp),
        ))
        .map(|opt: ((), Span, (), InstValue)| DirectiveToken::Define {
            key: span_string(&opt.1),
            value: opt.3,
        });

        let parse_include = map(
            preceded(match_dir_name(DIRECTIVE_INCLUDE), parse_string),
            |s| DirectiveToken::Include {
                path: span_string(&s),
            },
        );

        let parse_macro = map(
            tuple((
                match_dir_name(DIRECTIVE_MACRO),
                identifier_mlog,
                sp0,
                argument_list,
            )),
            |tup: ((), Span, Span, Vec<Span>)| DirectiveToken::BeginMacro {
                name: span_string(&tup.1),
                args: tup.3.iter().map(span_string).collect(),
            },
        );

        let parse_end_macro = value(DirectiveToken::EndMacro, tag_no_case(DIRECTIVE_ENDMACRO));

        let parse_option = preceded(
            match_dir_name(DIRECTIVE_OPTION),
            separated_pair(identifier_mlog, sp1, alt((parse_string, identifier_mlog))),
        )
        .map(|opt| DirectiveToken::Option {
            key: span_string(&opt.0),
            value: span_string(&opt.1),
        });

        let parse_if = preceded(match_dir_name(DIRECTIVE_IF), find_end_of_statement).map(|f| {
            DirectiveToken::If {
                cond: span_string(&f),
            }
        });

        let parse_else = value(DirectiveToken::Else, tag_no_case(DIRECTIVE_ELSE));

        let parse_elseif = map(
            preceded(match_dir_name(DIRECTIVE_ELSEIF), find_end_of_statement),
            |cond| DirectiveToken::ElseIf {
                cond: span_string(&cond),
            },
        );

        let parse_endif = value(DirectiveToken::EndIf, tag_no_case(DIRECTIVE_ENDIF));

        let alt_directives = alt((
            parse_define,
            parse_include,
            parse_macro,
            parse_end_macro,
            parse_option,
            parse_if,
            parse_else,
            parse_elseif,
            parse_endif,
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
        println!("parse_next_token: {:?}", *input);
        context(
            "expected label, instruction, or directive",
            alt((
                prcall!(self.parse_token_directive),
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

pub fn parse_instr_kind<'b>(
    ctx: &ParseContext,
    kind: InstructionKind,
) -> impl FnMut(Span<'b>) -> MyResult<'b, Instruction> + '_ {
    let next_arg =
        move |input: Span<'b>| preceded(sp1, |inp| parse_instruction_arg_string(ctx, inp))(input);

    move |input: Span<'b>| {
        let input0 = input;
        let (input, _) = tag(kind.name()).parse(input)?;
        let (input, args) = cut(many0(next_arg)).parse(input)?;
        let instr = kind.create_with_args(args).map_err(|_| {
            // TODO better nom error type that allows dynamic context
            nom::Err::Failure(nom_fail(
                input0,
                "instruction has incorrect number of arguments",
            ))
        })?;
        Ok((input, instr))
    }
}

pub fn parse_instruction<'b>(ctx: &ParseContext, input: Span<'b>) -> MyResult<'b, Instruction> {
    macro_rules! build_alt {
        ( [$($k:ident),*] ) => {
            alt((
                $( parse_instr_kind(ctx, InstructionKind::$k), )*
            ))(input)
        };
    }
    // PrintFlush and DrawFlush come before Print and Draw so that the longer names
    // are tested first, otherwise `draw` would error while trying to parse `drawflush`.
    build_alt!([
        DrawFlush,
        PrintFlush,
        Read,
        Write,
        Draw,
        Print,
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
        UnitLocate
    ])
}

fn parse_const_expression<'a>(_ctx: &ParseContext, input: Span<'a>) -> MyResult<'a, InstValue> {
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
}

pub fn parse_instruction_value_const<'a>(
    ctx: &ParseContext,
    input: Span<'a>,
) -> MyResult<'a, InstValue> {
    let parse_const_expr = |inp| parse_const_expression(ctx, inp);

    let parse_quick_const = map(
        separated_pair(ascii_identifier, tag(":"), identifier_mlog),
        |(key, value)| InstValue::QuickConstExpr(ctx.string_cache.get(*key), (*value).into()),
    );

    let parse_basic = identifier_mlog.map(|v| InstValue::Const(ctx.string_cache.get(*v)));

    preceded(
        char('$'),
        cut(alt((
            // Const in ${}
            context("const expression", parse_const_expr),
            // "quick const-expr" <quick-name> ":" <quick-value>
            context("quick-const expression", parse_quick_const),
            // Simple const name
            context("const name", parse_basic),
        ))),
    )
    .parse(input)
}

fn new_expr_parser() -> impl IExpressionParser<BasicOp> {
    ExpressionParser::<BasicOp, _, _>::new(match_name_basic, match_name_basic)
}

pub fn parse_instruction_arg_string<'a>(
    ctx: &ParseContext,
    input: Span<'a>,
) -> MyResult<'a, InstValue> {
    alt((
        |inp| parse_instruction_value_const(ctx, inp),
        map(ascii_identifier, |s| {
            InstValue::EnumName(ctx.string_cache.get(*s))
        }),
        map(parse_value, InstValue::Value),
    ))
    .parse(input)
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
