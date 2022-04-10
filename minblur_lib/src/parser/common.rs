use bn_expression::AValue;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{alpha1, alphanumeric1, char, hex_digit1, oct_digit1, one_of},
    combinator::{cut, map, recognize},
    error::context,
    multi::many0,
    number::complete::double,
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};

pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;
pub type ErrType<'a> = nom::error::VerboseError<Span<'a>>;
pub type MyResult<'a, O> = IResult<Span<'a>, O, ErrType<'a>>;

/// Helper function to convert a span to a string
pub fn span_string(span: &Span) -> String {
    String::from(*span.fragment())
}

/// Combinator that matches any/all spaces, INLCUDING newlines
pub fn sp0_nl(input: Span) -> MyResult<Span> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

/// Combinator that matches zero or more spaces EXCLUDING newlines
pub fn sp0(input: Span) -> MyResult<Span> {
    let chars = " \t";
    take_while(move |c| chars.contains(c))(input)
}

/// Combinator that matches one or more spaces EXCLUDING newlines
pub fn sp1(input: Span) -> MyResult<Span> {
    let chars = " \t";
    take_while1(move |c| chars.contains(c))(input)
}

pub fn statement_end(input: Span) -> MyResult<Span> {
    alt((nom::character::complete::line_ending, tag(";")))(input)
}

/// Match a decimal integer
fn number_dec(input: Span) -> MyResult<Span> {
    take_while1(move |c: char| c.is_digit(10))(input)
}

/// Match a hex integer
fn number_hex(input: Span) -> MyResult<Span> {
    preceded(tag("0x"), hex_digit1)(input)
}

/// Match an octal integer
fn number_oct(input: Span) -> MyResult<Span> {
    preceded(tag("0"), oct_digit1)(input)
}

#[allow(dead_code)]
/// Match any type of integer
fn number_any_int(input: Span) -> MyResult<Span> {
    alt((number_dec, number_hex, number_oct))(input)
}

/// Extract a decimal integer
fn parse_number_dec(input: Span) -> MyResult<i64> {
    map(number_dec, |s| (*s).parse::<i64>().unwrap())(input)
}

/// Extract a hex integer
fn parse_number_hex(input: Span) -> MyResult<i64> {
    map(number_hex, |s| i64::from_str_radix(*s, 16).unwrap())(input)
}

/// Extract an octal integer
fn parse_number_oct(input: Span) -> MyResult<i64> {
    map(number_oct, |s| i64::from_str_radix(*s, 8).unwrap())(input)
}

/// Extract any integer type
fn parse_number_any_int(input: Span) -> MyResult<i64> {
    alt((parse_number_dec, parse_number_hex, parse_number_oct))(input)
}

pub fn number_any(input: Span) -> MyResult<f64> {
    alt((double, map(parse_number_any_int, |i| i as f64)))(input)
}

/// Parse an escaped string (excluding the surrounding quotes).
fn match_string<'a>(input: Span<'a>) -> MyResult<'a, Span<'a>> {
    let normal = |inp: Span<'a>| {
        take_while1(|c: char| !c.is_control() && !"\\\"\'\r\n\t\0".contains(c)).parse(inp)
    };
    escaped(normal, '\\', one_of("\"nrt\'\\"))(input)
}

/// Parse a string surrounded by quotes, fails if string parsing fails.
pub fn parse_string(input: Span) -> MyResult<Span> {
    context(
        "string",
        preceded(char('\"'), cut(terminated(match_string, char('\"')))),
    )(input)
}

pub fn identifier_char(input: Span) -> MyResult<Span> {
    alt((alphanumeric1, tag("_"), tag(".")))(input)
}

/// Basic identifier without special characters.
pub fn identifier_basic(input: Span) -> IResult<Span, Span, ErrType> {
    recognize(pair(
        alt((alpha1, tag("_"), recognize(pair(tag("@"), identifier_char)))),
        many0(identifier_char),
    ))(input)
}

/// Recognize a constant (basic identifier prefixed with $)
pub fn identifier_const(input: Span) -> IResult<Span, (char, Span), ErrType> {
    let take_cond = |c: char| (c != '}' && c != '\r' && c != '\n');
    pair(
        char('$'),
        alt((
            delimited(char('{'), take_while1(take_cond), char('}')),
            identifier_basic,
        )),
    )(input)
}

pub fn match_identifier_basic_const(input: Span) -> IResult<Span, Span, ErrType> {
    alt((recognize(identifier_const), identifier_basic))(input)
}

pub fn parse_identifier_basic_const(input: Span) -> IResult<Span, String, ErrType> {
    alt((
        map(identifier_const, |(_, name)| format!("${0}", name)),
        map(identifier_basic, |s| (*s).to_string()),
    ))(input)
}

/// Identifier / variable name with complex names
pub fn identifier(input: Span) -> MyResult<Span> {
    recognize(alt((
        recognize(identifier_const),
        recognize(terminated(identifier_basic, char('!'))),
        identifier_basic,
    )))(input)
}

pub fn _match_value(input: Span) -> MyResult<Span> {
    alt((parse_string, identifier_basic, number_any_int))(input)
}

/// Parse an identifier, number, constant, or constexpr
pub fn parse_value_no_string(input: Span) -> MyResult<AValue> {
    alt((
        map(parse_identifier_basic_const, AValue::name),
        map(number_any, AValue::Num),
    ))(input)
}

/// Parse an expression, either a variable, goto, number, or constant identifier
pub fn parse_value(input: Span) -> MyResult<AValue> {
    alt((
        map(parse_string, |s| AValue::string(*s)),
        map(parse_identifier_basic_const, AValue::name),
        map(number_any, AValue::Num),
    ))(input)
}

/// Returns a nom error if the input string isn't empty
///
/// Param `f` should generate an approriate error given the input
pub fn assert_input_consumed<'a, F, O>(mut f: F) -> impl FnMut(Span<'a>) -> MyResult<'a, ()>
where
    F: Parser<Span<'a>, O, ErrType<'a>>,
{
    move |input| {
        if (*input).is_empty() {
            Ok((input, ()))
        } else {
            let (input, _) = f.parse(input)?;
            Ok((input, ()))
        }
    }
}
