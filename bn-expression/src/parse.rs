use std::{fmt, marker::PhantomData};

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1},
    character::complete::{alpha1, char, hex_digit1, oct_digit1, one_of, space0},
    combinator::{cut, eof, map, recognize},
    error::{context, VerboseErrorKind},
    multi::{many1, separated_list0},
    number::complete::double,
    sequence::{delimited, pair, preceded, separated_pair, terminated},
    Finish, InputTake, InputTakeAtPosition, Parser,
};

use crate::{AValue, Expression, ExpressionOp};

/// A location in the input.
/// Basically values taken from LocatedSpan, but without the string reference.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub struct Position {
    pub line: u32,
    pub column: u32,
}
impl Position {
    pub fn from_span(v: &Span) -> Self {
        Self {
            line: v.location_line(),
            column: v.get_utf8_column() as u32,
        }
    }

    pub fn from_error(e: &ErrType) -> Self {
        Self::from_span(&e.errors[0].0)
    }
}
impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub use nom::error::VerboseError as NomError;
pub type Span<'a> = nom_locate::LocatedSpan<&'a str>;
pub type ErrType<'a> = NomError<Span<'a>>;
pub type IResult<'a, O> = nom::IResult<Span<'a>, O, ErrType<'a>>;

/// Combinator that matches any/all spaces, INLCUDING newlines
pub fn sp0_nl(input: Span) -> IResult<Span> {
    let chars = " \t\r\n";
    take_while(move |c| chars.contains(c))(input)
}

/// Match against "names" which begin with a letter or underscore and contain
/// letters, numbers, or underscores.
pub fn match_name_basic<'a>(input: Span<'a>) -> IResult<'a, Span<'a>> {
    let alphanum = |inp: Span<'a>| {
        inp.split_at_position_complete(move |c: char| !(c.is_alphanumeric() || c == '_'))
    };
    recognize(pair(alt((alpha1, tag("_"))), alphanum))(input)
}

/// Match a decimal integer
fn number_dec(input: Span) -> IResult<Span> {
    take_while1(move |c: char| c.is_digit(10))(input)
}

/// Match a hex integer
fn number_hex(input: Span) -> IResult<Span> {
    preceded(tag("0x"), hex_digit1)(input)
}

/// Match an octal integer
fn number_oct(input: Span) -> IResult<Span> {
    preceded(tag("0"), oct_digit1)(input)
}

/// Extract a decimal integer
fn parse_number_dec(input: Span) -> IResult<i64> {
    map(number_dec, |s| (*s).parse::<i64>().unwrap())(input)
}

/// Extract a hex integer
fn parse_number_hex(input: Span) -> IResult<i64> {
    map(number_hex, |s| i64::from_str_radix(*s, 16).unwrap())(input)
}

/// Extract an octal integer
fn parse_number_oct(input: Span) -> IResult<i64> {
    map(number_oct, |s| i64::from_str_radix(*s, 8).unwrap())(input)
}

/// Extract any integer type
fn parse_number_any_int(input: Span) -> IResult<i64> {
    alt((parse_number_dec, parse_number_hex, parse_number_oct))(input)
}

fn number_any(input: Span) -> IResult<f64> {
    alt((double, map(parse_number_any_int, |i| i as f64)))(input)
}

/// Parse an escaped string (excluding the surrounding quotes).
fn match_string<'a>(input: Span<'a>) -> IResult<'a, Span<'a>> {
    let normal = |inp: Span<'a>| {
        take_while1(|c: char| !c.is_control() && !"\\\"\'\r\n\t\0".contains(c)).parse(inp)
    };
    escaped(normal, '\\', one_of("\"nrt\'\\"))(input)
}

/// Parse a string surrounded by quotes, fails if string parsing fails.
pub fn parse_string(input: Span) -> IResult<Span> {
    context(
        "parsing string",
        preceded(char('\"'), cut(terminated(match_string, char('\"')))),
    )(input)
}

/// Parse something enclosed by parenthesies, returns a failure if the close paren isn't found
pub fn paren_delimited<'a, O1, F>(mut middle: F) -> impl FnMut(Span<'a>) -> IResult<O1>
where
    F: Parser<Span<'a>, O1, ErrType<'a>>,
{
    move |input: Span<'a>| {
        let (input, _) = char('(').parse(input)?;
        let (input, out) = middle.parse(input)?;
        let (input, _) = cut(char(')')).parse(input)?;
        Ok((input, out))
    }
}

pub fn assert_eof(input: Span) -> IResult<()> {
    context("unexpected token", cut(map(eof, |_| ())))(input)
}
pub fn nom_fail<'a>(input: Span<'a>, message: &'static str) -> ErrType<'a> {
    NomError {
        errors: vec![(input, VerboseErrorKind::Context(message))],
    }
}

#[derive(Debug, Clone)]
struct Token<'a, Op> {
    pub pos: Span<'a>,
    pub data: TokenData<'a, Op>,
}

#[derive(Debug, Clone)]
enum TokenData<'a, Op> {
    Name(&'a str),
    String(String),
    Op(Op),
    Num(f64),
    Group(Vec<Token<'a, Op>>),
    Parens(Vec<Token<'a, Op>>),
    Call {
        name: &'a str,
        args: Vec<Token<'a, Op>>,
    },
}
impl<'a, Op: ExpressionOp> TokenData<'a, Op> {
    pub fn with_span(self, span: &Span<'a>) -> Token<'a, Op> {
        Token {
            pos: *span,
            data: self,
        }
    }
    pub fn as_op(&self) -> Option<Op> {
        if let TokenData::Op(op) = self {
            Some(op.clone())
        } else {
            None
        }
    }
    pub fn group_to_parens(self) -> Self {
        match self {
            TokenData::Group(g) => TokenData::Parens(g),
            _ => self,
        }
    }
}

pub trait IExpressionParser<Op: ExpressionOp> {
    fn parse_expression<'a, I: Into<Span<'a>>>(
        &self,
        input: I,
    ) -> Result<Expression<Op>, ErrType<'a>>;
    fn parse_value<'a, I: Into<Span<'a>>>(&self, input: I) -> Result<AValue, ErrType<'a>>;

    /// Parse an expression surrounded by parenthesies. This will error if the expression is not surrounded
    /// by parenthesies but does not expect to consume all input.
    fn parse_paren_expression<'a, I: Into<Span<'a>>>(
        &self,
        input: I,
    ) -> IResult<'a, Expression<Op>>;

    /// Greedy match against the input to try and match the longest expression possible.
    ///
    fn match_expression<'a, I: Into<Span<'a>>>(&self, input: I) -> IResult<'a, Span<'a>>;
}

pub struct ExpressionParser<Op, VName, FName> {
    var_name_parser: VName,
    func_name_parser: FName,
    _op: PhantomData<Op>,
}
impl<Op, VN, FN> IExpressionParser<Op> for ExpressionParser<Op, VN, FN>
where
    Op: ExpressionOp,
    VN: for<'b> nom::Parser<Span<'b>, Span<'b>, ErrType<'b>> + Clone,
    FN: for<'b> nom::Parser<Span<'b>, Span<'b>, ErrType<'b>> + Clone,
{
    fn parse_expression<'a, I: Into<Span<'a>>>(
        &self,
        input: I,
    ) -> Result<Expression<Op>, ErrType<'a>> {
        self.parse_expression_impl(input.into())
    }
    fn parse_value<'a, I: Into<Span<'a>>>(&self, input: I) -> Result<AValue, ErrType<'a>> {
        self.parse_value_impl(input.into())
    }

    fn parse_paren_expression<'a, I: Into<Span<'a>>>(
        &self,
        input: I,
    ) -> IResult<'a, Expression<Op>> {
        self.parse_paren_expression_impl(input.into())
    }

    fn match_expression<'a, I: Into<Span<'a>>>(&self, input: I) -> IResult<'a, Span<'a>> {
        self.match_expression_impl(input.into())
    }
}

impl<Op, VN, FN> ExpressionParser<Op, VN, FN>
where
    Op: ExpressionOp,
    VN: for<'a> nom::Parser<Span<'a>, Span<'a>, ErrType<'a>> + Clone,
    FN: for<'a> nom::Parser<Span<'a>, Span<'a>, ErrType<'a>> + Clone,
{
    pub fn new(var_name_parser: VN, func_name_parser: FN) -> Self {
        Self {
            var_name_parser,
            func_name_parser,
            _op: PhantomData::<Op>,
        }
    }

    fn parse_expression_impl<'a>(&self, input: Span<'a>) -> Result<Expression<Op>, ErrType<'a>> {
        let parse_full = |input| -> IResult<'a, Token<'a, Op>> {
            terminated(
                |input| self.parse_expression_token(input),
                pair(sp0_nl, assert_eof),
            )(input)
        };
        let token: Token<'a, Op> = Self::map_parse_finish(parse_full, input)?;
        self.convert_token(&token)
    }

    fn parse_value_impl<'a>(&self, input: Span<'a>) -> Result<AValue, ErrType<'a>> {
        let parse_full = |input| -> IResult<'a, Token<'a, Op>> {
            terminated(
                |input| self.parse_value_token(input),
                pair(sp0_nl, assert_eof),
            )(input)
        };
        let token = Self::map_parse_finish(parse_full, input)?;
        self.convert_token(&token)?
            .into_value()
            .map_err(|_| nom_fail(input, "expression not a value"))
    }

    fn parse_paren_expression_impl<'a>(&self, input: Span<'a>) -> IResult<'a, Expression<Op>> {
        let (input, token) = self.parse_paren_expression_token(input)?;
        let expr = self.convert_token(&token).map_err(nom::Err::Failure)?;
        Ok((input, expr))
    }

    /// See [`IExpressionParser::match_expression`]
    fn match_expression_impl<'a>(&self, input: Span<'a>) -> IResult<'a, Span<'a>> {
        recognize(|inp| self.parse_expression_token(inp))(input)
    }

    fn map_parse_finish<'a, F, O>(mut f: F, input: Span<'a>) -> Result<O, ErrType<'a>>
    where
        F: Parser<Span<'a>, O, ErrType<'a>>,
    {
        f.parse(input).finish().map(|v| v.1)
    }

    /// Parse a string, name, or number
    fn parse_value_token<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        alt((
            map(parse_string, |s| TokenData::String(String::from(*s))),
            map(|inp| self.parse_name(inp), |s| TokenData::Name(*s)),
            map(number_any, TokenData::Num),
        ))
        .map(|d| d.with_span(&input))
        .parse(input)
    }

    fn parse_function_call<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        // Parse a list of arguments separated by commas and surrounded by parens
        let middle = |inp: Span<'a>| -> IResult<Vec<Token<'a, Op>>> {
            separated_list0(
                char(','),
                delimited(sp0_nl, |inp| self.parse_expression_token(inp), sp0_nl),
            )(inp)
        };
        separated_pair(
            |inp| self.parse_func_name(inp),
            space0,
            paren_delimited(middle),
        )
        .map(|(name, args)| TokenData::Call { name: *name, args }.with_span(&name))
        .parse(input)
    }

    /// Parse an expression surrounded by parens
    fn parse_paren_expression_token<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        let middle = map(
            |inp| self.parse_expression_token(inp),
            |mut t| {
                t.data = t.data.group_to_parens();
                t
            },
        );
        paren_delimited(middle)(input)
    }

    /// Parse a single token: parenthesies, a simple value, or a function call
    fn parse_basic_term<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        delimited(
            space0,
            alt((
                // Function call
                |inp| self.parse_function_call(inp),
                // Parens
                |inp| self.parse_paren_expression_token(inp),
                // An operator
                |inp| self.parse_op(inp),
                // Basic value
                |inp| self.parse_value_token(inp),
            )),
            space0,
        )(input)
    }

    #[inline]
    fn parse_func_name<'a>(&self, input: Span<'a>) -> IResult<'a, Span<'a>> {
        self.func_name_parser.clone().parse(input)
    }
    #[inline]
    fn parse_name<'a>(&self, input: Span<'a>) -> IResult<'a, Span<'a>> {
        self.var_name_parser.clone().parse(input)
    }

    fn parse_op<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        let (remain, op) = Op::match_op(*input).map_err(|e| {
            let e = ErrType {
                errors: vec![(input, VerboseErrorKind::Context(e))],
            };
            nom::Err::Error(e)
        })?;
        let (input, used) = input.take_split(input.len() - remain.len());
        Ok((input, TokenData::Op(op).with_span(&used)))
    }

    /// Parse an entire expression
    fn parse_expression_token<'a>(&self, input: Span<'a>) -> IResult<'a, Token<'a, Op>> {
        map(many1(|inp| self.parse_basic_term(inp)), |tokens| {
            TokenData::Group(tokens).with_span(&input)
        })(input)
    }

    fn convert_token<'a>(&self, token: &Token<'a, Op>) -> Result<Expression<Op>, ErrType<'a>> {
        match &token.data {
            TokenData::Name(name) => Ok(AValue::name(name).into()),
            TokenData::String(s) => Ok(AValue::string(s).into()),
            TokenData::Num(n) => Ok(AValue::Num(*n).into()),
            TokenData::Group(tokens) => self.convert_token_group(token.pos, tokens),
            TokenData::Parens(tokens) => self.convert_token_group(token.pos, tokens),
            TokenData::Call { name, args } => {
                let new_args: Result<Vec<_>, _> =
                    args.iter().map(|tok| self.convert_token(tok)).collect();
                let new_args = new_args?;
                Ok(Expression::new_call(name, new_args))
            }
            TokenData::Op(_) => Err(nom_fail(token.pos, "unexpected operator")),
        }
    }

    fn convert_token_group<'a>(
        &self,
        pos: Span<'a>,
        tokens: &[Token<'a, Op>],
    ) -> Result<Expression<Op>, ErrType<'a>> {
        // Short-circut if group is only one token
        if tokens.len() == 1 {
            return self.convert_token(&tokens[0]);
        }
        // Error if group is no tokens
        if tokens.is_empty() {
            return Err(nom_fail(pos, "empty token group"));
        }
        // Find last lowest-precedence operator
        let (index, _) = tokens
            .iter()
            .enumerate()
            .fold((usize::MAX, 0), |accum, elem| {
                // If it's an Op AND has lower precedence, use this op instead
                if let Some(op) = elem.1.data.as_op() {
                    let prec = op.precedence();
                    if prec >= accum.1 {
                        return (elem.0, prec);
                    }
                }
                accum
            });
        // If we didn't find one... what?
        if index == usize::MAX {
            println!("{:?}", tokens);
            return Err(nom_fail(pos, "no operator in group"));
        }
        let top_op = tokens[index].data.as_op().unwrap();
        if top_op.is_unary() {
            // If unary: left MUST be empty
            if index > 0 {
                todo!()
            } else {
                let right = self.convert_token_group(tokens[0].pos, &tokens[1..])?;
                Ok(Expression::Unary(top_op, Box::new(right)))
            }
        } else {
            // op is binary, take left and right
            let pos = tokens[index].pos;
            let left = self.convert_token_group(pos, &tokens[..index])?;
            let right = self.convert_token_group(pos, &tokens[index + 1..])?;
            Ok(Expression::Binary(top_op, Box::new(left), Box::new(right)))
        }
    }
}
