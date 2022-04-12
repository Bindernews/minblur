use std::rc::Rc;

use bn_expression::{parse::Position, ParseError};

use crate::{compiler::instruction::InstValue, compiler::macros::prelude::*};

#[derive(Debug, thiserror::Error)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize), serde(tag = "kind"))]
pub enum EvalMacroError {
    #[error(transparent)]
    Parse {
        #[from]
        parse: ParseError,
    },
}
impl DynamicError for EvalMacroError {}

#[derive(Debug, Hash)]
pub struct EvalMacro {}
impl EvalMacro {
    pub fn new() -> Self {
        Self {}
    }
}
impl MacroHandler for EvalMacro {
    fn generate(
        &mut self,
        mut ctx: CompilerEnv,
        source: Source,
        _name: &str,
        content: &str,
    ) -> MacroResult {
        // parse input into pieces
        let pieces = parse::Parser::new(&ctx.string_cache())
            .parse_content_str(content)
            .map_err(EvalMacroError::from)?;
        // generate output text
        let mut output = String::new();
        for piece in pieces {
            match piece.data {
                PieceData::Text(s) => {
                    output.push_str(s);
                }
                PieceData::InstValue(value) => {
                    // When doing an offset we want 0-based lines/columns instead of 1-based
                    let source2 = source
                        .clone()
                        .offset_position(piece.pos.line as i32 - 1, piece.pos.column as i32 - 1);
                    let value2 = ctx.get_eval_context(&source2, true).simplify(value)?;
                    output.push_str(&format!("{}", value2));
                }
            }
        }
        // parse new output into statements
        let mut ctx2 = CompilerEnv::with_parent(ctx);
        let tokens = ctx2.parse(&output, &source.name, Some(Rc::new(source.clone())))?;
        Ok(tokens)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Piece<'a> {
    pub pos: Position,
    pub data: PieceData<'a>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum PieceData<'a> {
    /// A piece of the input text
    Text(&'a str),
    /// InstValue
    InstValue(InstValue),
}
impl<'a> From<&'a str> for PieceData<'a> {
    fn from(v: &'a str) -> Self {
        Self::Text(v)
    }
}
impl<'a> From<InstValue> for PieceData<'a> {
    fn from(v: InstValue) -> Self {
        Self::InstValue(v)
    }
}

mod parse {
    use bn_expression::{parse::Position, ParseError};
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while1},
        combinator::map,
        multi::many1,
        sequence::terminated,
        Finish, Parser as NomParser,
    };

    use super::{Piece, PieceData};
    use crate::{
        common::{macros::prcall, string_cache::StringCache},
        parser::{
            common::{assert_input_consumed, MyResult, Span},
            parse_instruction_value_const, ParseContext,
        },
    };

    pub struct Parser {
        ctx: ParseContext,
    }
    impl<'a> Parser {
        pub fn new(string_cache: &StringCache) -> Self {
            Self {
                ctx: ParseContext::new(string_cache.clone()),
            }
        }

        pub fn next_text_piece(&self, input: Span<'a>) -> MyResult<'a, PieceData<'a>> {
            map(take_while1(|c| c != '$'), |v: Span<'a>| PieceData::Text(*v)).parse(input)
        }

        pub fn next_value_piece(&self, input: Span<'a>) -> MyResult<'a, PieceData<'a>> {
            let (input, value) = parse_instruction_value_const(&self.ctx, input)?;
            Ok((input, PieceData::InstValue(value)))
        }

        pub fn next_piece(&self, input: Span<'a>) -> MyResult<'a, Piece<'a>> {
            alt((
                map(tag("$$"), |v: Span<'a>| PieceData::Text(&(*v)[0..1])),
                |inp| self.next_value_piece(inp),
                |inp| self.next_text_piece(inp),
            ))
            .parse(input)
            .map(|v| {
                let piece = Piece {
                    pos: Position::from_span(&input),
                    data: v.1,
                };
                (v.0, piece)
            })
        }

        pub fn parse_content(&self, input: Span<'a>) -> MyResult<'a, Vec<Piece<'a>>> {
            terminated(
                many1(prcall!(self.next_piece)),
                assert_input_consumed(prcall!(self.next_piece)),
            )
            .parse(input)
        }

        pub fn parse_content_str(&self, input: &'a str) -> Result<Vec<Piece<'a>>, ParseError> {
            self.parse_content(Span::new(input))
                .finish()
                .map(|v| v.1)
                .map_err(ParseError::from)
        }
    }
}

#[cfg(test)]
mod test {
    use nom::Finish;

    use super::*;
    use crate::{
        common::string_cache::{CaString, StringCache},
        compiler::{instruction::InstValue, CompilerEnv, EnvMode, GenerateCode, OsSystemApi},
        parser::common::Span,
    };

    fn new_compiler() -> CompilerEnv {
        let mut comp = CompilerEnv::new(OsSystemApi::new());
        comp.register_builtin_macros().unwrap();
        comp
    }

    fn new_parser() -> parse::Parser {
        parse::Parser::new(&StringCache::new())
    }

    fn map_piece_data(mut pieces: Vec<Piece>) -> Vec<PieceData> {
        pieces.drain(..).map(|v| v.data).collect()
    }

    #[test]
    fn text_piece_dollar() {
        let str1 = "abc$def ";
        assert_eq!(
            parse::Parser::new(&StringCache::new())
                .next_text_piece(Span::new(str1))
                .map(|v| v.1),
            Ok(PieceData::Text("abc")),
        );
    }

    #[test]
    fn text_piece_end() {
        let str1 = " def ";
        assert_eq!(
            new_parser().next_text_piece(Span::new(str1)).map(|v| v.1),
            Ok(PieceData::Text(str1)),
        );
    }

    #[test]
    fn const_piece() {
        let code = "$def";
        assert_eq!(
            new_parser().next_value_piece(Span::new(code)).map(|v| v.1),
            Ok(PieceData::InstValue(InstValue::Const(CaString::from(
                "def"
            )))),
        );
    }

    #[test]
    fn text_piece_multi() {
        let code = "abc$def ";
        assert_eq!(
            new_parser()
                .parse_content(Span::new(code))
                .finish()
                .map(|v| map_piece_data(v.1)),
            Ok(vec![
                PieceData::Text("abc"),
                InstValue::Const(CaString::from("def")).into(),
                PieceData::Text(" "),
            ]),
        );
    }

    #[test]
    fn basic_const_value() {
        let mut comp = new_compiler();
        comp.add_define(EnvMode::Pass, "JUMP", InstValue::new_name("jump"));
        let code = "eval!{ $JUMP -1 always -1 -1 }";
        assert_eq!(
            comp.compile_and_generate("test", code).unwrap(),
            "jump -1 always -1 -1\n"
        );
    }

    #[test]
    fn merging_const_values() {
        let mut comp = new_compiler();
        comp.add_define(EnvMode::Pass, "JU", InstValue::new_name("ju"));
        comp.add_define(EnvMode::Pass, "MP", InstValue::new_name("mp"));
        let code = "eval!{ $JU$MP -1 always -1 -1 }; eval!{ ju${MP} -1 equal -1 -1 }; ";
        assert_eq!(
            comp.compile_and_generate("test", code).unwrap(),
            "jump -1 always -1 -1\njump -1 equal -1 -1\n"
        );
    }

    #[test]
    fn dolla_dolla_bills() {
        let mut comp = new_compiler();
        let source = Source::new_unique("test", 1, 1);
        let code = "set $$billz 1";
        let tokens = comp.call_macro("eval", code, source).unwrap();
        assert_eq!(format!("{}", GenerateCode::new(&tokens)), "set $billz 1\n");
    }
}
