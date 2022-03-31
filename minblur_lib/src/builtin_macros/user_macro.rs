use std::rc::Rc;

use bn_expression::ParseError;

use crate::{
    compiler::{macros::prelude::*, EnvMode},
    parser::DirectiveDefineMacro,
};

#[derive(Clone, Debug, PartialEq)]
pub struct UserMacro {
    original_source: Source,
    macro_define: DirectiveDefineMacro,
}
impl UserMacro {
    pub fn new(source: Source, define: DirectiveDefineMacro) -> Self {
        Self {
            original_source: source,
            macro_define: define,
        }
    }
}
impl MacroHandler for UserMacro {
    fn generate(
        &mut self,
        ctx: CompilerEnv,
        source: Source,
        _name: &str,
        content: &str,
    ) -> MacroResult {
        let arg_values = parse::parse_args(&ctx.string_cache(), content)
            .map_err(|e| CompileError::macro_error(source.clone(), e))?;
        let arg_names = &self.macro_define.args;
        if arg_values.len() != arg_names.len() {
            return Err(UserMacroError::IncorrectNumberOfArgs {
                got: arg_values.len(),
                expected: arg_names.len(),
            }
            .into());
        }
        // Set so local defines don't mess with global scope and add local arguments
        let mut ctx2 = CompilerEnv::with_parent(ctx);
        ctx2.set_default_mode(EnvMode::Local);
        for i in 0..arg_values.len() {
            ctx2.add_define(EnvMode::Local, &arg_names[i], arg_values[i].clone());
        }
        // Adjust the source of each statement
        let source_parent = Rc::new(self.original_source.with_tree_parent(&Rc::new(source)));
        let mut statements = self.macro_define.statements.clone();
        for item in &mut statements {
            item.source.parent = Some(source_parent.clone());
        }
        // Parse the macro statements given this new context
        let statements = ctx2.expansion_pass(statements)?;
        Ok(statements)
    }
}

#[derive(thiserror::Error, Debug)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize), serde(tag = "kind"))]
pub enum UserMacroError {
    #[error(transparent)]
    Parse {
        #[from]
        parse: ParseError,
    },
    #[error("incorrect number of arguments, got {got} expected {expected}")]
    IncorrectNumberOfArgs { got: usize, expected: usize },
}
impl UserMacroError {}
impl DynamicError for UserMacroError {}

mod parse {
    use bn_expression::ParseError;
    use nom::{
        bytes::complete::tag,
        multi::separated_list0,
        sequence::{preceded, terminated},
        Finish, Parser,
    };

    use super::UserMacroError;
    use crate::{
        common::string_cache::StringCache,
        compiler::instruction::InstValue,
        parser::{
            common::{assert_input_consumed, sp0, Span},
            parse_instruction_arg_string,
        },
    };

    pub fn parse_args(scache: &StringCache, input: &str) -> Result<Vec<InstValue>, UserMacroError> {
        let next_arg = |input2| {
            let (input2, _) = sp0.parse(input2)?;
            let (input2, value) = parse_instruction_arg_string(scache, input2)?;
            Ok((input2, value))
        };

        terminated(
            separated_list0(preceded(sp0, tag(",")), next_arg),
            preceded(sp0, assert_input_consumed(next_arg)),
        )
        .parse(Span::new(input))
        .finish()
        .map(|a| a.1)
        .map_err(|e| ParseError::from(e).into())
    }
}
