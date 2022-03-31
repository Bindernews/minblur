use std::{cell::RefCell, fmt::Debug, rc::Rc};

use super::error::DynamicError;
use super::{CompilerEnv, PassError};
use crate::parser::Source;
use crate::parser::Statement;

/// Common imports for Rust macro implementations
pub mod prelude {
    pub use bn_expression::AValue;

    pub use super::{MacroHandler, MacroResult};
    pub use crate::compiler::{error::*, CompilerEnv, EnvMode};
    pub use crate::parser::{Directive, Source, Statement, StatementData};
}

pub type MacroResult = Result<Vec<Statement>, Box<dyn DynamicError>>;

pub trait MacroHandler: Debug {
    fn generate(
        &mut self,
        ctx: CompilerEnv,
        source: Source,
        name: &str,
        content: &str,
    ) -> MacroResult;
}

/// Wraps a macro so that it's safe to call it even while mutating whatever
/// contains the macro.
#[derive(Debug)]
pub(crate) struct MacroWrap {
    /// Wrapped handler
    wrapped: RefCell<Box<dyn MacroHandler>>,
}
impl MacroWrap {
    pub fn new(wrapped: Box<dyn MacroHandler>) -> Rc<Self> {
        Rc::new(Self {
            wrapped: RefCell::new(wrapped),
        })
    }

    /// Invoke the macro with the given context and return the generated tokens.
    ///
    /// Note that if `generate()` is called recursively `CompileError::RecrusiveMacro` will be returned.
    ///
    pub fn generate(
        &self,
        ctx: CompilerEnv,
        source: Source,
        name: &str,
        content: &str,
    ) -> MacroResult {
        let mut wrapped_mut = self
            .wrapped
            .try_borrow_mut()
            .map_err(|_| PassError::RecrusiveMacro { name: name.into() }.with_source(&source))?;
        wrapped_mut.generate(ctx, source, name, content)
    }
}
impl PartialEq for MacroWrap {
    fn eq(&self, other: &Self) -> bool {
        self.wrapped.as_ptr() == other.wrapped.as_ptr()
    }
}
impl Eq for MacroWrap {}
