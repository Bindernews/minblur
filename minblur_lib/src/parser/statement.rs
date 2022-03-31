use std::fmt;
use std::hash::Hash;

// export
pub use first_pass::tokens_to_statements;

use super::consts::*;
use super::Source;
use crate::common::macros::impl_accessors;
use crate::common::string_cache::CaString;
use crate::compiler::instruction::{InstValue, Instruction};
use crate::compiler::GenerateCode;

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Statement {
    pub source: Source,
    pub data: StatementData,
}
impl Statement {
    pub fn new<D: Into<StatementData>>(source: Source, data: D) -> Self {
        let data = data.into();
        Self { source, data }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum Directive {
    Define(DirectiveDefine),
    Include { path: String },
    LoadExtension { ext_type: String, path: String },
    InlineExtension { ext_type: String, code: String },
    DefineMacro(DirectiveDefineMacro),
    Option(DirectiveOption),
    If(DirectiveIf),
}
crate::enum_from_variants!(
    Directive;
    Define(DirectiveDefine),
    Option(DirectiveOption),
    DefineMacro(DirectiveDefineMacro),
    If(DirectiveIf),
);
impl Directive {
    pub fn generate(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use DIRECTIVE_PREFIX as DOT;
        match self {
            Self::Define(d) => {
                write!(f, "{}{} {} {}", DOT, DIRECTIVE_DEFINE, d.key, d.value)
            }
            Self::Include { path } => {
                write!(f, "{}{} {:?}", DOT, DIRECTIVE_INCLUDE, path)
            }
            Self::LoadExtension { ext_type, path } => {
                write!(f, "{}{} ", DOT, DIRECTIVE_INCLUDE)?;
                write!(f, "\"{:?}\" \"{:?}\"", ext_type, path)
            }
            Self::InlineExtension { ext_type, code } => {
                writeln!(f, "{}{} \"{:?}\"", DOT, DIRECTIVE_BEGIN_EXTENSION, ext_type)?;
                writeln!(f, "{}", code)?;
                write!(f, "{}{}", DOT, DIRECTIVE_END_EXTENSION)
            }
            Self::DefineMacro(d) => {
                write!(f, "{}{} {}(", DOT, DIRECTIVE_MACRO, d.name)?;
                for i in 0..d.args.len() {
                    let pre = if i > 0 { ", " } else { "" };
                    write!(f, "{}{}", pre, d.args[i])?;
                }
                writeln!(f, ")")?;
                write!(f, "{}", GenerateCode::new(&d.statements))?;
                write!(f, "{}{}", DOT, DIRECTIVE_ENDMACRO)
            }
            Self::Option(d) => {
                write!(f, "{}{} {} {}", DOT, DIRECTIVE_OPTION, d.key, d.value)
            }
            Self::If(d) => d.generate(f, false),
        }
    }
}
impl fmt::Display for Directive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.generate(f)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DirectiveDefine {
    pub key: String,
    pub value: InstValue,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DirectiveOption {
    pub key: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DirectiveDefineMacro {
    pub name: String,
    pub args: Vec<String>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct DirectiveIf {
    /// The "if" condition, empty string for "else"
    pub cond: String,
    pub statements: Vec<Statement>,
    pub else_if: Option<Box<DirectiveIf>>,
}
impl DirectiveIf {
    fn generate(&self, f: &mut fmt::Formatter<'_>, is_else: bool) -> fmt::Result {
        let d_type = if is_else {
            if self.cond.is_empty() {
                DIRECTIVE_ELSE
            } else {
                DIRECTIVE_ELSEIF
            }
        } else {
            DIRECTIVE_IF
        };
        writeln!(f, "{}{} {}", DIRECTIVE_PREFIX, d_type, self.cond)?;
        write!(f, "{}", GenerateCode::new(&self.statements))?;
        if let Some(else_if) = &self.else_if {
            else_if.generate(f, true)
        } else {
            write!(f, "{}{}", DIRECTIVE_PREFIX, DIRECTIVE_ENDIF)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label {
    pub name: String,
}
impl Label {
    pub fn new<S: AsRef<str>>(name: S) -> Self {
        Self {
            name: name.as_ref().into(),
        }
    }

    /// Labels have auto-generated suffixes added to them.
    /// This strips the suffix.
    pub fn strip_suffix(name: &str) -> &str {
        name.find('@')
            .map(|idx| name[0..idx].into())
            .unwrap_or_else(|| name)
    }

    pub fn generate(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:", &self.name)
    }
}
impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.generate(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroCall {
    pub name: CaString,
    pub content: String,
}
impl MacroCall {
    pub fn generate(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.content.contains("\r\n") {
            // Write multi-line block version
            write!(f, "{}! {{{}}}", self.name, self.content)
        } else {
            // Write single-line version
            write!(f, "{}! {}", self.name, self.content)
        }
    }
}
impl fmt::Display for MacroCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.generate(f)
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub enum StatementData {
    Label(Label),
    Directive(Directive),
    Instruction(Instruction),
    MacroCall(MacroCall),
    Comment(String),
}
impl StatementData {
    pub fn new_label<S: AsRef<str>>(name: S) -> Self {
        Self::Label(Label::new(name))
    }

    pub fn new_instr<I: Into<Instruction>>(instr: I) -> Self {
        Self::Instruction(instr.into())
    }

    pub fn new_macro_call(name: impl AsRef<str>, content: impl AsRef<str>) -> Self {
        Self::MacroCall(MacroCall {
            name: name.as_ref().into(),
            content: content.as_ref().into(),
        })
    }

    pub fn generate(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Label(v) => v.generate(f),
            Self::Directive(v) => v.generate(f),
            Self::Instruction(v) => v.generate(f),
            Self::MacroCall(v) => v.generate(f),
            Self::Comment(v) => fmt::Display::fmt(v, f),
        }
    }
}
crate::enum_from_variants!(
    StatementData;
    Label(Label),
    Directive(Directive),
    Instruction(Instruction),
    MacroCall(MacroCall),
);
impl_accessors! {
    StatementData; self;
    fn instruction_ref() -> Option[Instruction] {
        match self {
            Self::Instruction(v) => Some(v),
            _ => None,
        }
    }
    mut fn instruction_mut();

    fn directive_ref() -> Option[Directive] {
        match self {
            Self::Directive(v) => Some(v),
            _ => None
        }
    }
    mut fn directive_mut();
}

mod first_pass {
    use super::*;
    use crate::{
        compiler::{CompileError, PassError},
        parser::token::{DirectiveToken, Token, TokenData},
    };

    pub fn tokens_to_statements(
        tokens: Vec<Token>,
        source: &Source,
    ) -> Result<Vec<Statement>, CompileError> {
        FirstPass::new(source).convert(tokens)
    }

    #[derive(Copy, Clone, PartialEq, Debug)]
    enum BlockKind {
        Root,
        Macro,
        If,
        ElseIf,
    }
    impl BlockKind {
        pub fn is_if(&self) -> bool {
            matches!(self, Self::If | Self::ElseIf)
        }
        pub fn as_str(&self) -> &'static str {
            match self {
                Self::Root => "root",
                Self::Macro => "macro",
                Self::If => "if",
                Self::ElseIf => "elseif",
            }
        }
    }

    #[derive(Clone, Debug)]
    struct Block {
        kind: BlockKind,
        /// Original block source
        source: Source,
        /// Additional data for the token
        data: TokenData,
        statements: Vec<Statement>,
    }
    impl Block {
        pub fn new(kind: BlockKind, source: Source, data: TokenData) -> Self {
            Self {
                kind,
                source,
                data,
                statements: Vec::new(),
            }
        }
    }

    pub struct FirstPass {
        block_stack: Vec<Block>,
        source: Source,
        need_eos: bool,
    }

    impl FirstPass {
        pub fn new(source: &Source) -> Self {
            Self {
                block_stack: Vec::new(),
                source: source.clone(),
                need_eos: false,
            }
        }

        /// Converts an array of tokens to statements.
        fn convert(&mut self, mut tokens: Vec<Token>) -> Result<Vec<Statement>, CompileError> {
            self.block_stack.push(Block::new(
                BlockKind::Root,
                self.source.clone(),
                TokenData::Skip,
            ));
            for tok in tokens.drain(..) {
                let source = self.source.clone().with_position(tok.line, tok.column);
                match tok.data {
                    TokenData::DirectiveToken(directive) => {
                        self.expect_eos(&source)?;
                        self.handle_directive(directive, source)?;
                    }
                    TokenData::Instruction(instr) => {
                        self.expect_eos(&source)?;
                        self.append_statement(Statement::new(source, instr));
                    }
                    TokenData::Label(label) => {
                        self.expect_eos(&source)?;
                        self.append_statement(Statement::new(source, StatementData::Label(label)));
                    }
                    TokenData::MacroCall { name, content } => {
                        self.expect_eos(&source)?;
                        self.append_statement(Statement::new(source, MacroCall { name, content }));
                    }
                    TokenData::EndOfStatement => {
                        // We found an end-of-statement so we're good
                        self.need_eos = false;
                    }
                    TokenData::OutputComment(comment) => {
                        self.append_statement(Statement::new(
                            source,
                            StatementData::Comment(comment),
                        ));
                    }
                    TokenData::Skip => {}
                }
            }
            let root_block = self.pop_block(&self.source.clone())?;
            if root_block.kind != BlockKind::Root {
                let (got, exp) = (&root_block.kind, BlockKind::Root.as_str());
                Err(Self::unexpected_block_type(got, exp, &self.source))
            } else {
                Ok(root_block.statements)
            }
        }

        /// Helper to assert that we are ready for a new statement and change state accordingly
        fn expect_eos(&mut self, source: &Source) -> Result<(), CompileError> {
            if self.need_eos {
                Err(PassError::ExpectedEndOfStatement().with_source(source))
            } else {
                self.need_eos = true;
                Ok(())
            }
        }

        /// Helper to create a `BlockStackUnexpectedType` error
        fn unexpected_block_type(got: &BlockKind, exp: &str, source: &Source) -> CompileError {
            PassError::BlockStackUnexpectedType(got.as_str().into(), exp.into()).with_source(source)
        }

        fn handle_directive(
            &mut self,
            directive: DirectiveToken,
            source: Source,
        ) -> Result<(), CompileError> {
            // Helper for when we expect an if-block
            let expect_if_block = |block: &Block| -> Result<(), CompileError> {
                if !block.kind.is_if() {
                    Err(Self::unexpected_block_type(
                        &block.kind,
                        "if or elseif",
                        &source,
                    ))
                } else {
                    Ok(())
                }
            };

            match directive {
                DirectiveToken::BeginMacro { .. } => {
                    self.block_stack.push(Block::new(
                        BlockKind::Macro,
                        source,
                        TokenData::DirectiveToken(directive),
                    ));
                    Ok(())
                }
                DirectiveToken::EndMacro => {
                    let block = self.pop_block(&source)?;
                    if block.kind != BlockKind::Macro {
                        let exp = BlockKind::Macro.as_str();
                        return Err(Self::unexpected_block_type(&block.kind, exp, &source));
                    }
                    match block.data {
                        TokenData::DirectiveToken(DirectiveToken::BeginMacro {
                            name: macro_name,
                            args,
                        }) => {
                            self.append_statement(Statement {
                                source,
                                data: StatementData::Directive(
                                    DirectiveDefineMacro {
                                        name: macro_name,
                                        args,
                                        statements: block.statements,
                                    }
                                    .into(),
                                ),
                            });
                            Ok(())
                        }
                        _ => panic!(),
                    }
                }
                DirectiveToken::Define { key, value } => {
                    self.append_statement(Statement {
                        source,
                        data: StatementData::Directive(DirectiveDefine { key, value }.into()),
                    });
                    Ok(())
                }
                DirectiveToken::Option { key, value } => {
                    self.append_statement(Statement {
                        source,
                        data: StatementData::Directive(DirectiveOption { key, value }.into()),
                    });
                    Ok(())
                }
                DirectiveToken::Include { path } => {
                    self.append_statement(Statement {
                        source,
                        data: Directive::Include { path }.into(),
                    });
                    Ok(())
                }
                DirectiveToken::LoadExtension { ext_type, path } => {
                    self.append_statement(Statement {
                        source,
                        data: Directive::LoadExtension { ext_type, path }.into(),
                    });
                    Ok(())
                }
                DirectiveToken::InlineExtension { .. } => {
                    // Parser doesn't support this yet
                    todo!()
                }
                DirectiveToken::If { .. } => {
                    self.block_stack.push(Block::new(
                        BlockKind::If,
                        source,
                        TokenData::DirectiveToken(directive),
                    ));
                    Ok(())
                }
                DirectiveToken::ElseIf { .. } => {
                    let block = self.pop_block(&source)?;
                    // Expecting an If/ElseIf block
                    expect_if_block(&block)?;
                    // Re-push old block because "else" if
                    self.block_stack.push(block);
                    // Push new "else-if" block
                    self.block_stack.push(Block::new(
                        BlockKind::ElseIf,
                        source,
                        TokenData::DirectiveToken(directive),
                    ));
                    Ok(())
                }
                DirectiveToken::Else => {
                    let block = self.pop_block(&source)?;
                    // Expecting an If/ElseIf block
                    expect_if_block(&block)?;
                    // Re-push old block because "else" if
                    self.block_stack.push(block);
                    // Push new "else-if" block
                    self.block_stack.push(Block::new(
                        BlockKind::ElseIf,
                        source,
                        TokenData::DirectiveToken(directive),
                    ));
                    Ok(())
                }
                DirectiveToken::EndIf => {
                    // Unroll if/else-if chain into DirectiveIf statements
                    let mut block = self.pop_block(&source)?;
                    expect_if_block(&block)?;
                    let mut real_source = source.clone();
                    let mut direct: Option<DirectiveIf> = None;
                    // Put the block back on the stack so it's processed the same as everything else
                    self.block_stack.push(block);
                    loop {
                        block = self.pop_block(&source)?;
                        if block.kind.is_if() {
                            // Create the new if, assign its "else-if" to the current one,
                            // then replace the current if-directive.
                            let mut next_direct = self.make_directive_if(&mut block)?;
                            next_direct.else_if = direct.take().map(Box::new);
                            direct = Some(next_direct);
                            // Update the real source
                            real_source = block.source;
                            // If we're at the "top" of this if/else-if chain, end the loop
                            if block.kind == BlockKind::If {
                                break;
                            }
                        } else {
                            self.block_stack.push(block);
                            break;
                        }
                    }
                    // Add the new if-chain as a single statement
                    self.append_statement(Statement {
                        source: real_source,
                        data: Directive::from(direct.unwrap()).into(),
                    });
                    Ok(())
                }
            }
        }

        fn make_directive_if(&self, block: &mut Block) -> Result<DirectiveIf, CompileError> {
            let cond = match block.data {
                TokenData::DirectiveToken(
                    DirectiveToken::If { ref cond } | DirectiveToken::ElseIf { ref cond },
                ) => cond.clone(),
                TokenData::DirectiveToken(DirectiveToken::Else) => "".into(),
                _ => {
                    eprintln!("E382: {:?}", block.data);
                    todo!()
                }
            };
            let mut direct = DirectiveIf {
                cond,
                statements: Vec::new(),
                else_if: None,
            };
            std::mem::swap(&mut direct.statements, &mut block.statements);
            Ok(direct)
        }

        fn pop_block(&mut self, source: &Source) -> Result<Block, CompileError> {
            self.block_stack
                .pop()
                .ok_or_else(|| PassError::BlockStackEmpty.with_source(source))
        }

        fn append_statement(&mut self, stmt: Statement) {
            let block = self.block_stack.last_mut().unwrap();
            block.statements.push(stmt);
        }
    }
}
