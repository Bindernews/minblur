use std::{
    collections::hash_map::{DefaultHasher, HashMap},
    convert::{TryFrom, TryInto},
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

use bn_expression::{
    basic::BasicOp,
    enum_to_from_str,
    expression::{EvalContext, FunctionType},
    parse::Position,
    util::ArgumentHelper,
    EvalError, ExpressionOp,
};

use super::MathError;
use crate::{
    common::string_cache::StringCache,
    compiler::{consts::*, instruction::*, macros::prelude::*},
    enum_from_variants, try_enum_convert,
};

pub type Expression = bn_expression::Expression<MathOp>;

/// An eval context useful for evaluating math expressions where names can be unadorned
///
/// In this evaluator prefixing a name with a $ will try to evaluate it as a constant
/// and if that constant eval fails, the eval will fail.
pub struct MathEval<'a> {
    env: &'a mut CompilerEnv,
    source: &'a Source,
}
impl<'a> MathEval<'a> {
    pub fn new(env: &'a mut CompilerEnv, source: &'a Source) -> Self {
        Self { env, source }
    }
}
impl<'a> EvalContext<MathOp> for MathEval<'a> {
    fn get_variable(&mut self, name: &str) -> Result<AValue, EvalError> {
        if !name.starts_with(CONST_PREFIX) {
            Ok(AValue::name(name))
        } else {
            let value = self
                .env
                .get_define(EnvMode::Global, &name[1..])
                .map(|v| v.clone())
                .ok_or_else(|| EvalError::UnknownVariable { name: name.into() })?;
            let value = self
                .env
                .simplify_inst_value(value, self.source, false)
                .map_err(|e| EvalError::Other(Box::new(e)))?;
            value.into_value().ok_or(EvalError::UnresolvedValue)
        }
    }

    fn call(&mut self, name: &str, args: Vec<AValue>) -> Result<Expression, EvalError> {
        MathFunction::from_name(name)
            .ok_or_else(|| EvalError::unknown_function(name))
            .and_then(|f| f.eval_call(name, args))
    }

    fn get_function_type(&self, name: &str) -> FunctionType {
        MathFunction::from_name(name).map_or(FunctionType::Unknown, |f| f.get_function_type())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathFunction {
    Read,
    Write,
    Draw,
    Print,
    DrawFlush,
    PrintFlush,
    GetLink,
    Control,
    Radar,
    Sensor,
    Jump,

    Max,
    Min,
    Angle,
    Len,
    Noise,
    Abs,
    Log,
    Log10,
    Floor,
    Ceil,
    Sqrt,
    Rand,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
}
impl MathFunction {
    pub fn eval_call(&self, name: &str, args: Vec<AValue>) -> Result<Expression, EvalError> {
        let args = ArgumentHelper::new(name, args);
        let delay_err = || EvalError::delay_function_called(self.name());
        match self {
            // Yes I could use _ but I want to ensure we match all variants
            Self::Read
            | Self::Write
            | Self::Draw
            | Self::Print
            | Self::DrawFlush
            | Self::PrintFlush
            | Self::GetLink
            | Self::Control
            | Self::Radar
            | Self::Sensor
            | Self::Jump => Err(delay_err()),
            Self::Max => Self::math2(args, |a, b| a.max(b)),
            Self::Min => Self::math2(args, |a, b| a.min(b)),
            // Self::Angle => Self::math2(args, |x, y| y.atan2(x)),
            Self::Angle => Err(delay_err()),
            Self::Len => Self::math2(args, |x, y| (x * x + y * y).sqrt()),
            Self::Noise => Err(delay_err()),
            Self::Abs => Self::math1(args, |v| v.abs()),
            Self::Log => Self::math2(args, |a, b| a.log(b)),
            Self::Log10 => Self::math1(args, |v| v.log10()),
            Self::Floor => Self::math1(args, |v| v.floor()),
            Self::Ceil => Self::math1(args, |v| v.ceil()),
            Self::Sqrt => Self::math1(args, |v| v.sqrt()),
            Self::Rand => Err(delay_err()),
            Self::Sin => Self::math1(args, |v| v.sin()),
            Self::Cos => Self::math1(args, |v| v.cos()),
            Self::Tan => Self::math1(args, |v| v.tan()),
            Self::Asin => Self::math1(args, |v| v.asin()),
            Self::Acos => Self::math1(args, |v| v.acos()),
            Self::Atan => Self::math1(args, |v| v.atan()),
        }
    }

    pub fn get_function_type(&self) -> FunctionType {
        match self {
            Self::Read
            | Self::Write
            | Self::Draw
            | Self::Print
            | Self::DrawFlush
            | Self::PrintFlush
            | Self::GetLink
            | Self::Control
            | Self::Radar
            | Self::Sensor
            | Self::Jump => FunctionType::Delay,
            Self::Angle | Self::Noise | Self::Rand => FunctionType::Delay,
            _ => FunctionType::Eval,
        }
    }

    /// Returns true if this function has a return value
    pub fn has_return(&self) -> bool {
        if OpSymbol::try_from(*self).is_ok() {
            true
        } else {
            match self {
                Self::Read => true,
                Self::Write => false,
                Self::Draw => false,
                Self::Print => false,
                Self::DrawFlush => false,
                Self::PrintFlush => false,
                Self::GetLink => true,
                Self::Control => false,
                Self::Radar => true,
                Self::Sensor => true,
                Self::Jump => false,
                _ => false,
            }
        }
    }

    fn math1<F>(args: ArgumentHelper, f: F) -> Result<Expression, EvalError>
    where
        F: FnOnce(f64) -> f64,
    {
        Ok(args
            .args_as_f64(0..1)
            .map(|ar| AValue::from(f(ar[0])).into())
            .unwrap_or_else(|_| args.return_input()))
    }

    fn math2<F>(args: ArgumentHelper, f: F) -> Result<Expression, EvalError>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        Ok(args
            .args_as_f64(0..2)
            .map(|ar| AValue::from(f(ar[0], ar[1])).into())
            .unwrap_or_else(|_| args.return_input()))
    }

    pub fn name(&self) -> &'static str {
        ConstNames::try_from(*self).unwrap().name()
    }
    pub fn from_name(input: &str) -> Option<Self> {
        ConstNames::from_name(input)?.try_into().ok()
    }
}
impl TryFrom<ConstNames> for MathFunction {
    type Error = ();
    fn try_from(value: ConstNames) -> Result<Self, Self::Error> {
        try_enum_convert!(ConstNames, MathFunction, value, ();
            {   Read, Write, Draw, Print, DrawFlush, PrintFlush, GetLink, Control, Radar,
                Sensor, Jump, Max, Min, Angle, Len, Noise, Abs, Log, Log10, Floor, Ceil, Sqrt,
                Rand, Sin, Cos, Tan, Asin, Acos, Atan, }
            {}
        )
    }
}
impl TryFrom<MathFunction> for ConstNames {
    type Error = ();
    fn try_from(value: MathFunction) -> Result<Self, Self::Error> {
        try_enum_convert!(MathFunction, ConstNames, value, ();
            {   Read, Write, Draw, Print, DrawFlush, PrintFlush, GetLink, Control, Radar,
                Sensor, Jump, Max, Min, Angle, Len, Noise, Abs, Log, Log10, Floor, Ceil, Sqrt,
                Rand, Sin, Cos, Tan, Asin, Acos, Atan, }
            {}
        )
    }
}
impl TryFrom<MathFunction> for OpSymbol {
    type Error = ();
    fn try_from(value: MathFunction) -> Result<Self, Self::Error> {
        try_enum_convert!(MathFunction, OpSymbol, value, ();
            {   Max, Min, Angle, Len, Noise, Abs, Log, Log10, Floor, Ceil, Sqrt,
                Rand, Sin, Cos, Tan, Asin, Acos, Atan, }
            {}
        )
    }
}
impl TryFrom<MathFunction> for InstructionKind {
    type Error = ();
    fn try_from(value: MathFunction) -> Result<Self, Self::Error> {
        try_enum_convert!(MathFunction, InstructionKind, value, ();
            {   Read, Write, Draw, Print, DrawFlush, PrintFlush, GetLink, Control, Radar,
                Sensor, Jump, }
            {}
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    LAnd,
    BAnd,
    BOr,
    BXor,
    Not,
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Shl,
    Shr,
}
impl MathOp {
    pub fn as_expr_str(&self) -> &'static str {
        BasicOp::from(*self).as_expr_str()
    }
    pub fn from_expr_str(input: &str) -> Option<Self> {
        Some(BasicOp::from_expr_str(input)?.into())
    }
}
impl fmt::Display for MathOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_expr_str())
    }
}
impl ExpressionOp for MathOp {
    fn eval(&self, left: &AValue, right: &AValue) -> Result<AValue, EvalError> {
        BasicOp::from(*self).eval(left, right)
    }

    fn precedence(&self) -> usize {
        BasicOp::from(*self).precedence()
    }

    fn is_unary(&self) -> bool {
        BasicOp::from(*self).is_unary()
    }

    fn match_op(input: &str) -> Result<(&str, Self), &'static str> {
        use nom::InputTake;
        for op_length in [2, 1] {
            if input.len() >= op_length {
                let (remain, used) = input.take_split(op_length);
                if let Some(op) = Self::from_expr_str(used) {
                    return Ok((remain, op));
                }
            }
        }
        Err("failed to parse op")
    }
}
impl From<MathOp> for BasicOp {
    fn from(value: MathOp) -> BasicOp {
        try_enum_convert!(MathOp, BasicOp, value, ();
            { Add, Sub, Mul, Div, IDiv, Mod, LAnd, BAnd, BOr, BXor, Not, Equal, NotEqual,
                LessThan, LessThanEq, GreaterThan, GreaterThanEq, Shl, Shr, }
            {}
        )
        .unwrap()
    }
}
impl From<BasicOp> for MathOp {
    fn from(value: BasicOp) -> MathOp {
        try_enum_convert!(BasicOp, MathOp, value, ();
            { Add, Sub, Mul, Div, IDiv, Mod, LAnd, BAnd, BOr, BXor, Not, Equal, NotEqual,
                LessThan, LessThanEq, GreaterThan, GreaterThanEq, Shl, Shr, }
            {}
        )
        .unwrap()
    }
}

impl TryFrom<MathOp> for OpSymbol {
    type Error = ();
    fn try_from(value: MathOp) -> Result<Self, Self::Error> {
        try_enum_convert!(MathOp, OpSymbol, value, ();
            { Add, Sub, Mul, Div, IDiv, Mod, LAnd, BAnd, BOr, BXor, Equal, NotEqual,
                LessThan, LessThanEq, GreaterThan, GreaterThanEq, Shl, Shr, }
            { Not => Flip, }
        )
    }
}
impl TryFrom<MathOp> for JumpSymbol {
    type Error = ();
    fn try_from(value: MathOp) -> Result<Self, Self::Error> {
        try_enum_convert!(MathOp, JumpSymbol, value, ();
            { Equal, NotEqual, LessThan, LessThanEq, GreaterThan, GreaterThanEq, }
            {}
        )
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum AssignmentOp {
    Set,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
}
impl AssignmentOp {
    enum_to_from_str!(
        AssignmentOp; pub fn as_str(); pub fn from_str();
        {
            AssignmentOp::Set => "=",
            AssignmentOp::Add => "+=",
            AssignmentOp::Sub => "-=",
            AssignmentOp::Mul => "*-",
            AssignmentOp::Div => "/-",
            AssignmentOp::IDiv => "//=",
        }
    );
}
impl TryFrom<AssignmentOp> for MathOp {
    type Error = ();
    fn try_from(v: AssignmentOp) -> Result<MathOp, Self::Error> {
        match v {
            AssignmentOp::Set => Err(()),
            AssignmentOp::Add => Ok(MathOp::Add),
            AssignmentOp::Sub => Ok(MathOp::Sub),
            AssignmentOp::Mul => Ok(MathOp::Mul),
            AssignmentOp::Div => Ok(MathOp::Div),
            AssignmentOp::IDiv => Ok(MathOp::IDiv),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MathStatement {
    Assign(AssignStatement),
    Jump(JumpStatement),
}
enum_from_variants!(MathStatement; Assign(AssignStatement), Jump(JumpStatement),);

impl MathStatement {
    pub fn generate(
        &self,
        ctx: &mut CompilerEnv,
        source_name: &Rc<String>,
    ) -> Result<Vec<Statement>, MathError> {
        match self {
            Self::Assign(st) => st.generate(ctx, source_name),
            Self::Jump(st) => st.generate(ctx, source_name),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AssignStatement {
    pub pos: Position,
    pub assignee: String,
    pub op: AssignmentOp,
    pub expression: Expression,
}
impl AssignStatement {
    pub fn generate(
        &self,
        ctx: &mut CompilerEnv,
        source_name: &Rc<String>,
    ) -> Result<Vec<Statement>, MathError> {
        // Generate new expression based on assignment op
        let new_expr = match self.op {
            AssignmentOp::Set => self.expression.clone(),
            _ => Expression::Binary(
                self.op.try_into().unwrap(),
                Box::new(AValue::name(&self.assignee).into()),
                Box::new(self.expression.clone()),
            ),
        };
        let source = Source::new(source_name.clone(), self.pos.line, self.pos.column);
        // Constant folding
        let new_expr = new_expr.partial_eval(&mut MathEval::new(ctx, &source))?;
        // Generate final output
        let dest = AValue::name(&self.assignee);
        let gen = ExpressionTokenGen::new(&new_expr, dest, source, &ctx.string_cache());
        gen.generate(ctx)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct JumpStatement {
    pub pos: Position,
    /// Jump condition
    pub cond: Expression,
    /// Destination
    pub dest: AValue,
}
impl JumpStatement {
    pub fn generate(
        &self,
        ctx: &mut CompilerEnv,
        source_name: &Rc<String>,
    ) -> Result<Vec<Statement>, MathError> {
        // Need to know our source
        let source = Source::new(source_name.clone(), self.pos.line, self.pos.column);
        // Constant folding
        let new_expr = self.cond.partial_eval(&mut MathEval::new(ctx, &source))?;
        // Map destination into either quick-jump expression or straight number
        let dest_value = if self.dest.is_name() {
            let sc = ctx.string_cache();
            InstValue::QuickConstExpr(sc.get(FUNC_LABEL_DEST), sc.get(&format!("{}", &self.dest)))
        } else {
            InstValue::Value(self.dest.clone())
        };

        // If we're a constant value, try to convert to bool and jump based on that
        if let Some(v) = new_expr.as_value() {
            let mut toks = Vec::new();
            if v.is_string() {
                // TODO return an error
                todo!()
            }
            let args = if v.is_truthy() {
                [
                    dest_value,
                    JumpSymbol::Always.into_value(&ctx.string_cache()),
                    InstValue::new_num(-1),
                    InstValue::new_num(-1),
                ]
            } else {
                [
                    dest_value,
                    JumpSymbol::StrictEqual.into_value(&ctx.string_cache()),
                    InstValue::new_num(0),
                    InstValue::new_num(1),
                ]
            };
            toks.push(Statement::new(
                source,
                StatementData::new_instr(InstructionJump::from(args)),
            ));
            Ok(toks)
        } else {
            // Not const

            // Assert that the top-most expression can be turned into a jump instruction
            if !new_expr
                .get_op()
                .map(|op| JumpSymbol::try_from(op).is_ok())
                .unwrap_or(false)
            {
                return Err(MathError::InvalidJumpOp(new_expr.get_op().into()));
            }
            // Generate new tokens
            let gen = ExpressionTokenGen::new(
                &new_expr,
                AValue::name("OUT"),
                source.clone(),
                &ctx.string_cache(),
            );
            let mut tokens = gen.generate(ctx)?;
            // Replace last op with jump.
            let last_op = tokens
                .pop()
                .and_then(|t| match t.data {
                    StatementData::Instruction(Instruction::Op(i_op)) => Some(i_op),
                    _ => None,
                })
                .expect("Expected InstructionOp but got something else");
            tokens.push(Statement::new(
                source,
                StatementData::new_instr(InstructionJump {
                    left: last_op.left,
                    op: last_op.op,
                    right: last_op.right,
                    dest: dest_value,
                }),
            ));
            Ok(tokens)
        }
    }
}

struct ExpressionTokenGen<'a> {
    expr: &'a Expression,
    dest: AValue,
    next_temp_var: usize,
    temp_vars: HashMap<u64, AValue>,
    new_tokens: Vec<Statement>,
    source: Source,
    string_cache: StringCache,
}
impl<'a> ExpressionTokenGen<'a> {
    fn new(
        expr: &'a Expression,
        dest: AValue,
        source: Source,
        string_cache: &'_ StringCache,
    ) -> Self {
        Self {
            expr,
            dest,
            next_temp_var: 0,
            temp_vars: HashMap::new(),
            new_tokens: Vec::new(),
            source,
            string_cache: string_cache.clone(),
        }
    }

    /// Consume self and generate new tokens, or an error.
    pub fn generate(mut self, _ctx: &mut CompilerEnv) -> Result<Vec<Statement>, MathError> {
        // Handle case where we have one singular value/name
        if self.expr.as_value().is_some() {
            let token_data = StatementData::new_instr(InstructionSet {
                result: InstValue::Value(self.dest),
                value: InstValue::Value(self.expr.clone().into_value().unwrap()),
            });
            self.new_tokens
                .push(Statement::new(self.source.clone(), token_data));
        } else {
            // Generate new expressions
            self.visit_expressions(self.expr)?;
            // Replace final expression output with correct output variable
            let tok = (self.new_tokens.last_mut()).expect("last token wasn't mutable");
            if let Some(instr) = tok.data.instruction_mut() {
                // If the output is an empty string then we expect no result variable,
                // otherwise we MUST be able to set the result.
                // then replace the final output with output to the real variable
                let kind = instr.kind();
                if !self.dest.as_string().unwrap().is_empty() {
                    let result = instr
                        .result_mut()
                        .ok_or_else(|| MathError::NoReturnFunction { name: kind.name() })?;
                    *result = self.dest.into();
                } else if instr.result().is_some() {
                    // Double-check to ensure we have no output
                    return Err(MathError::ReturnUnused {
                        name: instr.kind().name(),
                    });
                }
            } else {
                panic!(
                    "expected last token to be an Instruction, was {:?}",
                    tok.data
                );
            }
        }
        Ok(self.new_tokens)
    }

    /// Add an instruction to our list of new tokens.
    fn add_op_instr(
        &mut self,
        expr: &Expression,
        op: OpSymbol,
        left: &Expression,
        right: &Expression,
    ) -> Result<(), MathError> {
        let lval = self.expr_to_value(left)?;
        let rval = self.expr_to_value(right)?;
        // Determine our output name
        let out_name = self.make_temp_var(expr);
        // Create the new token data and source
        let token_data = StatementData::new_instr(InstructionOp {
            op: op.into_value(&self.string_cache),
            result: InstValue::from(out_name),
            left: InstValue::from(lval),
            right: InstValue::from(rval),
        });
        // Append token
        self.new_tokens
            .push(Statement::new(self.source.clone(), token_data));
        Ok(())
    }

    fn add_any_instr(
        &mut self,
        kind: InstructionKind,
        expr: &Expression,
        args: &[Expression],
        result_index: Option<usize>,
    ) -> Result<(), MathError> {
        // Convert each
        let args2: Result<Vec<AValue>, MathError> =
            args.iter().map(|v| self.expr_to_value(v)).collect();
        let mut args2 = args2?;

        // Add and track output variable
        if let Some(out_index) = result_index {
            args2.insert(out_index, self.make_temp_var(expr));
        }
        // Convert arguments to InstValues
        let args2: Vec<_> = args2.drain(..).map(InstValue::from).collect();
        // Try to construct the instruction
        let instr = kind
            .create_with_args(args2)
            .map_err(|_| MathError::InstructionCreate {
                kind: kind.name().to_string(),
            })?;
        self.new_tokens
            .push(Statement::new(self.source.clone(), instr));
        Ok(())
    }

    /// Create and track a new temporary output variable to use as the result of `expr`.
    fn make_temp_var(&mut self, expr: &Expression) -> AValue {
        let out_name = AValue::name(format!("__t{}", self.next_temp_var));
        let expr_hash = Self::calculate_hash(expr);
        self.temp_vars.insert(expr_hash, out_name.clone());
        self.next_temp_var += 1;
        out_name
    }

    /// Get either the value or temporary result variable of the expression
    fn expr_to_value(&self, expr: &Expression) -> Result<AValue, MathError> {
        let hash = Self::calculate_hash(expr);
        Ok(expr
            .as_value()
            .unwrap_or_else(|| self.temp_vars.get(&hash).unwrap())
            .clone())
    }

    fn visit_expressions(&mut self, root: &Expression) -> Result<(), MathError> {
        // This is so we don't have to keep creating new expressions
        let zero_expr = Expression::from(0f64);
        // Visit each sub-expression and generate op calls for each
        root.visit(
            false,
            0,
            |expr: &Expression, _: i64| -> Result<i64, MathError> {
                match expr {
                    Expression::Value(_) => {}
                    Expression::Binary(op, left, right) => {
                        self.add_op_instr(expr, OpSymbol::try_from(*op).unwrap(), left, right)?;
                    }
                    Expression::Unary(op, left) => {
                        self.add_op_instr(
                            expr,
                            OpSymbol::try_from(*op).unwrap(),
                            left,
                            &zero_expr,
                        )?;
                    }
                    Expression::Call(name, args) => self.visit_call(expr, name, args)?,
                }
                Ok(0)
            },
        )?;
        Ok(())
    }

    fn visit_call(
        &mut self,
        expr: &Expression,
        name: &str,
        args: &[Expression],
    ) -> Result<(), MathError> {
        // Helper
        let get_arg = |index| Self::get_arg(name, args, index);
        macro_rules! add_it {
            (op1, $func:ident) => {{
                let op = OpSymbol::try_from($func).unwrap();
                self.add_op_instr(expr, op, get_arg(0)?, &AValue::from(0).into())
            }};
            (op2, $func:ident) => {{
                let op = OpSymbol::try_from($func).unwrap();
                self.add_op_instr(expr, op, get_arg(0)?, get_arg(1)?)
            }};
            (any, $func:ident, $result_index:expr) => {{
                let name = InstructionKind::try_from($func).unwrap();
                self.add_any_instr(name, expr, args, $result_index)
            }};
        }
        let func =
            MathFunction::from_name(name).ok_or_else(|| EvalError::unknown_function(name))?;
        type MF = MathFunction;
        match func {
            MF::Read => add_it!(any, func, Some(0)),
            MF::Write => add_it!(any, func, None),
            MF::Draw => add_it!(any, func, None),
            MF::Print => add_it!(any, func, None),
            MF::DrawFlush => add_it!(any, func, None),
            MF::PrintFlush => add_it!(any, func, None),
            MF::GetLink => add_it!(any, func, Some(0)),
            MF::Control => add_it!(any, func, None),
            MF::Radar => add_it!(any, func, Some(6)),
            MF::Sensor => add_it!(any, func, Some(0)),
            MF::Jump => self.generate_jump(name, args),
            MF::Max => add_it!(op2, func),
            MF::Min => add_it!(op2, func),
            MF::Angle => add_it!(op2, func),
            MF::Len => add_it!(op2, func),
            MF::Noise => add_it!(op2, func),
            MF::Abs => add_it!(op1, func),
            MF::Log => add_it!(op2, func),
            MF::Log10 => add_it!(op1, func),
            MF::Floor => add_it!(op1, func),
            MF::Ceil => add_it!(op1, func),
            MF::Sqrt => add_it!(op1, func),
            MF::Rand => add_it!(op1, func),
            MF::Sin => add_it!(op1, func),
            MF::Cos => add_it!(op1, func),
            MF::Tan => add_it!(op1, func),
            MF::Asin => add_it!(op1, func),
            MF::Acos => add_it!(op1, func),
            MF::Atan => add_it!(op1, func),
        }
    }

    #[allow(clippy::try_err)]
    fn generate_jump(&mut self, name: &str, args: &[Expression]) -> Result<(), MathError> {
        let true_cond = AValue::from(1).into();
        let (dest, cond) = match args.len() {
            1 => (Self::get_arg(name, args, 0)?, &true_cond),
            2 => (Self::get_arg(name, args, 0)?, Self::get_arg(name, args, 1)?),
            _ => Err(EvalError::call_error(name, "incorrect number of arguments"))?,
        };
        // Get destination value
        let dest_value = dest
            .as_value()
            .map(|v| {
                if let Some(name) = v.as_string() {
                    let sc = &self.string_cache;
                    InstValue::QuickConstExpr(sc.get(FUNC_LABEL_DEST), sc.get(name))
                } else {
                    InstValue::new_num(v.as_f64().unwrap_or_default())
                }
            })
            .ok_or_else(|| EvalError::invalid_arg(name, 0))?;

        if let Some(v) = cond.as_value() {
            // If we're a constant value, try to convert to bool and jump based on that
            let (symbol, left) = if v.is_truthy() {
                (JumpSymbol::Always, InstValue::new_num(1))
            } else {
                (JumpSymbol::StrictEqual, InstValue::new_num(0))
            };
            let instr: Instruction = InstructionJump {
                dest: dest_value,
                op: symbol.into_value(&self.string_cache),
                left,
                right: InstValue::new_num(1),
            }
            .into();
            self.new_tokens
                .push(Statement::new(self.source.clone(), instr));
            Ok(())
        } else {
            // Not const

            // Assert that the top-most expression can be turned into a jump instruction
            if !cond
                .get_op()
                .map(|op| JumpSymbol::try_from(op).is_ok())
                .unwrap_or(false)
            {
                return Err(MathError::InvalidJumpOp(cond.get_op().into()));
            }
            // We're ASSUMING that the last-most instruction was an OP
            let last_op = self
                .new_tokens
                .pop()
                .and_then(|t| match t.data {
                    StatementData::Instruction(Instruction::Op(i_op)) => Some(i_op),
                    _ => None,
                })
                .expect("Expected InstructionOp but got something else");
            // Replace last op with jump.
            let instr: Instruction = InstructionJump {
                dest: dest_value,
                op: last_op.op,
                left: last_op.left,
                right: last_op.right,
            }
            .into();
            self.new_tokens
                .push(Statement::new(self.source.clone(), instr));
            Ok(())
        }
    }

    fn get_arg<'b>(
        name: &str,
        args: &'b [Expression],
        index: usize,
    ) -> Result<&'b Expression, EvalError> {
        args.get(index)
            .ok_or_else(|| EvalError::invalid_arg(name, index))
    }

    fn calculate_hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
