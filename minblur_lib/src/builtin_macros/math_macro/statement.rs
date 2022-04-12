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
    enum_from_variants,
    parser::Label,
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
pub enum OpFunction {
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
impl OpFunction {
    pub fn eval_call(&self, name: &str, args: Vec<AValue>) -> Result<Expression, EvalError> {
        let args = ArgumentHelper::new(name, args);
        match self {
            Self::Max => Self::math2(args, |a, b| a.max(b)),
            Self::Min => Self::math2(args, |a, b| a.min(b)),
            // Self::Angle => Self::math2(args, |x, y| y.atan2(x)),
            Self::Angle => Self::check_arg_count(args, 2),
            Self::Len => Self::math2(args, |x, y| (x * x + y * y).sqrt()),
            Self::Noise => Self::check_arg_count(args, 2),
            Self::Abs => Self::math1(args, |v| v.abs()),
            Self::Log => Self::math2(args, |a, b| a.log(b)),
            Self::Log10 => Self::math1(args, |v| v.log10()),
            Self::Floor => Self::math1(args, |v| v.floor()),
            Self::Ceil => Self::math1(args, |v| v.ceil()),
            Self::Sqrt => Self::math1(args, |v| v.sqrt()),
            Self::Rand => Self::check_arg_count(args, 1),
            Self::Sin => Self::math1(args, |v| v.sin()),
            Self::Cos => Self::math1(args, |v| v.cos()),
            Self::Tan => Self::math1(args, |v| v.tan()),
            Self::Asin => Self::math1(args, |v| v.asin()),
            Self::Acos => Self::math1(args, |v| v.acos()),
            Self::Atan => Self::math1(args, |v| v.atan()),
        }
    }

    fn check_arg_count(args: ArgumentHelper, count: usize) -> Result<Expression, EvalError> {
        args.assert_count(count, count)?;
        Ok(args.return_input())
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

    enum_to_from_str!(
        OpFunction; pub fn name(); pub fn from_name();
        {
            Self::Max => "max",
            Self::Min => "min",
            Self::Angle => "angle",
            Self::Len => "len",
            Self::Noise => "noise",
            Self::Abs => "abs",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Sqrt => "sqrt",
            Self::Rand => "rand",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
        }
    );
}
crate::enum_try_from! {
    impl TryFrom[OpSymbol] for OpFunction [symmetric];
    {   Max, Min, Angle, Len, Noise, Abs, Log, Log10, Floor, Ceil, Sqrt,
        Rand, Sin, Cos, Tan, Asin, Acos, Atan, }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MathFunction {
    Instruction(InstructionKind),
    Op(OpFunction),
}
impl MathFunction {
    pub fn eval_call(&self, name: &str, args: Vec<AValue>) -> Result<Expression, EvalError> {
        match self {
            Self::Instruction(i_kind) => {
                let mut args = ArgumentHelper::new(name, args);
                match i_kind {
                    InstructionKind::Jump => {
                        args.assert_count(1, 2)?;
                        // Add default "true" argument
                        if args.args.len() == 1 {
                            args.args.push(AValue::Num(1f64));
                        }
                    }
                    _ => {
                        let count = i_kind.arg_count() - usize::from(i_kind.has_result());
                        args.assert_count(count, count)?;
                    }
                }
                Ok(args.return_input())
            }
            Self::Op(op) => op.eval_call(name, args),
        }
    }

    pub fn get_function_type(&self) -> FunctionType {
        FunctionType::Eval
    }

    #[allow(unused)]
    pub fn name(&self) -> &'static str {
        match self {
            Self::Instruction(v) => v.name(),
            Self::Op(v) => v.name(),
        }
    }

    pub fn from_name(input: &str) -> Option<Self> {
        // diallow op and set instructions
        match input {
            "op" | "set" => {
                return None;
            }
            _ => {}
        }
        if let Some(op) = OpFunction::from_name(input) {
            Some(Self::Op(op))
        } else {
            InstructionKind::from_name(input).map(Self::Instruction)
        }
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
        Some(BasicOp::from_expr_str(input)?.try_into().unwrap())
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
crate::build_enum_match! {
    math_op_props; MathOp;
    basic_op [enum] => {
        Add, Sub, Mul, Div, IDiv, Mod, LAnd, BAnd, BOr, BXor, Not, Equal, NotEqual,
        LessThan, LessThanEq, GreaterThan, GreaterThanEq, Shl, Shr,
    }
    op_symbol [enum] => {
        Add, Sub, Mul, Div, IDiv, Mod, LAnd, BAnd, BOr, BXor, Equal, NotEqual,
        LessThan, LessThanEq, GreaterThan, GreaterThanEq, Shl, Shr, Not => Flip,
    }
    jump_symbol [enum] =>
        { Equal, NotEqual, LessThan, LessThanEq, GreaterThan, GreaterThanEq, }
}
math_op_props! {basic_op; impl From[Self] for BasicOp}
math_op_props! {basic_op; impl TryFrom[BasicOp] for Self}
math_op_props! {op_symbol; impl TryFrom[Self] for OpSymbol}
math_op_props! {jump_symbol; impl TryFrom[Self] for JumpSymbol}

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
crate::enum_try_from! {
    impl TryFrom[AssignmentOp] for MathOp;
    { Add, Sub, Mul, Div, IDiv, }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MathStatement {
    Assign(AssignStatement),
    Label(LabelStatement),
}
enum_from_variants!(MathStatement;
    Assign(AssignStatement),
    Label(LabelStatement),
);

impl MathStatement {
    pub fn generate(
        &self,
        ctx: &mut CompilerEnv,
        source_name: &Rc<String>,
    ) -> Result<Vec<Statement>, MathError> {
        match self {
            Self::Assign(st) => st.generate(ctx, source_name),
            Self::Label(st) => st.generate(ctx, source_name),
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

#[derive(Debug, Clone, PartialEq)]
pub struct LabelStatement {
    pub pos: Position,
    pub name: String,
}
impl LabelStatement {
    pub fn generate(
        &self,
        _ctx: &mut CompilerEnv,
        source_name: &Rc<String>,
    ) -> Result<Vec<Statement>, MathError> {
        let lbl = Label::new(self.name.clone());
        let source = Source::new(source_name.clone(), self.pos.line, self.pos.column);
        let stmt = Statement::new(source, lbl);
        Ok(vec![stmt])
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
                        let op2 = OpSymbol::try_from(*op).unwrap();
                        self.add_op_instr(expr, op2, left, right)?;
                    }
                    Expression::Unary(op, left) => {
                        let op2 = OpSymbol::try_from(*op).unwrap();
                        self.add_op_instr(expr, op2, left, &zero_expr)?;
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
            (op1, $op_func:ident) => {{
                let op = OpSymbol::try_from($op_func).unwrap();
                self.add_op_instr(expr, op, get_arg(0)?, &AValue::from(0).into())
            }};
            (op2, $op_func:ident) => {{
                let op = OpSymbol::try_from($op_func).unwrap();
                self.add_op_instr(expr, op, get_arg(0)?, get_arg(1)?)
            }};
            (kind, $kind:ident, $result_index:expr) => {{
                self.add_any_instr($kind, expr, args, $result_index)
            }};
        }
        let func =
            MathFunction::from_name(name).ok_or_else(|| EvalError::unknown_function(name))?;
        match func {
            MathFunction::Instruction(kind) => {
                type IK = InstructionKind;
                match kind {
                    IK::Read => add_it!(kind, kind, Some(0)),
                    IK::Write => add_it!(kind, kind, None),
                    IK::Draw => add_it!(kind, kind, None),
                    IK::Print => add_it!(kind, kind, None),
                    IK::DrawFlush => add_it!(kind, kind, None),
                    IK::PrintFlush => add_it!(kind, kind, None),
                    IK::GetLink => add_it!(kind, kind, Some(0)),
                    IK::Control => add_it!(kind, kind, None),
                    IK::Radar => add_it!(kind, kind, Some(6)),
                    IK::Sensor => add_it!(kind, kind, Some(0)),
                    IK::Set => add_it!(kind, kind, Some(0)),
                    IK::Op => add_it!(kind, kind, Some(0)),
                    IK::End => add_it!(kind, kind, None),
                    IK::Jump => self.generate_jump(name, args),
                    IK::UnitBind => add_it!(kind, kind, None),
                    IK::UnitControl => add_it!(kind, kind, None),
                    IK::UnitRadar => add_it!(kind, kind, Some(6)),
                    IK::UnitLocate => add_it!(kind, kind, None),
                }
            }
            MathFunction::Op(op) => {
                type MF = OpFunction;
                match op {
                    MF::Max => add_it!(op2, op),
                    MF::Min => add_it!(op2, op),
                    MF::Angle => add_it!(op2, op),
                    MF::Len => add_it!(op2, op),
                    MF::Noise => add_it!(op2, op),
                    MF::Abs => add_it!(op1, op),
                    MF::Log => add_it!(op2, op),
                    MF::Log10 => add_it!(op1, op),
                    MF::Floor => add_it!(op1, op),
                    MF::Ceil => add_it!(op1, op),
                    MF::Sqrt => add_it!(op1, op),
                    MF::Rand => add_it!(op1, op),
                    MF::Sin => add_it!(op1, op),
                    MF::Cos => add_it!(op1, op),
                    MF::Tan => add_it!(op1, op),
                    MF::Asin => add_it!(op1, op),
                    MF::Acos => add_it!(op1, op),
                    MF::Atan => add_it!(op1, op),
                }
            }
        }
    }

    fn generate_jump(&mut self, name: &str, args: &[Expression]) -> Result<(), MathError> {
        let dest = Self::get_arg(name, args, 0)?;
        let cond = Self::get_arg(name, args, 1)?;
        // Get destination value
        let dest_value = dest
            .as_value()
            .map(|v| {
                InstValue::Value(v.clone())
                // if let Some(name) = v.as_string() {
                //     let sc = &self.string_cache;
                //     InstValue::QuickConstExpr(sc.get(FUNC_LABEL_DEST), sc.get(name))
                // } else {
                //     InstValue::new_num(v.as_f64().unwrap_or_default())
                // }
            })
            .ok_or_else(|| EvalError::invalid_arg(name, 0))?;

        if let Some(v) = cond.as_value() {
            // If we're a constant value, try to convert to bool and jump based on that
            let (symbol, left) = if v.is_truthy() {
                (JumpSymbol::Always, InstValue::new_num(1))
            } else {
                (JumpSymbol::StrictEqual, InstValue::new_num(0))
            };
            let instr = InstructionJump {
                dest: dest_value,
                op: symbol.into_value(&self.string_cache),
                left,
                right: InstValue::new_num(1),
            };
            self.new_tokens
                .push(Statement::new(self.source.clone(), instr));
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
            let instr = InstructionJump {
                dest: dest_value,
                op: last_op.op,
                left: last_op.left,
                right: last_op.right,
            };
            self.new_tokens
                .push(Statement::new(self.source.clone(), instr));
        }
        Ok(())
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
