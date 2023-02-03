use std::ops::Index;

use anyhow::{bail, Result};
use lasso::{Rodeo, Spur};
use stdx::{impl_debug_display, impl_idx_from};
use typed_index_collections::{TiSlice, TiVec};
use typed_indexmap::TiMap;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Expr {
    Eval(ExprPtr),
    Value(Value),
}

impl Expr {
    pub const UNDEF: Expr = Expr::Value(Value::UNDEF);

    pub fn eval(self, ctx: ExprEvalCtxRef) -> Result<Value> {
        match self {
            Expr::Value(val) => Ok(val),
            Expr::Eval(ptr) => ptr.eval(ctx),
        }
    }

    pub fn eval_num(&self, ctx: ExprEvalCtxRef) -> Result<f64> {
        self.eval(ctx)?.to_num()
    }

    pub fn eval_str<'a>(&self, ctx: ExprEvalCtxRef<'a>) -> Result<&'a str> {
        Ok(ctx.intern.resolve(&self.eval(ctx)?.to_str()?))
    }

    pub fn param(arena: &mut Arena, param: CircuitParam) -> Expr {
        Expr::Eval(arena.alloc(ExprData::Param(param)))
    }

    pub fn cond(arena: &mut Arena, cond: Expr, then_val: Expr, else_val: Expr) -> Result<Expr> {
        let res = match cond {
            _ if then_val == else_val => then_val,
            Expr::Eval(cond) => arena.alloc(Cond { cond, then_val, else_val }.into()).into(),
            Expr::Value(val) => {
                if val.to_bool()? {
                    then_val
                } else {
                    else_val
                }
            }
        };
        Ok(res)
    }

    pub fn func_call(
        arena: &mut Arena,
        ctx: CircuitParamCtx,
        func_val: Expr,
        args: Box<[Expr]>,
    ) -> Expr {
        match func_val {
            Expr::Eval(func_val) => {
                arena.alloc(UserFunc { expr: func_val, args, ctx }.into()).into()
            }
            Expr::Value(val) => val.into(),
        }
    }

    pub fn eq(arena: &mut Arena, lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            _ if lhs == rhs => true.into(),
            (Expr::Eval(lhs), rhs) | (rhs, Expr::Eval(lhs)) => {
                arena.alloc(ExprData::Equal(lhs, rhs)).into()
            }
            (Expr::Value(lhs), Expr::Value(rhs)) => (lhs == rhs).into(),
        }
    }

    pub fn neq(arena: &mut Arena, lhs: Expr, rhs: Expr) -> Expr {
        match (lhs, rhs) {
            _ if lhs == rhs => false.into(),
            (Expr::Eval(lhs), rhs) | (rhs, Expr::Eval(lhs)) => {
                arena.alloc(ExprData::NotEqual(lhs, rhs)).into()
            }
            (Expr::Value(lhs), Expr::Value(rhs)) => (lhs != rhs).into(),
        }
    }

    pub fn inv(arena: &mut Arena, arg: Expr) -> Result<Expr> {
        let res = match arg {
            Expr::Eval(arg) => {
                let ptr = if let ExprData::Unary { op: UnaryOp::Inv, arg } = *arena.lookup(arg) {
                    arg
                } else {
                    arena.alloc(ExprData::Unary { op: UnaryOp::Inv, arg })
                };
                ptr.into()
            }
            Expr::Value(arg) => (1.0 / arg.to_num()?).into(),
        };
        Ok(res)
    }

    pub fn mul(arena: &mut Arena, lhs: Expr, rhs: Expr) -> Result<Expr> {
        let res = match (lhs, rhs) {
            (Expr::Value(lhs), Expr::Value(rhs)) => (lhs.to_num()? * rhs.to_num()?).into(),
            (Expr::Eval(lhs), rhs) | (rhs, Expr::Eval(lhs)) => {
                match rhs {
                    Expr::Value(Value::Num(rhs)) => match *arena.lookup(lhs) {
                        ExprData::Commutative {
                            op: CommutativeOp::Mul,
                            rhs: Expr::Value(ref mut dst),
                            ..
                        } => {
                            *dst = (dst.to_num()? * rhs).into();
                            return Ok(lhs.into());
                        }
                        ExprData::Unary { op: UnaryOp::Inv, arg, .. } => {
                            if let ExprData::Commutative {
                                op: CommutativeOp::Mul,
                                rhs: Expr::Value(ref mut dst),
                                ..
                            } = arena.lookup(arg)
                            {
                                *dst = (dst.to_num()? / rhs).into();
                                return Ok(lhs.into());
                            }
                        }

                        _ => (),
                    },
                    Expr::Eval(rhs) => {
                        if let ExprData::Unary { op: UnaryOp::Inv, arg: rhs } = *arena.lookup(rhs) {
                            if let ExprData::Unary { op: UnaryOp::Inv, arg: lhs } =
                                *arena.lookup(lhs)
                            {
                                let arg = Expr::mul(arena, lhs.into(), rhs.into())?;
                                return Expr::inv(arena, arg);
                            }
                        }
                    }
                    _ => (),
                }
                arena.alloc(ExprData::Commutative { op: CommutativeOp::Mul, lhs, rhs }).into()
            }
        };
        Ok(res)
    }

    pub fn neg(arena: &mut Arena, arg: Expr) -> Result<Expr> {
        match arg {
            Expr::Eval(arg) => {
                let ptr = if let ExprData::Unary { op: UnaryOp::Neg, arg } = *arena.lookup(arg) {
                    arg
                } else {
                    arena.alloc(ExprData::Unary { op: UnaryOp::Neg, arg })
                };
                Ok(ptr.into())
            }
            Expr::Value(arg) => Ok((1.0 / arg.to_num()?).into()),
        }
    }

    pub fn add(arena: &mut Arena, lhs: Expr, rhs: Expr) -> Result<Expr> {
        let res = match (lhs, rhs) {
            (Expr::Value(lhs), Expr::Value(rhs)) => (lhs.to_num()? + rhs.to_num()?).into(),
            (Expr::Eval(lhs), rhs) | (rhs, Expr::Eval(lhs)) => {
                match rhs {
                    Expr::Value(Value::Num(rhs)) => match *arena.lookup(lhs) {
                        ExprData::Commutative {
                            op: CommutativeOp::Add,
                            rhs: Expr::Value(ref mut dst),
                            ..
                        } => {
                            *dst = (dst.to_num()? + rhs).into();
                            return Ok(lhs.into());
                        }
                        ExprData::Unary { op: UnaryOp::Neg, arg, .. } => {
                            if let ExprData::Commutative {
                                op: CommutativeOp::Add,
                                rhs: Expr::Value(ref mut dst),
                                ..
                            } = arena.lookup(arg)
                            {
                                *dst = (dst.to_num()? - rhs).into();
                                return Ok(lhs.into());
                            }
                        }

                        _ => (),
                    },
                    Expr::Eval(rhs) => {
                        if let ExprData::Unary { op: UnaryOp::Neg, arg: rhs } = *arena.lookup(rhs) {
                            if let ExprData::Unary { op: UnaryOp::Neg, arg: lhs } =
                                *arena.lookup(lhs)
                            {
                                let arg = Expr::add(arena, lhs.into(), rhs.into())?;
                                return Expr::neg(arena, arg);
                            }
                        }
                    }
                    _ => (),
                }
                arena.alloc(ExprData::Commutative { op: CommutativeOp::Add, lhs, rhs }).into()
            }
        };
        Ok(res)
    }

    fn unary_op(arena: &mut Arena, op: UnaryOp, arg: Expr) -> Result<Expr> {
        let res = match arg {
            Expr::Value(arg) => op.eval(arg.to_num()?).into(),
            Expr::Eval(arg) => arena.alloc(ExprData::Unary { op, arg }).into(),
        };
        Ok(res)
    }

    fn bin_op(arena: &mut Arena, op: BinOp, lhs: Expr, rhs: Expr) -> Result<Expr> {
        let res = if let (Expr::Value(lhs), Expr::Value(rhs)) = (lhs, rhs) {
            op.eval(lhs.to_num()?, rhs.to_num()?).into()
        } else {
            arena.alloc(ExprData::Binary { op, lhs, rhs }).into()
        };
        Ok(res)
    }

    fn commutative_op(arena: &mut Arena, op: CommutativeOp, lhs: Expr, rhs: Expr) -> Result<Expr> {
        match (lhs, rhs) {
            (Expr::Value(lhs), Expr::Value(rhs)) => {
                Ok(op.eval(lhs.to_num()?, rhs.to_num()?).into())
            }
            (Expr::Eval(lhs), rhs) | (rhs, Expr::Eval(lhs)) => {
                Ok(arena.alloc(ExprData::Commutative { op, lhs, rhs }).into())
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Value {
    Num(f64),
    Str(Spur),
    UNDEF,
}

impl Value {
    pub fn to_str(self) -> Result<Spur> {
        match self {
            Value::Num(_) => bail!("expected string but found number"),
            Value::Str(val) => Ok(val),
            Value::UNDEF => unreachable!("encountered UNDEF value"),
        }
    }

    pub fn to_num(self) -> Result<f64> {
        match self {
            Value::Num(val) => Ok(val),
            Value::Str(_) => bail!("expected number but found string"),
            Value::UNDEF => unreachable!("encountered UNDEF value"),
        }
    }

    pub fn to_int(self) -> Result<i32> {
        Ok(self.to_num()?.round() as i32)
    }

    pub fn to_bool(self) -> Result<bool> {
        Ok(self.to_num()? != 0.0)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::Num(val)
    }
}

impl From<i32> for Value {
    fn from(val: i32) -> Self {
        Value::Num(val as f64)
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Value::Num(val as i32 as f64)
    }
}

impl From<ExprPtr> for Expr {
    fn from(ptr: ExprPtr) -> Self {
        Expr::Eval(ptr)
    }
}

impl<T: Into<Value>> From<T> for Expr {
    fn from(val: T) -> Self {
        Expr::Value(val.into())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprPtr(u32);
impl_debug_display!(match ExprPtr{ ExprPtr(id) => "expr{:?}", id;});

#[derive(PartialEq, Debug, Clone)]
enum ExprData {
    Cond(Box<Cond>),
    Equal(ExprPtr, Expr),
    NotEqual(ExprPtr, Expr),
    UserFunc(Box<UserFunc>),
    Commutative { op: CommutativeOp, lhs: ExprPtr, rhs: Expr },
    Binary { op: BinOp, lhs: Expr, rhs: Expr },
    Unary { op: UnaryOp, arg: ExprPtr },
    Param(CircuitParam),
}

impl From<Cond> for ExprData {
    fn from(cond: Cond) -> Self {
        ExprData::Cond(Box::new(cond))
    }
}

impl From<UserFunc> for ExprData {
    fn from(func: UserFunc) -> Self {
        ExprData::UserFunc(Box::new(func))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cond {
    cond: ExprPtr,
    then_val: Expr,
    else_val: Expr,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UserFunc {
    expr: ExprPtr,
    ctx: CircuitParamCtx,
    args: Box<[Expr]>,
}

macro_rules! op_enums {
    ($(
    #[builder($builder: ident ($($arg: ident),*))]
    enum $enum: ident {
        $($op: ident $(= $fn: ident)?),*
    })*) => {$(
        op_enums!(@impl
            $builder, arena, (arena: &mut Arena, $($arg: Expr),*), ($($arg),*),  $enum, $($op $(= $fn)?,)*
        );)*
    };

    (@call $fn: ident [$(($($arg: path),*)),*]) => {
        Expr::$fn ($($($arg),*),*)
    };


    (@impl
        $builder: ident, $arena: ident, $args: tt, $args_forward: tt, $enum: ident,
        $($op: ident $(= $fn: ident)?,)*
    ) => {
        #[derive(PartialEq, Eq, Debug, Clone, Copy)]
        #[allow(clippy::upper_case_acronyms)]
        enum $enum {
            $($op),*
        }

        impl Expr{
            $(
                $(
                    pub fn $fn $args -> Result<Expr>{
                        op_enums!(@call $builder [($arena, $enum::$op), $args_forward])
                    }
                )*
            )?
        }
    };
}

op_enums! {
    #[builder(unary_op(arg))]
    enum UnaryOp {
        Neg,
        Inv,
        Log = log,
        Log10 = log10,
        Exp = exp,
        Sqrt = sqrt,
        Abs = abs,
        Sin = sin,
        Cos = cos,
        Tan = tan,
        ATan = atam,
        ASin = asin,
        ACos = acos,
        SinH = sinh,
        CosH = cosh,
        TanH = tanh,
        ATanH = atanh,
        ASinH = asinh,
        Ceil = ceil,
        Floor = floor,
        Int = int
    }

    #[builder(bin_op(lhs, rhs))]
    enum BinOp {
        Mod = modulo,
        Pow = pow,
        Shl = shl,
        Shr = shr,
        LessThen = lt,
        LessEqual = le,
        Fmod = fmod,
        ATan2 = atan2,
        Hypot = hypot
    }

    #[builder(commutative_op(lhs, rhs))]
    enum CommutativeOp {
        Add,
        Mul,
        Or = or,
        And = and,
        XOR = xolr,
        Min = min,
        Max = max,
        // spectre does not supterminal any short circuiting so we don't either
        LogcialOr = logic_or,
        LogcialAnd = logic_and
    }
}

impl ExprData {
    fn eval(&self, mut ctx: ExprEvalCtxRef) -> Result<Value> {
        let val = match *self {
            ExprData::Param(param) => ctx[param],
            ExprData::Cond(ref cond) => return cond.eval(ctx),
            ExprData::UserFunc(ref func) => return func.eval(ctx),
            ExprData::Equal(lhs, rhs) => {
                let lhs = lhs.eval(ctx.borrow())?;
                let rhs = rhs.eval(ctx)?;
                (lhs == rhs).into()
            }
            ExprData::NotEqual(lhs, rhs) => {
                let lhs = lhs.eval(ctx.borrow())?;
                let rhs = rhs.eval(ctx)?;
                (lhs != rhs).into()
            }
            ExprData::Commutative { op, lhs, rhs } => {
                let lhs = lhs.eval_num(ctx.borrow())?;
                let rhs = rhs.eval_num(ctx)?;
                op.eval(lhs, rhs).into()
            }
            ExprData::Binary { op, lhs, rhs } => {
                let lhs = lhs.eval_num(ctx.borrow())?;
                let rhs = rhs.eval_num(ctx)?;
                op.eval(lhs, rhs).into()
            }
            ExprData::Unary { op, arg } => op.eval(arg.eval_num(ctx)?).into(),
        };

        Ok(val)
    }
}

impl ExprPtr {
    fn eval(self, ctx: ExprEvalCtxRef) -> Result<Value> {
        ctx.lookup(self).eval(ctx)
    }

    fn eval_num(self, ctx: ExprEvalCtxRef) -> Result<f64> {
        ctx.lookup(self).eval(ctx)?.to_num()
    }
}

impl Cond {
    fn eval(&self, mut ctx: ExprEvalCtxRef) -> Result<Value> {
        if self.cond.eval(ctx.borrow())?.to_bool()? {
            self.then_val.eval(ctx)
        } else {
            self.else_val.eval(ctx)
        }
    }
}

impl UserFunc {
    fn eval(&self, mut eval_ctx: ExprEvalCtxRef) -> Result<Value> {
        for (i, val) in self.args.iter().enumerate() {
            let param = CircuitParam { param: i.into(), ctx: self.ctx };
            let val = val.eval(eval_ctx.borrow())?;
            eval_ctx.set_param(param, val);
        }
        self.expr.eval(eval_ctx)
    }
}

macro_rules! int_op {
    (|$lhs: ident, $rhs: ident| $val: expr) => {{
        let $lhs = $lhs.round() as i32;
        let $rhs = $rhs.round() as i32;
        $val as f64
    }};
}

macro_rules! bool_op {
    (|$lhs: ident, $rhs: ident| $val: expr) => {{
        let $lhs = $lhs != 0.0;
        let $rhs = $rhs != 0.0;
        $val as i32 as f64
    }};
}

impl CommutativeOp {
    pub fn eval(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            CommutativeOp::Add => lhs + rhs,
            CommutativeOp::Mul => lhs * rhs,
            CommutativeOp::Or => int_op!(|lhs, rhs| lhs | rhs),
            CommutativeOp::And => int_op!(|lhs, rhs| lhs & rhs),
            CommutativeOp::XOR => int_op!(|lhs, rhs| lhs ^ rhs),
            CommutativeOp::Min => lhs.min(rhs),
            CommutativeOp::Max => lhs.max(rhs),
            CommutativeOp::LogcialOr => bool_op!(|lhs, rhs| lhs || rhs),
            CommutativeOp::LogcialAnd => bool_op!(|lhs, rhs| lhs && rhs),
        }
    }
}

impl BinOp {
    pub fn eval(self, lhs: f64, rhs: f64) -> f64 {
        match self {
            BinOp::Mod => int_op!(|lhs, rhs| lhs % rhs),
            BinOp::Pow => lhs.powf(rhs),
            BinOp::Shl => int_op!(|lhs, rhs| lhs << rhs),
            BinOp::Shr => int_op!(|lhs, rhs| lhs >> rhs),
            BinOp::LessThen => (lhs < rhs) as i32 as f64,
            BinOp::LessEqual => (lhs <= rhs) as i32 as f64,
            BinOp::Fmod => lhs % rhs,
            BinOp::ATan2 => lhs.atan2(rhs),
            BinOp::Hypot => lhs.hypot(rhs),
        }
    }
}

impl UnaryOp {
    pub fn eval(self, arg: f64) -> f64 {
        match self {
            UnaryOp::Log => arg.ln(),
            UnaryOp::Log10 => arg.log10(),
            UnaryOp::Exp => arg.exp(),
            UnaryOp::Sqrt => arg.sqrt(),
            UnaryOp::Abs => arg.abs(),
            UnaryOp::Sin => arg.sin(),
            UnaryOp::Cos => arg.cos(),
            UnaryOp::Tan => arg.tan(),
            UnaryOp::ATan => arg.atan(),
            UnaryOp::ASin => arg.asin(),
            UnaryOp::ACos => arg.acos(),
            UnaryOp::SinH => arg.sinh(),
            UnaryOp::CosH => arg.cosh(),
            UnaryOp::TanH => arg.tanh(),
            UnaryOp::ATanH => arg.atanh(),
            UnaryOp::ASinH => arg.asinh(),
            UnaryOp::Ceil => arg.ceil(),
            UnaryOp::Floor => arg.floor(),
            UnaryOp::Int => arg.round(),
            UnaryOp::Neg => -arg,
            UnaryOp::Inv => 1.0 / arg,
        }
    }
}

impl CircuitParamCtx {
    pub const ROOT: Self = Self(0);
}

impl CircuitParam {
    pub const TEMPERATURE: Self =
        CircuitParam { param: LocalCircuitParam(0), ctx: CircuitParamCtx::ROOT };
}

pub struct Arena {
    exprs: Vec<ExprData>,
    params: TiVec<CircuitParamCtx, TiMap<LocalCircuitParam, String, ParamInfo>>,
    intern: Rodeo,
}

impl Arena {
    pub fn new() -> Arena {
        let mut res = Arena {
            exprs: Vec::with_capacity(256),
            params: TiVec::default(),
            intern: Rodeo::new(),
        };
        let _root = res.add_ctx();
        debug_assert_eq!(_root, CircuitParamCtx::ROOT);
        res.def_param(CircuitParamCtx::ROOT, "temp".to_owned())
            .expect("inserting builtin params should never fail");
        res
    }

    /// Allocates and stores a non-constant expression in the arena and return an [`ExprPtr`] to the allocation.
    fn alloc(&mut self, data: ExprData) -> ExprPtr {
        let idx = self.exprs.len();
        self.exprs.push(data);
        ExprPtr(idx as u32)
    }

    fn lookup(&mut self, ptr: ExprPtr) -> &mut ExprData {
        &mut self.exprs[ptr.0 as usize]
    }

    /// Create a new parameter with name `name` and optionally a default value
    ///
    /// # Returns
    ///
    /// The parameters index and an expression that can be used to read the parameter.
    /// `None` if no parameter `name` was found
    pub fn def_param(
        &mut self,
        ctx: CircuitParamCtx,
        name: String,
    ) -> Result<(CircuitParam, Expr)> {
        let dst = &mut self.params[ctx];
        let param = CircuitParam { param: dst.next_index(), ctx };
        if dst.contains_key(&name) {
            bail!("parameter {name} was definied multiple times")
        }
        let read_expr = Expr::param(self, param);
        let info = ParamInfo { read_expr, name: name.clone() };
        self.params[ctx].insert(name, info);
        Ok((param, read_expr))
    }

    /// Add a new context for parameter
    pub fn add_ctx(&mut self) -> CircuitParamCtx {
        self.params.push_and_get_key(TiMap::default())
    }

    /// Lookup an parameter by name
    ///
    /// # Returns
    ///
    /// The parameters index and an expression that can be used to read the parameter.
    /// `None` if no parameter `name` was found
    pub fn lookup_param_by_name(
        &self,
        ctx: CircuitParamCtx,
        name: &str,
    ) -> Option<(CircuitParam, Expr)> {
        self.params
            .get(ctx)?
            .index_and_val(name)
            .map(|(param, info)| (CircuitParam { param, ctx }, info.read_expr))
    }

    /// Lookup information about a parameter
    ///
    /// # Returns
    ///
    /// The parameters name and an expression that can be used to read the parameter.
    /// `None` if this parameter does not belong to this earena
    pub fn lookup_param_info(&self, param: CircuitParam) -> Option<(&str, Expr)> {
        let info = &self.params.get(param.ctx)?.get_index(param.param)?.1;
        Some((&info.name, info.read_expr))
    }

    /// Iterate all parameters in a specific contex
    ///
    /// # Returns
    ///
    /// An (exct size) iterator that yields all parameters in `ctx`
    pub fn ctx_params(
        &self,
        ctx: CircuitParamCtx,
    ) -> impl Iterator<Item = CircuitParam> + ExactSizeIterator {
        self.params[ctx].keys().map(move |param| CircuitParam { ctx, param })
    }
}

struct ParamInfo {
    name: String,
    read_expr: Expr,
}

impl Default for Arena {
    fn default() -> Self {
        Arena::new()
    }
}

#[derive(Debug)]
pub struct ExprEvalCtxRef<'a> {
    arena: &'a [ExprData],
    params: &'a mut [Value],
    ctx_offsets: &'a TiSlice<CircuitParamCtx, u32>,
    intern: &'a Rodeo,
}

impl<'a> ExprEvalCtxRef<'a> {
    #[inline(always)]
    pub fn borrow(&mut self) -> ExprEvalCtxRef<'_> {
        ExprEvalCtxRef {
            arena: self.arena,
            params: &mut *self.params,
            ctx_offsets: self.ctx_offsets,
            intern: self.intern,
        }
    }

    fn lookup(&self, ptr: ExprPtr) -> &'a ExprData {
        &self.arena[ptr.0 as usize]
    }

    pub fn set_param(&mut self, param: CircuitParam, val: Value) {
        let off: u32 = self.ctx_offsets[param.ctx] + u32::from(param.param);
        self.params[off as usize] = val;
    }
}

impl<'a> Index<CircuitParam> for ExprEvalCtxRef<'a> {
    type Output = Value;

    fn index(&self, param: CircuitParam) -> &Value {
        let off: u32 = self.ctx_offsets[param.ctx] + u32::from(param.param);
        &self.params[off as usize]
    }
}

#[derive(Debug)]
pub struct ExprEvalCtx<'a> {
    arena: &'a [ExprData],
    intern: &'a Rodeo,
    params: Box<[Value]>,
    ctx_offsets: Box<TiSlice<CircuitParamCtx, u32>>,
}

impl<'a> ExprEvalCtx<'a> {
    pub fn new(arena: &'a Arena) -> ExprEvalCtx<'a> {
        let mut num_params = 0;
        let ctx_offsets = arena
            .params
            .iter()
            .map(|ctx| {
                let ctx_off = num_params;
                num_params += ctx.len();
                ctx_off as u32
            })
            .collect();
        ExprEvalCtx {
            arena: &*arena.exprs,
            intern: &arena.intern,
            params: vec![Value::UNDEF; num_params].into_boxed_slice(),
            ctx_offsets,
        }
    }

    #[inline(always)]
    pub fn borrow(&mut self) -> ExprEvalCtxRef<'_> {
        ExprEvalCtxRef {
            arena: self.arena,
            intern: self.intern,
            params: &mut *self.params,
            ctx_offsets: &*self.ctx_offsets,
        }
    }

    pub fn set_param(&mut self, param: CircuitParam, val: Value) {
        let off: u32 = self.ctx_offsets[param.ctx] + u32::from(param.param);
        self.params[off as usize] = val;
    }

    pub fn resolve_str(&self, str: Spur) -> &'a str {
        self.intern.resolve(&str)
    }
}

impl<'a> Index<CircuitParam> for ExprEvalCtx<'a> {
    type Output = Value;

    fn index(&self, param: CircuitParam) -> &Value {
        let off: u32 = self.ctx_offsets[param.ctx] + u32::from(param.param);
        &self.params[off as usize]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CircuitParam {
    param: LocalCircuitParam,
    ctx: CircuitParamCtx,
}

impl_debug_display!(match CircuitParam{ CircuitParam{ctx, param} => "{ctx:?}::{param:?}";});

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LocalCircuitParam(u32);
impl_debug_display!(match LocalCircuitParam{ LocalCircuitParam(id) => "param{id:?}";});
impl_idx_from!(LocalCircuitParam(u32));

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CircuitParamCtx(u32);
impl_debug_display!(match CircuitParamCtx{ CircuitParamCtx(id) => "ctx{id:?}";});
impl_idx_from!(CircuitParamCtx(u32));
