use ahash::{AHashMap, AHashSet};
use hir::{Body, CompilationDB, Node, Type, Variable};
use hir::{BodyRef, ExprId};
use mir::builder::InstBuilder;
use mir::{Block, FuncRef, Opcode, Value, FALSE, F_ZERO, TRUE};
use mir_build::{FunctionBuilder, Place};
use stdx::iter::zip;
use typed_index_collections::TiVec;
use typed_indexmap::TiSet;

use crate::{
    CallBackKind, Dim, HirInterner, ImplicitEquation, ImplicitEquationKind, LimitState, ParamKind,
    PlaceKind, REACTIVE_DIM, RESISTIVE_DIM,
};

macro_rules! match_signature {
    ($signature:ident: $($case:ident $(| $extra_case:ident)* => $res:expr),*) => {
        match $signature {
            $($case $(|$extra_case)* => $res,)*
            signature => unreachable!("invalid signature {:?}",signature)
        }

    };
}

mod expr;
mod stmt;

pub struct LoweringCtx<'a, 'c> {
    pub db: &'a CompilationDB,
    pub data: &'a mut HirInterner,
    pub places: &'a mut TiSet<Place, PlaceKind>,
    pub func: &'a mut FunctionBuilder<'c>,
    pub body: BodyRef<'a>,
    pub path: &'a str,
    pub tagged_vars: &'a AHashSet<Variable>,
    pub extra_dims: Option<&'a mut TiVec<Dim, AHashMap<ExprId, Value>>>,
    pub contribute_rhs: bool,
    pub inside_lim: bool,
}

impl LoweringCtx<'_, '_> {
    pub fn lower_entry_stmts(&mut self) {
        for &stmnt in self.body.entry() {
            self.lower_stmt(stmnt)
        }
    }
    pub fn place(&mut self, kind: PlaceKind) -> Place {
        Self::place_(self.func, self.data, self.places, kind)
    }

    fn place_(
        func: &mut FunctionBuilder,
        intern: &mut HirInterner,
        places: &mut TiSet<Place, PlaceKind>,
        kind: PlaceKind,
    ) -> Place {
        let (place, inserted) = places.ensure(kind);
        if inserted {
            match kind {
                PlaceKind::Var(var) => {
                    let hidden_state = intern.ensure_param(func.func, ParamKind::HiddenState(var));
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, hidden_state, entry);
                }
                // always initialized
                PlaceKind::FunctionReturn { .. }
                | PlaceKind::FunctionArg { .. }
                | PlaceKind::Param(_)
                | PlaceKind::ParamMin(_)
                | PlaceKind::ParamMax(_) => (),

                // always zero initialized
                PlaceKind::ImplicitResidual { .. } | PlaceKind::Contribute { .. } => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, F_ZERO, entry);
                }
                PlaceKind::CollapseImplicitEquation(_) => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, TRUE, entry);
                }
                PlaceKind::IsVoltageSrc(_) => {
                    let entry = func.func.layout.entry_block().unwrap();
                    func.def_var_at(place, FALSE, entry);
                }
            }
        }
        place
    }

    pub fn param(&mut self, kind: ParamKind) -> Value {
        self.data.ensure_param(self.func.func, kind)
    }

    pub fn callback(&mut self, kind: CallBackKind) -> FuncRef {
        let data = kind.signature();
        let (func_ref, changed) = self.data.callbacks.ensure(kind);
        if changed {
            let sig = self.func.func.import_function(data);
            debug_assert_eq!(func_ref, sig);
        }
        func_ref
    }

    fn node(&self, node: Node) -> Option<Node> {
        if node.is_gnd(self.db) {
            None
        } else {
            Some(node)
        }
    }

    fn nodes_from_args(
        &mut self,
        args: &[ExprId],
        kind: impl Fn(Node, Option<Node>) -> ParamKind,
    ) -> Value {
        let hi = self.body.into_node(args[0]);
        let lo = args.get(1).map(|&arg| self.body.into_node(arg));
        self.nodes(hi, lo, kind)
    }

    fn nodes(
        &mut self,
        hi: Node,
        lo: Option<Node>,
        kind: impl Fn(Node, Option<Node>) -> ParamKind,
    ) -> Value {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.param(kind(hi, None)),
            (None, Some(lo)) => {
                let lo = self.param(kind(lo, None));
                self.func.ins().fneg(lo)
            }
            // TODO refactor to nice if let binding when stable
            (Some(hi), Some(lo)) => {
                if let Some(inverted) = self.data.params.raw.get(&kind(lo, Some(hi))) {
                    self.func.ins().fneg(*inverted)
                } else {
                    self.param(kind(hi, Some(lo)))
                }
            }
            (None, None) => F_ZERO,
        }
    }

    fn lower_select(
        &mut self,
        cond: ExprId,
        lower_then_val: impl FnMut(&mut LoweringCtx) -> Value,
        lower_else_val: impl FnMut(&mut LoweringCtx) -> Value,
    ) -> Value {
        let cond = self.lower_expr(cond);
        self.lower_select_with(cond, lower_then_val, lower_else_val)
    }

    fn lower_select_with(
        &mut self,
        cond: Value,
        mut lower_then_val: impl FnMut(&mut LoweringCtx) -> Value,
        mut lower_else_val: impl FnMut(&mut LoweringCtx) -> Value,
    ) -> Value {
        self.func.make_select(cond, |func, branch| {
            let mut ctx = LoweringCtx {
                db: self.db,
                data: self.data,
                func,
                body: self.body.clone(),
                tagged_vars: self.tagged_vars,
                places: self.places,
                extra_dims: self.extra_dims.as_deref_mut(),
                path: self.path,
                contribute_rhs: self.contribute_rhs,
                inside_lim: self.inside_lim,
            };
            if branch {
                lower_then_val(&mut ctx)
            } else {
                lower_else_val(&mut ctx)
            }
        })
    }

    fn lower_cond_with<T>(
        &mut self,
        cond: Value,
        mut lower_body: impl FnMut(&mut LoweringCtx, bool) -> T,
    ) -> ((Block, T), (Block, T)) {
        self.func.make_cond(cond, |func, branch| {
            let mut ctx = LoweringCtx {
                db: self.db,
                data: self.data,
                func,
                body: self.body.clone(),
                tagged_vars: self.tagged_vars,
                places: self.places,
                extra_dims: self.extra_dims.as_deref_mut(),
                path: self.path,
                contribute_rhs: self.contribute_rhs,
                inside_lim: self.inside_lim,
            };
            lower_body(&mut ctx, branch)
        })
    }

    fn lower_multi_select<const N: usize>(
        &mut self,
        cond: Value,
        lower_body: impl FnMut(&mut LoweringCtx, bool) -> [Value; N],
    ) -> [Value; N] {
        let ((then_bb, mut then_vals), (else_bb, else_vals)) =
            self.lower_cond_with(cond, lower_body);
        for (then_val, else_val) in zip(&mut then_vals, else_vals) {
            *then_val = self.func.ins().phi(&[(then_bb, *then_val), (else_bb, else_val)]);
        }
        then_vals
    }

    fn insert_cast(&mut self, val: Value, src: &Type, dst: &Type) -> Value {
        let op = match (dst, src) {
            (Type::Real, Type::Integer) => Opcode::IFcast,
            (Type::Integer, Type::Real) => Opcode::FIcast,
            (Type::Bool, Type::Real) => Opcode::FBcast,
            (Type::Real, Type::Bool) => Opcode::BFcast,
            (Type::Integer, Type::Bool) => Opcode::BIcast,
            (Type::Bool, Type::Integer) => Opcode::IBcast,
            (Type::Array { .. }, Type::EmptyArray) | (Type::EmptyArray, Type::Array { .. }) => {
                return val
            }
            _ => unreachable!("unknown cast found  {:?} -> {:?}", src, dst),
        };
        let inst = self.func.ins().unary(op, val).0;
        self.func.func.dfg.first_result(inst)
    }

    fn implicit_eqation(&mut self, kind: ImplicitEquationKind) -> (ImplicitEquation, Value) {
        let equation = self.data.implicit_equations.push_and_get_key(kind);
        let place = self.place(PlaceKind::CollapseImplicitEquation(equation));
        self.func.def_var(place, FALSE);
        let val = self.param(ParamKind::ImplicitUnknown(equation));
        (equation, val)
    }

    fn define_resist_residual(&mut self, residual_val: Value, equation: ImplicitEquation) {
        let place = PlaceKind::ImplicitResidual { equation, dim: RESISTIVE_DIM };
        let place = self.place(place);
        self.func.def_var(place, residual_val);
    }

    fn define_react_residual(&mut self, residual_val: Value, equation: ImplicitEquation) {
        let place = PlaceKind::ImplicitResidual { equation, dim: REACTIVE_DIM };
        let place = self.place(place);
        self.func.def_var(place, residual_val);
    }

    fn limit_state(&mut self, probe: ExprId) -> (Value, LimitState) {
        let new_val = self.lower_expr(probe);
        let mut unknown = new_val;
        if let Some(inst) = self.func.func.dfg.value_def(unknown).inst() {
            debug_assert_eq!(self.func.func.dfg.insts[inst].opcode(), Opcode::Fneg);
            unknown = self.func.func.dfg.instr_args(inst)[0];
        }
        let dst = self.data.lim_state.raw.entry(unknown);
        let state = LimitState::from(dst.index());
        dst.or_default().push((new_val, new_val != unknown));

        (new_val, state)
    }

    fn insert_limit(&mut self, state: LimitState, mut val: Value) -> Value {
        let func_ref = self.callback(CallBackKind::StoreLimit(state));
        val = self.func.ins().call1(func_ref, &[val]);
        self.data.lim_state[state].last_mut().unwrap().0 = val;
        val
    }
}

impl HirInterner {
    /// Lowers a body
    pub fn lower_expr_body(
        &mut self,
        db: &CompilationDB,
        body: Body,
        i: usize,
        func: &mut FunctionBuilder,
    ) -> Value {
        let expr = body.borrow().get_entry_expr(i);
        LoweringCtx {
            db,
            data: self,
            func,
            path: "",
            body: body.borrow(),
            tagged_vars: &AHashSet::new(),
            places: &mut TiSet::default(),
            extra_dims: None,
            contribute_rhs: false,
            inside_lim: false,
        }
        .lower_expr(expr)
    }
}
