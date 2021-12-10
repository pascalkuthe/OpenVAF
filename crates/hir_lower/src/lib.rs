use std::vec;

use ahash::AHashMap;
use cfg::{
    BasicBlock, CfgBuilder, CfgParam, Const, ControlFlowGraph, Local, Op, Operand, Place,
    Terminator,
};
use hir_def::body::Body;
use hir_def::expr::CaseCond;
use hir_def::{
    BranchId, BuiltIn, Case, DefWithBodyId, ExprId, FunctionId, LocalFunctionArgId, NodeId,
    ParamId, Stmt, StmtId, Type, VarId,
};
use hir_ty::builtin::{
    ABS_INT, ABS_REAL, DDX_POT, IDT_NO_IC, MAX_INT, MAX_REAL, NATURE_ACCESS_BRANCH,
    NATURE_ACCESS_NODES, NATURE_ACCESS_NODE_GND, NATURE_ACCESS_PORT_FLOW, SIMPARAM_DEFAULT,
    SIMPARAM_NO_DEFAULT,
};
use hir_ty::db::HirTyDB;
use hir_ty::inference::{AssignDst, BranchWrite, InferenceResult, ResolvedFun};
use hir_ty::types::{Ty as HirTy, BOOL_EQ, INT_EQ, INT_OP, REAL_EQ, REAL_OP, STR_EQ};
use stdx::{impl_debug, impl_idx_from};
use syntax::ast::{AssignOp, BinaryOp, UnaryOp};
use ty_index_set::TiSet;
use typed_index_collections::{TiSlice, TiVec};

#[cfg(test)]
mod tests;

macro_rules! match_signature {
    ($signature:ident: $($case:ident $(| $extra_case:ident)* => $res:expr),*) => {
        match *$signature.unwrap(){
            $($case $(|$extra_case)* => $res,)*
            signature => unreachable!("invalid signature {:?}",signature)
        }

    };
}

enum Unkown {
    Simple(CfgParam),
    NodePot(NodeId),
}

pub type RequestedDerivatives = AHashMap<Local, (Operand, Vec<(CfgParam, f64)>)>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ParamKind {
    Param(ParamId),
    Voltage { hi: NodeId, lo: Option<NodeId> },
    Current { branch: BranchId },
    ImplictCurrent { hi: NodeId, lo: Option<NodeId> },
    PortCurrent { port: NodeId },
    Temperature,
    ParamGiven { param: ParamId },
    PortConnected { port: NodeId },
}

impl ParamKind {
    fn unwrap_pot_node(&self) -> NodeId {
        match self {
            ParamKind::Voltage { hi, lo: None } => *hi,
            _ => unreachable!("called unwrap_pot_node on {:?}", self),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PlaceKind {
    Var(VarId),
    FunctionReturn { fun: FunctionId },
    BranchVoltage(BranchId),
    BranchCurrent(BranchId),
    ImplicitBranchVoltage { hi: NodeId, lo: Option<NodeId> },
    ImplicitBranchCurrent { hi: NodeId, lo: Option<NodeId> },
}

pub mod callbacks {
    use cfg::Callback;

    pub const SIM_PARAM: Callback = Callback(0);
    pub const SIM_PARAM_OPT: Callback = Callback(1);
    pub const SIM_PARAM_STR: Callback = Callback(2);
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CfgNodeId(u32);
impl_idx_from!(CfgNodeId(u32));
impl_debug!(match CfgNodeId{node => "node{}",node.0;});

pub struct LoweringResult {
    pub cfg: ControlFlowGraph,
    pub params: TiSet<CfgParam, ParamKind>,
    pub places: TiSet<Place, PlaceKind>,
    pub derivatives: RequestedDerivatives,
}

impl LoweringResult {
    pub fn lower_body(db: &dyn HirTyDB, def: DefWithBodyId) -> LoweringResult {
        let mut cfg = ControlFlowGraph::default();
        let body = db.body(def);
        let infere = db.inference_result(def);

        let mut ctx_data = LoweringCtxData {
            cfg: CfgBuilder::new_cfg(&mut cfg),
            params: TiSet::default(),
            places: TiSet::default(),
            db,
            derivatives: AHashMap::new(),
        };

        let mut ctx = LoweringCtx {
            data: &mut ctx_data,
            body: &body,
            infere: &infere,
            args: TiVec::new().into_boxed_slice(),
        };

        ctx.lower_entry_stmts();
        ctx_data.cfg.terminate(Terminator::Goto(ctx_data.cfg.cfg.blocks.len().into()));
        ctx_data.cfg.enter_new_block();
        ctx_data.cfg.terminate(Terminator::End);

        LoweringResult {
            derivatives: ctx_data.finish_derivatives(),
            places: ctx_data.places,
            params: ctx_data.params,
            cfg,
        }
    }
}

pub struct LoweringCtxData<'a> {
    pub db: &'a dyn HirTyDB,
    pub cfg: CfgBuilder<'a>,
    pub params: TiSet<CfgParam, ParamKind>,
    pub places: TiSet<Place, PlaceKind>,
    derivatives: AHashMap<Local, (Operand, Unkown)>,
}

impl LoweringCtxData<'_> {
    pub fn finish_derivatives(&self) -> RequestedDerivatives {
        self.derivatives
            .iter()
            .map(|(local, (operand, unkown))| {
                let vals = match unkown {
                    Unkown::Simple(param) => vec![(*param, 1.0)],
                    Unkown::NodePot(node) => self
                        .params
                        .iter_enumerated()
                        .filter_map(|(param, kind)| match kind {
                            ParamKind::Voltage { hi, .. } if hi == node => Some((param, 1.0)),
                            ParamKind::Voltage { lo: Some(lo), .. } if lo == node => {
                                Some((param, -1.0))
                            }
                            _ => None,
                        })
                        .collect(),
                };

                (*local, (operand.clone(), vals))
            })
            .collect()
    }
}

pub struct LoweringCtx<'a, 'c> {
    pub data: &'c mut LoweringCtxData<'a>,
    pub body: &'c Body,
    pub infere: &'c InferenceResult,
    pub args: Box<TiSlice<LocalFunctionArgId, Operand>>,
}

impl LoweringCtx<'_, '_> {
    pub fn lower_entry_stmts(&mut self) {
        for stmt in &*self.body.entry_stmts {
            self.lower_stmt(*stmt)
        }
    }

    pub fn lower_stmt(&mut self, stmt: StmtId) {
        match self.body.stmts[stmt] {
            Stmt::Empty | Stmt::Missing => (),
            Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            Stmt::EventControl { .. } => {
                // TODO handle porperly
            }
            Stmt::Assigment { val, assignment_kind, .. } => {
                let val_ = self.lower_expr(val);

                let mut negate = false;

                let place = match self.infere.assigment_destination[&stmt] {
                    AssignDst::Var(var) => self.place(PlaceKind::Var(var)),
                    AssignDst::FunVar { fun, arg: None } => {
                        self.place(PlaceKind::FunctionReturn { fun })
                    }
                    AssignDst::FunVar { arg: Some(arg), .. } => {
                        self.args[arg].clone().unwrap_place()
                    }
                    AssignDst::Flow(BranchWrite::Explicit(branch)) => {
                        self.place(PlaceKind::BranchCurrent(branch))
                    }
                    AssignDst::Potential(BranchWrite::Explicit(branch)) => {
                        self.place(PlaceKind::BranchVoltage(branch))
                    }
                    // TODO verify no contributes between GND nodes
                    AssignDst::Flow(BranchWrite::Implict { hi, lo }) => self
                        .lower_contribute_implict_branch(
                            &mut negate,
                            hi,
                            lo,
                            |hi, lo| PlaceKind::ImplicitBranchCurrent { hi, lo },
                            |hi, lo| ParamKind::ImplictCurrent { hi, lo },
                        ),
                    AssignDst::Potential(BranchWrite::Implict { hi, lo }) => self
                        .lower_contribute_implict_branch(
                            &mut negate,
                            hi,
                            lo,
                            |hi, lo| PlaceKind::ImplicitBranchVoltage { hi, lo },
                            |hi, lo| ParamKind::Voltage { hi, lo },
                        ),
                };
                if assignment_kind == AssignOp::Contribute {
                    let op = if negate { Op::RealSub } else { Op::RealAdd };
                    self.data.cfg.build_assign(
                        place.into(),
                        op,
                        vec![place.into(), val_],
                        val.into(),
                    );
                } else {
                    self.data.cfg.build_assign(place.into(), Op::Copy, vec![val_], val.into());
                }
            }
            Stmt::Block { ref body } => {
                for stmt in body {
                    self.lower_stmt(*stmt)
                }
            }
            Stmt::If { cond, then_branch, else_branch } => {
                self.lower_cond(cond, |s| s.lower_stmt(then_branch), |s| s.lower_stmt(else_branch));
            }
            Stmt::ForLoop { init, cond, incr, body } => {
                self.lower_stmt(init);
                self.lower_loop(cond, |s| {
                    s.lower_stmt(body);
                    s.lower_stmt(incr);
                });
            }
            Stmt::WhileLoop { cond, body } => self.lower_loop(cond, |s| s.lower_stmt(body)),
            Stmt::Case { discr, ref case_arms } => self.lower_case(discr, case_arms),
        }
    }

    fn lower_case(&mut self, discr: ExprId, case_arms: &[Case]) {
        let discr_op = match self.infere.expr_types[discr].to_value().unwrap() {
            Type::Real => Op::RealEq,
            Type::Integer => Op::IntEq,
            Type::Bool => Op::BoolEq,
            Type::String => Op::StringEq,
            Type::Array { .. } => todo!(),
            ty => unreachable!("Invalid type {}", ty),
        };
        let discr = self.lower_expr(discr);

        let end = self.data.cfg.create_block();

        for Case { cond, body } in case_arms {
            // TODO does default mean that further cases are ignored?
            // standard seems to suggest that no matter where the default case is placed that all
            // other conditions are tested prior
            let vals = match cond {
                CaseCond::Vals(vals) => vals,
                CaseCond::Default => continue,
            };

            // save position of the condition for later
            let cond_head = self.data.cfg.current;

            // First lower the body
            let body_head = self.data.cfg.enter_new_block();

            self.lower_stmt(*body);
            // At the end of the body skip to the end of the case
            self.data.cfg.terminate(Terminator::Goto(end));

            // now do the conditions...
            // Go back to that block
            self.data.cfg.current = cond_head;

            for val in vals {
                // Lower the condition (val == discriminant)
                let val_ = self.lower_expr(*val);
                let cond =
                    self.data.cfg.build_val(discr_op, vec![val_, discr.clone()], (*val).into());

                // Create the next block (either the next condition or another arm)

                // Save the current block (still needs to be terminated)
                let cond_block = self.data.cfg.current;
                let next_block = self.data.cfg.enter_new_block();

                // If the condition is true go to the body otherwise try the next block
                self.data.cfg.terminate_bb(
                    cond_block,
                    Terminator::Split {
                        condition: cond.into(),
                        true_block: body_head,
                        false_block: next_block,
                        loop_head: false,
                    },
                );
            }
        }

        if let Some(default_case) =
            case_arms.iter().find(|arm| matches!(arm.cond, CaseCond::Default))
        {
            self.lower_stmt(default_case.body);
            self.data.cfg.terminate(Terminator::Goto(end))
        }

        self.data.cfg.current = end
    }

    fn lower_loop(&mut self, cond: ExprId, lower_body: impl FnOnce(&mut Self)) {
        let start = self.data.cfg.current;

        let loop_cond_head = self.data.cfg.enter_new_block();
        let condition = self.lower_expr(cond);
        let loop_cond_tail = self.data.cfg.current;

        let loop_body_head = self.data.cfg.enter_new_block();
        lower_body(self);
        let loop_body_tail = self.data.cfg.current;

        let loop_end = self.data.cfg.enter_new_block();

        self.data.cfg.terminate_bb(
            loop_cond_tail,
            Terminator::Split {
                condition,
                true_block: loop_body_head,
                false_block: loop_end,
                loop_head: true,
            },
        );
        self.data.cfg.terminate_bb(start, Terminator::Goto(loop_cond_head));
        self.data.cfg.terminate_bb(loop_body_tail, Terminator::Goto(loop_cond_head));
    }

    fn lower_cond<T>(
        &mut self,
        cond: ExprId,
        lower_then: impl FnOnce(&mut Self) -> T,
        lower_else: impl FnOnce(&mut Self) -> T,
    ) -> ((BasicBlock, T), (BasicBlock, T)) {
        let cond = self.lower_expr(cond);
        let start = self.data.cfg.current;

        let then_head_bb = self.data.cfg.enter_new_block();
        let then_val = lower_then(self);
        let then_tail_bb = self.data.cfg.current;

        let else_head_bb = self.data.cfg.enter_new_block();
        let else_val = lower_else(self);
        let else_tail_bb = self.data.cfg.current;

        let end = self.data.cfg.enter_new_block();
        self.data.cfg.terminate_bb(else_tail_bb, Terminator::Goto(end));
        self.data.cfg.terminate_bb(then_tail_bb, Terminator::Goto(end));

        self.data.cfg.terminate_bb(
            start,
            Terminator::Split {
                condition: cond,
                true_block: then_head_bb,
                false_block: else_head_bb,
                loop_head: false,
            },
        );

        ((then_tail_bb, then_val), (else_tail_bb, else_val))
    }

    fn place(&mut self, kind: PlaceKind) -> Place {
        self.data.places.ensure(kind).0
    }

    fn param(&mut self, kind: ParamKind) -> CfgParam {
        self.data.params.ensure(kind).0
    }

    pub fn lower_expr(&mut self, expr: ExprId) -> Operand {
        let res: Operand = match self.body.exprs[expr] {
            hir_def::Expr::Path { port: false, .. } => match self.infere.expr_types[expr] {
                HirTy::Var(_, var) => self.place(PlaceKind::Var(var)).into(),
                HirTy::FuntionVar { fun, arg: None, .. } => {
                    self.place(PlaceKind::FunctionReturn { fun }).into()
                }
                HirTy::FuntionVar { arg: Some(arg), .. } => self.args[arg].clone(),
                HirTy::Param(_, param) => self.param(ParamKind::Param(param)).into(),
                _ => unreachable!(),
            },
            hir_def::Expr::BinaryOp { lhs, rhs, op: Some(op) } => {
                self.lower_bin_op(expr, lhs, rhs, op).into()
            }
            hir_def::Expr::UnaryOp { expr: arg, op } => self.lower_unary_op(expr, arg, op),
            hir_def::Expr::Select { cond, then_val, else_val } => self
                .lower_select(expr, cond, |s| s.lower_expr(then_val), |s| s.lower_expr(else_val))
                .into(),
            hir_def::Expr::Call { ref args, .. } => match self.infere.resolved_calls[&expr] {
                ResolvedFun::User(fun) => self.lower_user_fun(expr, fun, args),
                ResolvedFun::BuiltIn(builtin) => self.lower_builtin(expr, builtin, args),
            },
            hir_def::Expr::Array(ref vals) => self.lower_array(expr, vals),
            hir_def::Expr::Literal(lit) => match lit {
                hir_def::Literal::String(str) => Const::String(str).into(),
                hir_def::Literal::Int(val) => Const::Int(val).into(),
                hir_def::Literal::Float(val) => Const::Real(val.into()).into(),
                hir_def::Literal::Inf => Const::Real(f64::INFINITY).into(),
            },
            ref expr => unreachable!(
                "encounted invalid expr {:?}: this should have been caught in the frontend",
                expr
            ),
        };

        if let Some(dst) = self.infere.casts.get(&expr) {
            self.insert_cast(expr, res, dst)
        } else {
            res
        }
    }

    fn insert_cast(&mut self, expr: ExprId, val: Operand, dst: &Type) -> Operand {
        let src = self.infere.expr_types[expr].to_value().unwrap();
        let op = match (dst, src) {
            (Type::Real, Type::Integer) => Op::IntToReal,
            (Type::Integer, Type::Real) => Op::RealToInt,
            (Type::Real, Type::Bool) => Op::BoolToReal,
            (Type::Integer, Type::Bool) => Op::BoolToInt,
            (Type::Bool, Type::Integer) => Op::IntToBool,
            (Type::Array { .. }, Type::EmptyArray) | (Type::EmptyArray, Type::Array { .. }) => {
                return val
            }
            (dst @ Type::Array { .. }, src @ Type::Array { .. }) => {
                match (dst.base_type(), src.base_type()) {
                    (Type::Real, Type::Integer) => Op::ArrIntToReal,
                    (Type::Integer, Type::Real) => Op::ArrRealToInt,
                    (Type::Real, Type::Bool) => Op::ArrBoolToReal,
                    (Type::Integer, Type::Bool) => Op::ArrBoolToInt,
                    (Type::Bool, Type::Integer) => Op::ArrIntToBool,
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };
        self.data.cfg.build_val(op, vec![val], expr.into()).into()
    }

    fn lower_unary_op(&mut self, expr: ExprId, arg: ExprId, op: UnaryOp) -> Operand {
        let arg = self.lower_expr(arg);
        let op = match op {
            UnaryOp::BitNegate => Op::IntBitNegate,
            UnaryOp::Not => Op::BoolBitNegate,
            UnaryOp::Neg => match self.infere.resolved_signatures[&expr] {
                REAL_OP => Op::RealArtihNeg,
                INT_OP => Op::IntArithNeg,
                _ => unreachable!(),
            },
            UnaryOp::Identity => return arg,
        };

        self.data.cfg.build_val(op, vec![arg], expr.into()).into()
    }

    fn lower_array(&mut self, _expr: ExprId, _args: &[ExprId]) -> Operand {
        todo!("arrays")
    }

    fn node(&self, node: NodeId) -> Option<NodeId> {
        if self.data.db.node_data(node).is_gnd {
            None
        } else {
            Some(node)
        }
    }

    fn nodes_from_args(
        &mut self,
        expr: ExprId,
        args: &[ExprId],
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Operand {
        let hi = self.infere.expr_types[args[0]].unwrap_node();
        let lo = args.get(1).map(|arg| self.infere.expr_types[*arg].unwrap_node());
        self.nodes(expr, hi, lo, kind)
    }

    fn nodes(
        &mut self,
        expr: ExprId,
        hi: NodeId,
        lo: Option<NodeId>,
        kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Operand {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.param(kind(hi, None)).into(),
            (None, Some(lo)) => {
                let lo = self.param(kind(lo, None));
                self.data.cfg.build_val(Op::RealArtihNeg, vec![lo.into()], expr.into()).into()
            }
            // TODO refactor to nice if let binding when stable
            (Some(hi), Some(lo)) => {
                if let Some(inverted) = self.data.params.index(&kind(lo, Some(hi))) {
                    self.data
                        .cfg
                        .build_val(Op::RealArtihNeg, vec![inverted.into()], expr.into())
                        .into()
                } else {
                    self.param(kind(hi, Some(lo))).into()
                }
            }
            (None, None) => 0.0.into(),
        }
    }

    fn lower_contribute_implict_branch(
        &mut self,
        negate: &mut bool,
        hi: NodeId,
        lo: Option<NodeId>,
        kind: impl Fn(NodeId, Option<NodeId>) -> PlaceKind,
        param_kind: impl Fn(NodeId, Option<NodeId>) -> ParamKind,
    ) -> Place {
        let hi = self.node(hi);
        let lo = lo.and_then(|lo| self.node(lo));
        match (hi, lo) {
            (Some(hi), None) => self.place(kind(hi, None)),
            (None, Some(lo)) => {
                *negate = true;
                self.place(kind(lo, None))
            }
            (Some(hi), Some(lo)) => {
                if self.data.params.contains(&param_kind(lo, Some(hi))) {
                    *negate = true;
                    self.place(kind(lo, Some(hi)))
                } else {
                    self.data.params.ensure(param_kind(hi, Some(lo)));
                    self.place(kind(hi, Some(lo)))
                }
            }
            (None, None) => unreachable!(),
        }
    }

    fn lower_bin_op(&mut self, expr: ExprId, lhs: ExprId, rhs: ExprId, op: BinaryOp) -> Local {
        let signature = self.infere.resolved_signatures.get(&expr);
        let op = match op {
            BinaryOp::BooleanOr => {
                // lhs || rhs if lhs { true } else { rhs }
                return self.lower_select(expr, lhs, |_| true.into(), |s| s.lower_expr(rhs));
            }

            BinaryOp::BooleanAnd => {
                // lhs && rhs if lhs { rhs } else { false }
                return self.lower_select(expr, lhs, |s| s.lower_expr(rhs), |_| false.into());
            }

            BinaryOp::EqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Op::BoolEq,
                    INT_EQ => Op::IntEq,
                    REAL_EQ => Op::RealEq,
                    STR_EQ => Op::StringEq
            },
            BinaryOp::NegatedEqualityTest => match_signature! {
                signature:
                    BOOL_EQ => Op::BoolNeq,
                    INT_EQ => Op::IntNeq,
                    REAL_EQ => Op::RealNeq,
                    STR_EQ => Op::StringNeq
            },
            BinaryOp::GreaterEqualTest => {
                match_signature!(signature: INT_OP => Op::IntGreaterEqual, REAL_OP => Op::RealGreaterEqual)
            }
            BinaryOp::GreaterTest => {
                match_signature!(signature: INT_OP => Op::IntGreaterThen, REAL_OP => Op::RealGreaterThen)
            }
            BinaryOp::LesserEqualTest => {
                match_signature!(signature: INT_OP => Op::IntLessEqual, REAL_OP => Op::RealLessEqual)
            }
            BinaryOp::LesserTest => {
                match_signature!(signature: INT_OP => Op::IntLessThen, REAL_OP => Op::RealLessThen)
            }
            BinaryOp::Addition => {
                match_signature!(signature: INT_OP => Op::IntAdd, REAL_OP => Op::RealAdd)
            }
            BinaryOp::Subtraction => {
                match_signature!(signature: INT_OP => Op::IntSub, REAL_OP => Op::RealSub)
            }
            BinaryOp::Multiplication => {
                match_signature!(signature: INT_OP => Op::IntMul, REAL_OP => Op::RealMul)
            }
            BinaryOp::Division => {
                match_signature!(signature: INT_OP => Op::IntDiv, REAL_OP => Op::RealDiv)
            }
            BinaryOp::Remainder => {
                match_signature!(signature: INT_OP => Op::IntRem, REAL_OP => Op::RealRem)
            }
            BinaryOp::Power => {
                match_signature!(signature: REAL_OP => Op::RealPow, INT_OP => Op::IntPow)
            }

            BinaryOp::LeftShift => Op::IntShl,
            BinaryOp::ArithmaticLeftShift => Op::IntAShl,

            BinaryOp::RightShift => Op::IntShr,
            BinaryOp::ArithmaticRightShift => Op::IntAShr,

            BinaryOp::BitwiseXor => Op::IntXor,
            BinaryOp::BitwiseEq => Op::IntNXor,
            BinaryOp::BitwiseOr => Op::IntOr,
            BinaryOp::BitwiseAnd => Op::IntAdd,
        };

        let lhs = self.lower_expr(lhs);
        let rhs = self.lower_expr(rhs);
        self.data.cfg.build_val(op, vec![lhs, rhs], expr.into())
    }

    fn lower_to_local(&mut self, expr: ExprId, f: impl FnOnce(&mut Self) -> Operand) -> Local {
        let val = f(self);
        self.data.cfg.to_local(val, expr.into())
    }

    fn lower_select(
        &mut self,
        expr: ExprId,
        cond: ExprId,
        lower_then_val: impl FnOnce(&mut Self) -> Operand,
        lower_else_val: impl FnOnce(&mut Self) -> Operand,
    ) -> Local {
        let (then_src, else_src) = self.lower_cond(
            cond,
            |s| s.lower_to_local(expr, lower_then_val),
            |s| s.lower_to_local(expr, lower_else_val),
        );

        self.data.cfg.build_phi([then_src, else_src])
    }

    fn lower_user_fun(&mut self, expr: ExprId, fun: FunctionId, args: &[ExprId]) -> Operand {
        let info = self.data.db.function_data(fun);
        let return_var = self.place(PlaceKind::FunctionReturn { fun });

        let args = args.iter().map(|arg| self.lower_expr(*arg)).collect();
        let body = self.data.db.body(fun.into());
        let infere = self.data.db.inference_result(fun.into());

        let init = match &info.return_ty {
            Type::Real => 0.0.into(),
            Type::Integer => 0.into(),
            ty => unreachable!("invalid function return type {:?}", ty),
        };
        self.data.cfg.build_assign(return_var.into(), Op::Copy, vec![init], expr.into());

        let mut ctx = LoweringCtx { data: self.data, body: &body, infere: &infere, args };

        ctx.lower_entry_stmts();

        Operand::Place(return_var)
    }

    fn lower_builtin(&mut self, expr: ExprId, builtin: BuiltIn, args: &[ExprId]) -> Operand {
        let signature = self.infere.resolved_signatures.get(&expr);
        let op = match builtin {
            BuiltIn::abs => {
                match_signature!(signature: ABS_REAL => Op::RealAbs, ABS_INT => Op::IntAbs)
            }
            BuiltIn::acos => Op::ArcCos,
            BuiltIn::acosh => Op::ArcCosH,
            BuiltIn::asin => Op::ArcSin,
            BuiltIn::asinh => Op::ArcSinH,
            BuiltIn::atan => Op::ArcTan,
            BuiltIn::atan2 => Op::ArcTan2,
            BuiltIn::atanh => Op::ArcTanH,
            BuiltIn::cos => Op::Cos,
            BuiltIn::cosh => Op::CosH,
            BuiltIn::exp => Op::Exp,
            BuiltIn::floor => Op::Floor,
            BuiltIn::hypot => Op::Hypot,
            BuiltIn::ln => Op::Ln,
            BuiltIn::log => Op::Log,

            BuiltIn::sin => Op::Sin,
            BuiltIn::sinh => Op::SinH,
            BuiltIn::sqrt => Op::Sqrt,
            BuiltIn::tan => Op::Tan,
            BuiltIn::tanh => Op::TanH,
            BuiltIn::clog2 => Op::Clog2,
            BuiltIn::log10 => Op::Log,
            BuiltIn::ceil => Op::Ceil,

            BuiltIn::max => {
                match_signature!(signature: MAX_REAL => Op::RealMax, MAX_INT => Op::RealMax)
            }
            BuiltIn::min => {
                match_signature!(signature: MAX_REAL => Op::RealMin, MAX_INT => Op::RealMin)
            }

            // TODO check standard
            BuiltIn::pow => Op::RealPow,

            // TODO implement properly
            BuiltIn::display => return Operand::Const(Const::Zst),
            BuiltIn::strobe => return Operand::Const(Const::Zst),
            BuiltIn::write => return Operand::Const(Const::Zst),
            BuiltIn::monitor => return Operand::Const(Const::Zst),
            BuiltIn::debug => return Operand::Const(Const::Zst),
            BuiltIn::stop => return Operand::Const(Const::Zst),
            BuiltIn::fatal => return Operand::Const(Const::Zst),
            BuiltIn::warning => return Operand::Const(Const::Zst),
            BuiltIn::error => return Operand::Const(Const::Zst),
            BuiltIn::info => return Operand::Const(Const::Zst),
            BuiltIn::finish => return Operand::Const(Const::Zst),
            BuiltIn::analysis => return false.into(),
            BuiltIn::ac_stim => return 0.0.into(),
            BuiltIn::noise_table => return 0.0.into(),
            BuiltIn::noise_table_log => return 0.0.into(),
            BuiltIn::white_noise => return 0.0.into(),
            BuiltIn::flicker_noise => return 0.0.into(),
            BuiltIn::ddt => return 0.0.into(),
            BuiltIn::limexp => Op::Exp,
            BuiltIn::idt | BuiltIn::idtmod => {
                let dc_val = match *signature.unwrap() {
                    IDT_NO_IC => 0.0.into(),
                    _ => self.lower_expr(args[1]),
                };
                return dc_val;
            }
            BuiltIn::flow => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(expr,args, |hi,lo|ParamKind::ImplictCurrent{hi,lo}),
                        NATURE_ACCESS_BRANCH => self.param(ParamKind::Current{branch: self.infere.expr_types[args[0]].unwrap_branch()}).into(),
                        NATURE_ACCESS_PORT_FLOW => self.param(ParamKind::PortCurrent{ port: self.infere.expr_types[args[0]].unwrap_port_flow() }).into()
                };
                return res;
            }
            BuiltIn::potential => {
                let res = match_signature! {
                    signature:
                        NATURE_ACCESS_NODES|NATURE_ACCESS_NODE_GND => self.nodes_from_args(expr, args, |hi,lo|ParamKind::Voltage{hi,lo}),
                        NATURE_ACCESS_BRANCH => {
                            let branch = self.infere.expr_types[args[0]].unwrap_branch();
                            let info =self.data.db.branch_info(branch).unwrap().kind;
                            self.nodes(expr, info.unwrap_hi_node(), info.lo_node(), |hi,lo|ParamKind::Voltage{hi,lo})
                        }
                };
                return res;
            }
            BuiltIn::vt => {
                // TODO make this a database input
                const KB: f64 = 1.3806488e-23;
                const Q: f64 = 1.602176565e-19;
                let temp = match args.get(0) {
                    Some(temp) => self.lower_expr(*temp),
                    None => self.param(ParamKind::Temperature).into(),
                };

                return self
                    .data
                    .cfg
                    .build_val(Op::RealMul, vec![(KB / Q).into(), temp], expr.into())
                    .into();
            }

            BuiltIn::ddx => {
                let val = self.lower_expr(args[0]);
                let cfg_param = self.lower_expr(args[1]).unwrap_cfg_param();
                let unkown = if *signature.unwrap() == DDX_POT {
                    // TODO how to handle gnd nodes?
                    let node = self.data.params[cfg_param].unwrap_pot_node();
                    Unkown::NodePot(node)
                } else {
                    // TODO how to handle gnd nodes?
                    Unkown::Simple(cfg_param)
                };
                let dst = self.data.cfg.cfg.new_local();
                self.data.derivatives.insert(dst, (val, unkown));
                return dst.into();
            }
            BuiltIn::temperature => return self.param(ParamKind::Temperature).into(),
            BuiltIn::simparam => {
                let call = match_signature!(signature: SIMPARAM_NO_DEFAULT => callbacks::SIM_PARAM,SIMPARAM_DEFAULT => callbacks::SIM_PARAM_OPT);
                Op::Call(call)
            }
            BuiltIn::simparam_str => Op::Call(callbacks::SIM_PARAM_STR),
            BuiltIn::param_given => {
                return self
                    .param(ParamKind::ParamGiven {
                        param: self.infere.expr_types[args[0]].unwrap_param(),
                    })
                    .into()
            }
            BuiltIn::port_connected => {
                return self
                    .param(ParamKind::PortConnected {
                        port: self.infere.expr_types[args[0]].unwrap_node(),
                    })
                    .into()
            }

            builtin => todo!("{:?}", builtin),
        };

        let operands = args.iter().map(|arg| self.lower_expr(*arg)).collect();
        self.data.cfg.build_val(op, operands, expr.into()).into()
    }
}
