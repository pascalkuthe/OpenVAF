use crate::cfg::{
    BasicBlock, BasicBlockData, ControlFlowGraph, PhiData, Terminator, TerminatorKind,
};
use crate::{
    BranchId, COperand, CallType, DisciplineAccess, Expression, Local, LocalDeclaration, LocalKind,
    Mir, OperandData, RValue, Spanned, StatementId, StmntKind, SyntaxCtx, TyRValue, Type,
    VariableId, VariableLocalKind,
};
use openvaf_data_structures::index_vec::{index_vec, IndexVec};
use openvaf_data_structures::HashMap;
use openvaf_ir::convert::Convert;
use openvaf_session::sourcemap::Span;
use std::array;
use tracing::trace;

pub trait CfgEdit {
    type CallType: CallType;
    fn borrow_mut(&mut self) -> &mut ControlFlowGraph<Self::CallType>;
}

impl<'a, C: CallType> CfgEdit for &'a mut ControlFlowGraph<C> {
    type CallType = C;

    fn borrow_mut(&mut self) -> &mut ControlFlowGraph<Self::CallType> {
        self
    }
}

impl<C: CallType> CfgEdit for ControlFlowGraph<C> {
    type CallType = C;

    fn borrow_mut(&mut self) -> &mut ControlFlowGraph<Self::CallType> {
        self
    }
}

pub struct CfgBuilder<C> {
    pub cfg: C,
    vars: IndexVec<VariableId, Option<Local>>,
    flows: IndexVec<BranchId, Option<Local>>,
    potentials: IndexVec<BranchId, Option<Local>>,
    pub current: BasicBlock,
}

impl<C> CfgBuilder<C> {
    pub fn finish(self) -> C {
        self.cfg
    }
}

impl<C: CallType> CfgBuilder<ControlFlowGraph<C>> {
    pub fn new_small() -> Self {
        let mut res = Self {
            cfg: ControlFlowGraph::new(),
            vars: IndexVec::new(),
            flows: IndexVec::new(),
            potentials: IndexVec::new(),
            current: BasicBlock::from_raw_unchecked(0),
        };
        res.enter_new_block();
        res
    }

    pub fn new_fn(variable_cnt: usize, root_sctx: SyntaxCtx) -> Self {
        let mut res = Self {
            cfg: ControlFlowGraph::new(),
            vars: index_vec![None; variable_cnt],
            flows: IndexVec::new(),
            potentials: IndexVec::new(),
            current: BasicBlock::from_raw_unchecked(0),
        };
        // CFGS should have an empty entry node to easily prepending basic blocks
        let entry = res.create_block();
        res.enter_new_block();
        res.terminate_bb(entry, TerminatorKind::Goto(res.current), root_sctx);
        res
    }

    pub fn new_main(variable_cnt: usize, branch_cnt: usize, root_sctx: SyntaxCtx) -> Self {
        let mut res = Self {
            cfg: ControlFlowGraph::new(),
            vars: index_vec![None; variable_cnt],
            flows: index_vec![None; branch_cnt],
            potentials: index_vec![None; branch_cnt],
            current: BasicBlock::from_raw_unchecked(0),
        };
        // CFGS should have an empty entry node to easily prepending basic blocks
        let entry = res.create_block();
        res.enter_new_block();
        res.terminate_bb(entry, TerminatorKind::Goto(res.current), root_sctx);
        res
    }
}

impl<'a, C: CallType> CfgBuilder<&'a mut ControlFlowGraph<C>> {
    pub fn edit<const INIT_BRANCHES: bool, const INIT_VARS: bool>(
        cfg: &'a mut ControlFlowGraph<C>,
        current_bb: BasicBlock,
        var_cnt: usize,
        branch_cnt: usize,
    ) -> Self {
        let mut vars = if INIT_VARS {
            index_vec![None; var_cnt]
        } else {
            IndexVec::new()
        };
        let (mut flows, mut potentials) = if INIT_BRANCHES {
            (index_vec![None; branch_cnt], index_vec![None; branch_cnt])
        } else {
            (IndexVec::new(), IndexVec::new())
        };

        if INIT_BRANCHES | INIT_VARS {
            for (local, decl) in cfg.locals.iter_enumerated() {
                match decl.kind {
                    LocalKind::Variable(var, VariableLocalKind::User) if INIT_VARS => {
                        vars[var] = Some(local)
                    }
                    LocalKind::Branch(DisciplineAccess::Potential, branch) if INIT_BRANCHES => {
                        potentials[branch] = Some(local)
                    }
                    LocalKind::Branch(DisciplineAccess::Flow, branch) if INIT_BRANCHES => {
                        flows[branch] = Some(local)
                    }
                    _ => (),
                }
            }
        }

        Self {
            cfg,
            vars: Default::default(),
            flows: Default::default(),
            potentials: Default::default(),
            current: current_bb,
        }
    }
}

impl<C: CallType, CFG: CfgEdit<CallType = C>> CfgBuilder<CFG> {
    /// # Note
    /// This funciton is optimized for joining multiple cfgs late in the compilation process.
    /// It therefore doesn't join derivative mappings If you need that do it manually (simply join the hashmaps with the second cfgs locals offsetet).
    /// Furthermore it doesn't invalidate the predecessor cache you need to do that after you are done with modifying the cfg
    pub fn insert_cfg<X>(&mut self, mut cfg: ControlFlowGraph<X>) -> BasicBlock
    where
        X: CallType + Into<C>,
        X::I: Into<C::I>,
    {
        let dst = self.cfg.borrow_mut();
        // update blocks to be inserted
        let block_offset = dst.blocks.last_idx();
        let local_offset = dst.locals.len();
        for block in cfg.blocks.iter_mut() {
            for succ in block.successors_mut() {
                *succ += block_offset
            }
            block.for_locals_mut(|local| *local = *local + local_offset)
        }

        // move the end
        let new_end = dst.end() + cfg.blocks.len();
        let old_end = dst.end();
        for block in dst.blocks.iter_mut() {
            if let Some(terminator) = &mut block.terminator {
                for block in terminator.successors_mut() {
                    if *block == old_end {
                        *block = new_end
                    }
                }
            }
        }

        // insert the data
        let inserted_cfg_start = dst.end();

        dst.locals.append(&mut cfg.locals);
        dst.blocks.splice(
            dst.end()..dst.end(),
            cfg.blocks.into_iter().map(|block| block.convert()),
        );
        dst[new_end - 1].terminator = None;
        self.current = new_end - 1;
        inserted_cfg_start
    }

    /// See `insert_cfg`
    pub fn insert_expr<X>(&mut self, sctx: SyntaxCtx, expr: Expression<X>) -> RValue<C>
    where
        X: CallType + Into<C>,
        X::I: Into<C::I>,
    {
        let cfg = expr.0;
        if !cfg.blocks.is_empty() {
            let pred = self.current;
            let start = self.insert_cfg(cfg);
            if pred != start {
                self.terminate_bb(pred, TerminatorKind::Goto(start), sctx)
            }
        }
        RValue::Use(expr.1.convert())
    }

    pub fn new_temporary(&mut self, ty: Type) -> Local {
        self.cfg.borrow_mut().new_temporary(ty)
    }

    pub fn variable_local<A: CallType>(&mut self, var: VariableId, mir: &Mir<A>) -> (Local, Type) {
        if let Some(res) = self.vars[var] {
            (res, self.cfg.borrow_mut().locals[res].ty)
        } else {
            let ty = mir.variables[var].ty;
            let local = self.cfg.borrow_mut().locals.push(LocalDeclaration {
                kind: LocalKind::Variable(var, VariableLocalKind::User),
                ty,
            });
            self.vars[var] = Some(local);
            (local, ty)
        }
    }

    pub fn branch_local(&mut self, branch: BranchId, access: DisciplineAccess) -> Local {
        let local_store = match access {
            DisciplineAccess::Flow => &mut self.flows,
            DisciplineAccess::Potential => &mut self.potentials,
        };

        if let Some(local) = local_store[branch] {
            local
        } else {
            let local = self.cfg.borrow_mut().locals.push(LocalDeclaration {
                kind: LocalKind::Branch(access, branch),
                ty: Type::REAL,
            });

            local_store[branch] = Some(local);
            local
        }
    }

    pub fn rvalue_to_operand(
        &mut self,
        data: TyRValue<C>,
        span: Span,
        sctx: SyntaxCtx,
    ) -> COperand<C> {
        self.rvalue_to_operand_in_bb(self.current, data, span, sctx)
    }

    pub fn rvalue_to_operand_in_bb(
        &mut self,
        bb: BasicBlock,
        data: TyRValue<C>,
        span: Span,
        sctx: SyntaxCtx,
    ) -> COperand<C> {
        if let RValue::Use(op) = data.val {
            op
        } else {
            Spanned::new(
                OperandData::Copy(self.assign_temporary_in_bb(bb, data, sctx)),
                span,
            )
        }
    }

    pub fn assign_temporary_phi_static_srces<const N: usize>(
        &mut self,
        sources: [(BasicBlock, Local); N],
        sctx: SyntaxCtx,
        ty: Type,
    ) -> Local {
        self.assign_temporary_phi_static_srces_in_bb(self.current, sources, sctx, ty)
    }

    pub fn assign_temporary_phi_static_srces_in_bb<const N: usize>(
        &mut self,
        bb: BasicBlock,
        sources: [(BasicBlock, Local); N],
        sctx: SyntaxCtx,
        ty: Type,
    ) -> Local {
        self.assign_temporary_phi_in_bb(bb, array::IntoIter::new(sources).collect(), sctx, ty)
    }

    pub fn assign_temporary_phi(
        &mut self,
        sources: HashMap<BasicBlock, Local>,
        sctx: SyntaxCtx,
        ty: Type,
    ) -> Local {
        self.assign_temporary_phi_in_bb(self.current, sources, sctx, ty)
    }

    pub fn assign_temporary_phi_in_bb(
        &mut self,
        bb: BasicBlock,
        sources: HashMap<BasicBlock, Local>,
        sctx: SyntaxCtx,
        ty: Type,
    ) -> Local {
        let dst = self.new_temporary(ty);
        self.assign_phi_in_bb(dst, bb, sources, sctx)
    }

    pub fn assign_phi_static_srces<const N: usize>(
        &mut self,
        dst: Local,
        sources: [(BasicBlock, Local); N],
        sctx: SyntaxCtx,
    ) -> Local {
        self.assign_phi_static_srces_in_bb(dst, self.current, sources, sctx)
    }

    pub fn assign_phi_static_srces_in_bb<const N: usize>(
        &mut self,
        dst: Local,
        bb: BasicBlock,
        sources: [(BasicBlock, Local); N],
        sctx: SyntaxCtx,
    ) -> Local {
        self.assign_phi_in_bb(dst, bb, array::IntoIter::new(sources).collect(), sctx)
    }

    pub fn assign_phi(
        &mut self,
        dst: Local,
        sources: HashMap<BasicBlock, Local>,
        sctx: SyntaxCtx,
    ) -> Local {
        self.assign_phi_in_bb(dst, self.current, sources, sctx)
    }

    pub fn assign_phi_in_bb(
        &mut self,
        dst: Local,
        bb: BasicBlock,
        sources: HashMap<BasicBlock, Local>,
        sctx: SyntaxCtx,
    ) -> Local {
        self.cfg.borrow_mut().blocks[bb]
            .phi_statements
            .push(PhiData { dst, sources, sctx });
        dst
    }

    pub fn assign_temporary(&mut self, data: TyRValue<C>, sctx: SyntaxCtx) -> Local {
        self.assign_temporary_in_bb(self.current, data, sctx)
    }

    pub fn assign_temporary_in_bb(
        &mut self,
        bb: BasicBlock,
        data: TyRValue<C>,
        sctx: SyntaxCtx,
    ) -> Local {
        let lhs = self.new_temporary(data.ty);
        self.assign_in_bb(bb, lhs, data.val, sctx);
        lhs
    }

    pub fn assign(&mut self, lhs: Local, rhs: RValue<C>, sctx: SyntaxCtx) -> StatementId {
        self.cfg.borrow_mut().blocks[self.current]
            .statements
            .push((StmntKind::Assignment(lhs, rhs), sctx))
    }

    pub fn assign_in_bb(
        &mut self,
        bb: BasicBlock,
        lhs: Local,
        rhs: RValue<C>,
        sctx: SyntaxCtx,
    ) -> StatementId {
        self.cfg.borrow_mut().blocks[bb]
            .statements
            .push((StmntKind::Assignment(lhs, rhs), sctx))
    }

    pub fn terminate_bb(&mut self, block: BasicBlock, kind: TerminatorKind<C>, sctx: SyntaxCtx) {
        trace!(block = block.index(), kind = debug(&kind), "terminating");
        debug_assert!(
            self.cfg.borrow_mut()[block].terminator.is_none(),
            "terminate: block {:?}={:?} already has a terminator set",
            block,
            self.cfg.borrow_mut()[block]
        );

        self.cfg.borrow_mut()[block].terminator = Some(Terminator { sctx, kind });
    }

    pub fn terminate(&mut self, kind: TerminatorKind<C>, sctx: SyntaxCtx) {
        self.terminate_bb(self.current, kind, sctx)
    }

    pub fn create_block(&mut self) -> BasicBlock {
        self.cfg.borrow_mut().blocks.push(BasicBlockData {
            phi_statements: IndexVec::with_capacity(16),
            statements: IndexVec::with_capacity(128),
            terminator: None,
        })
    }

    pub fn enter_new_block(&mut self) {
        self.current = self.cfg.borrow_mut().blocks.push(BasicBlockData {
            phi_statements: IndexVec::with_capacity(16),
            statements: IndexVec::with_capacity(128),
            terminator: None,
        })
    }
}
