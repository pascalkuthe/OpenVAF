/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

pub use inkwell;
use inkwell::basic_block::BasicBlock as LlvmBasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use openvaf_data_structures::index_vec::{index_vec, IndexSlice, IndexVec};
use openvaf_ir::ids::BranchId;
use openvaf_middle::cfg::{BasicBlock, ControlFlowGraph, PhiData, TerminatorKind};
use openvaf_middle::{
    fold_rvalue, COperand, CallArg, CallType, ComparisonOp, DisciplineAccess, Local, LocalKind,
    Mir, OperandData, RValue, StmntKind, Type, VariableLocalKind,
};
use openvaf_session::sourcemap::StringLiteral;
use std::ffi::CString;

mod intrinsics;
mod rvalue;
mod types;
pub use types::ArrayCreation;

use inkwell::targets::TargetData;
pub use intrinsics::{intrinsic_prototypes, Intrinsic, IntrinsicPrototypes};
use tracing::{trace, trace_span};

pub enum LocalValue<'ctx> {
    Value(BasicValueEnum<'ctx>),
    Ptr(PointerValue<'ctx>),
    Undef,
}

pub struct LlvmCodegen<'a, 'c, A: CallType> {
    pub mir: &'a Mir<A>,
    pub context: &'c Context,
    pub module: Module<'c>,
    pub intrinsics: IntrinsicPrototypes<'c>,
    pub builder: Builder<'c>,
    string_literals: IndexVec<StringLiteral, Option<PointerValue<'c>>>,
    pub target: &'a TargetData,
}

impl<'a, 'c, A: CallType> LlvmCodegen<'a, 'c, A> {
    pub fn new(
        mir: &'a Mir<A>,
        context: &'c Context,
        msvc: bool,
        target: &'a TargetData,
        name: &str,
    ) -> Self {
        let module = context.create_module(name);
        module.set_data_layout(&target.get_data_layout());
        Self {
            mir,
            context,
            intrinsics: intrinsic_prototypes(&module, context, msvc),
            module,
            builder: context.create_builder(),
            string_literals: index_vec![None;StringLiteral::global_count()],
            target,
        }
    }

    pub fn str_literal(&mut self, literal: StringLiteral) -> PointerValue<'c> {
        if let Some(val) = self.string_literals[literal] {
            val
        } else {
            let val = literal.unescaped_contents();
            let name = format!("StringLiteral{} = \"{}\"", literal, val);
            let val = CString::new(val).unwrap();
            let char_ty = self.context.i8_type();
            let data: Vec<_> = val
                .as_bytes_with_nul()
                .iter()
                .map(|&b| char_ty.const_int(b as u64, false))
                .collect();
            let data = char_ty.const_array(&data);
            let location =
                self.module
                    .add_global(data.get_type(), Some(AddressSpace::Const), &name);
            location.set_initializer(&data);
            let ptr = location.as_pointer_value();
            let zero = self.context.i32_type().const_zero();
            // Not entirely sure why we need two zeros but llvm does that same in the builtin createglobalstringptr function
            let char_ptr = unsafe { ptr.const_gep(&[zero, zero]) };
            self.string_literals[literal] = Some(char_ptr);
            char_ptr
        }
    }

    pub fn cfg_codegen<'lt, C: CallTypeCodeGen<'lt, 'c>>(
        &'lt mut self,
        cfg: &'lt ControlFlowGraph<C>,
        function: FunctionValue<'c>,
    ) -> CfgCodegen<'lt, 'a, 'c, (), A, C> {
        CfgCodegen::new(self, cfg, function)
    }

    pub fn cfg_codegen_with<'lt, C: CallTypeCodeGen<'lt, 'c>, D>(
        &'lt mut self,
        cfg: &'lt ControlFlowGraph<C>,
        function: FunctionValue<'c>,
        call_type_data: D,
    ) -> CfgCodegen<'lt, 'a, 'c, D, A, C> {
        CfgCodegen::new_with(self, cfg, function, call_type_data)
    }
}

pub struct CfgCodegen<'lt, 'a, 'c, D, A: CallType, C: CallTypeCodeGen<'lt, 'c>> {
    pub ctx: &'lt mut LlvmCodegen<'a, 'c, A>,
    pub call_type_data: D,
    pub blocks: IndexVec<BasicBlock, LlvmBasicBlock<'c>>,
    pub cfg: &'lt ControlFlowGraph<C>,
    pub local_values: IndexVec<Local, LocalValue<'c>>,
    pub function: FunctionValue<'c>,
}

impl<'lt, 'a, 'c, A, C> CfgCodegen<'lt, 'a, 'c, (), A, C>
where
    A: CallType,
    C: CallTypeCodeGen<'lt, 'c>,
{
    pub fn new(
        ctx: &'lt mut LlvmCodegen<'a, 'c, A>,
        cfg: &'lt ControlFlowGraph<C>,
        function: FunctionValue<'c>,
    ) -> Self {
        Self::new_with(ctx, cfg, function, ())
    }

    pub fn attach_call_type_data<D>(self, call_type_data: D) -> CfgCodegen<'lt, 'a, 'c, D, A, C> {
        CfgCodegen {
            ctx: self.ctx,
            call_type_data,
            blocks: self.blocks,
            cfg: self.cfg,
            local_values: self.local_values,
            function: self.function,
        }
    }
}

impl<'lt, 'a, 'c, D, A, C> CfgCodegen<'lt, 'a, 'c, D, A, C>
where
    A: CallType,
    C: CallTypeCodeGen<'lt, 'c>,
{
    pub fn new_with(
        ctx: &'lt mut LlvmCodegen<'a, 'c, A>,
        cfg: &'lt ControlFlowGraph<C>,
        function: FunctionValue<'c>,
        call_type_data: D,
    ) -> Self {
        let blocks = cfg
            .blocks
            .indices()
            .map(|block| {
                ctx.context
                    .append_basic_block(function, &format!("Block {}", block))
            })
            .collect();

        Self {
            ctx,
            blocks,
            cfg,
            local_values: IndexVec::new(),
            function,
            call_type_data,
        }
    }

    pub fn map_call_type_data<N>(
        self,
        call_type_data: impl FnOnce(D) -> N,
    ) -> CfgCodegen<'lt, 'a, 'c, N, A, C> {
        CfgCodegen {
            ctx: self.ctx,
            call_type_data: call_type_data(self.call_type_data),
            blocks: self.blocks,
            cfg: self.cfg,
            local_values: self.local_values,
            function: self.function,
        }
    }
}

impl<'lt, 'a, 'c, A: CallType, C: CallTypeCodeGen<'lt, 'c>>
    CfgCodegen<'lt, 'a, 'c, C::CodeGenData, A, C>
{
    pub fn alloc_vars_and_branches(
        &mut self,
        mut alloc_branch: impl FnMut(&mut Self, DisciplineAccess, BranchId) -> PointerValue<'c>,
    ) {
        let mir = self.ctx.mir;
        self.local_values = self
            .cfg
            .locals
            .iter()
            .map(|declaration| match declaration.kind {
                LocalKind::Variable(id, VariableLocalKind::User) => {
                    let name = format!("var {}", mir.variables[id].ident);

                    LocalValue::Ptr(
                        self.ctx
                            .builder
                            .build_alloca(self.ctx.ty(self.ctx.mir[id].variable_type), &name),
                    )
                }

                LocalKind::Variable(id, VariableLocalKind::Derivative) => {
                    let name = format!("variablederivative {}", mir.variables[id].ident);
                    LocalValue::Ptr(
                        self.ctx
                            .builder
                            .build_alloca(self.ctx.context.f64_type(), &name),
                    )
                }

                LocalKind::Branch(access, branch) => {
                    LocalValue::Ptr(alloc_branch(self, access, branch))
                }
                LocalKind::Temporary => LocalValue::Undef,
            })
            .collect()
    }

    pub fn read_local(&mut self, local: Local) -> BasicValueEnum<'c> {
        trace!(local = local.index(), "Build local read");
        match self.local_values[local] {
            LocalValue::Ptr(ptr) => self.ctx.builder.build_load(ptr, "local read"),
            LocalValue::Value(val) => val,
            LocalValue::Undef => unreachable!("Value read before definition"),
        }
    }

    pub fn write_local(&mut self, local: Local, val: BasicValueEnum<'c>) {
        match self.local_values[local] {
            LocalValue::Ptr(ptr) => {
                let instruction = self.ctx.builder.build_store(ptr, val);
                debug_assert!(instruction.get_parent().is_some())
            }
            LocalValue::Value(old) => unreachable!(
                "Local {} was written twice (first: {:?} then {:?})",
                local, old, val
            ),
            LocalValue::Undef => self.local_values[local] = LocalValue::Value(val),
        }
    }

    pub fn operand(&mut self, op: &COperand<C>) -> BasicValueEnum<'c> {
        match op.contents {
            OperandData::Copy(local) => self.read_local(local),
            OperandData::Read(ref input) => C::read_input(self, input),
            OperandData::Constant(ref val) => self.ctx.constant(val),
        }
    }

    pub fn real_comparison(
        &mut self,
        op: ComparisonOp,
        lhs: BasicValueEnum<'c>,
        rhs: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        let pred = match op {
            ComparisonOp::LessThen => FloatPredicate::OLE,
            ComparisonOp::LessEqual => FloatPredicate::OLE,
            ComparisonOp::GreaterThen => FloatPredicate::OGT,
            ComparisonOp::GreaterEqual => FloatPredicate::OGE,
            ComparisonOp::Equal => FloatPredicate::OEQ,
            ComparisonOp::NotEqual => FloatPredicate::ONE,
        };

        self.ctx
            .builder
            .build_float_compare(
                pred,
                lhs.into_float_value(),
                rhs.into_float_value(),
                "float_cmp",
            )
            .as_basic_value_enum()
    }

    pub fn int_comparison(
        &mut self,
        op: ComparisonOp,
        lhs: BasicValueEnum<'c>,
        rhs: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        let pred = match op {
            ComparisonOp::LessThen => IntPredicate::SLT,
            ComparisonOp::LessEqual => IntPredicate::SLE,
            ComparisonOp::GreaterThen => IntPredicate::SGT,
            ComparisonOp::GreaterEqual => IntPredicate::SGE,
            ComparisonOp::Equal => IntPredicate::EQ,
            ComparisonOp::NotEqual => IntPredicate::NE,
        };

        self.ctx
            .builder
            .build_int_compare(pred, lhs.into_int_value(), rhs.into_int_value(), "int_cmp")
            .as_basic_value_enum()
    }

    pub fn string_comparison(
        &mut self,
        invert: bool,
        lhs: BasicValueEnum<'c>,
        rhs: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c> {
        let res = self
            .ctx
            .build_intrinsic_call(Intrinsic::StringCmp, &[lhs, rhs])
            .into_int_value();
        let predicate = if invert {
            IntPredicate::EQ
        } else {
            IntPredicate::NE
        };

        self.ctx
            .builder
            .build_int_compare(
                predicate,
                res,
                res.get_type().const_int(0, false),
                "string_neq",
            )
            .as_basic_value_enum()
    }

    pub fn rvalue(&mut self, val: &RValue<C>, dst_ty: Type) -> BasicValueEnum<'c> {
        fold_rvalue(self, val, dst_ty)
    }

    pub fn build_blocks(&mut self) {
        for bb in self.cfg.reverse_postorder() {
            let span = trace_span!("block codegen", bb_idx = bb.index());
            let _enter = span.enter();
            self.build_bb(bb)
        }
    }

    pub fn build_bb(&mut self, bb: BasicBlock) {
        self.ctx.builder.position_at_end(self.blocks[bb]);
        let bb = &self.cfg[bb];

        for phi in &bb.phi_statements {
            self.build_phi(phi);
        }

        for (stmnt, _) in &bb.statements {
            self.build_stmnt(stmnt)
        }

        match bb.terminator().kind {
            TerminatorKind::Goto(dst) => {
                self.ctx
                    .builder
                    .build_unconditional_branch(self.blocks[dst]);
            }
            TerminatorKind::Split {
                ref condition,
                true_block,
                false_block,
                ..
            } => {
                let condition = self.rvalue(condition, Type::BOOL);
                self.ctx.builder.build_conditional_branch(
                    condition.into_int_value(),
                    self.blocks[true_block],
                    self.blocks[false_block],
                );
            }

            TerminatorKind::End => {}
        }
    }

    pub fn build_phi(&mut self, phi: &PhiData) {
        let res = self.ctx.builder.build_phi(self.local_ty(phi.dst), "phi");
        let incoming: Vec<_> = phi
            .sources
            .iter()
            .map(|(block, local)| (self.read_local(*local), self.blocks[*block]))
            .collect();
        let incoming: Vec<_> = incoming
            .iter()
            .map(|(val, block)| (val as &dyn BasicValue<'c>, *block))
            .collect();

        res.add_incoming(incoming.as_slice());

        self.write_local(phi.dst, res.as_basic_value())
    }

    pub fn build_stmnt(&mut self, stmnt: &StmntKind<C>) {
        match stmnt {
            StmntKind::Assignment(dst, rval) => {
                let ty = self.cfg.locals[*dst].ty;
                let rval = self.rvalue(rval, ty);
                self.write_local(*dst, rval)
            }
            StmntKind::Call(call, args, _) => {
                let args: IndexVec<CallArg, _> = args.iter().map(|op| self.operand(op)).collect();
                C::gen_call(call, self, args.as_slice())
            }
            StmntKind::NoOp => {}
        }
    }
}

pub trait CallTypeCodeGen<'lt, 'c>: CallType {
    type CodeGenData;

    fn read_input<'a, A: CallType>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, Self::CodeGenData, A, Self>,
        input: &Self::I,
    ) -> BasicValueEnum<'c>;

    fn gen_call_rvalue<'a, A: CallType>(
        &self,
        cg: &mut CfgCodegen<'lt, 'a, 'c, Self::CodeGenData, A, Self>,
        args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    ) -> BasicValueEnum<'c>;

    fn gen_call<'a, A: CallType>(
        &self,
        cg: &mut CfgCodegen<'lt, 'a, 'c, Self::CodeGenData, A, Self>,
        args: &IndexSlice<CallArg, [BasicValueEnum<'c>]>,
    );

    fn gen_limexp<'a, A: CallType>(
        cg: &mut CfgCodegen<'lt, 'a, 'c, Self::CodeGenData, A, Self>,
        args: BasicValueEnum<'c>,
    ) -> BasicValueEnum<'c>;
}
