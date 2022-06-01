use std::slice;

use arrayvec::ArrayVec;
use libc::c_uint;
use llvm::{
    LLVMBuildExtractValue, LLVMBuildICmp, LLVMBuildLoad2, LLVMBuildStore, LLVMGetReturnType,
    UNNAMED,
};
use mir::{Block, FuncRef, Function, Inst, Opcode, Param, PhiNode, Value, ValueDef, F_ZERO, ZERO};
use typed_index_collections::TiVec;

use crate::callbacks::CallbackFun;
use crate::CodegenCx;

#[derive(Clone)]
pub struct MemLoc<'ll> {
    pub ptr: &'ll llvm::Value,
    pub ptr_ty: &'ll llvm::Type,
    pub ty: &'ll llvm::Type,
    pub indicies: Box<[&'ll llvm::Value]>,
}

impl<'ll> MemLoc<'ll> {
    /// # Safety
    ///
    /// ptr_ty, ty and indicies must be valid for ptr
    pub unsafe fn read(&self, llbuilder: &llvm::Builder<'ll>) -> &'ll llvm::Value {
        let ptr = self.to_ptr(llbuilder);
        LLVMBuildLoad2(llbuilder, self.ty, ptr, UNNAMED)
    }

    /// # Safety
    ///
    /// ptr_ty and indicies must be valid for ptr
    pub unsafe fn to_ptr(&self, llbuilder: &llvm::Builder<'ll>) -> &'ll llvm::Value {
        let mut ptr = self.ptr;
        if !self.indicies.is_empty() {
            ptr = llvm::LLVMBuildGEP2(
                llbuilder,
                self.ptr_ty,
                ptr,
                self.indicies.as_ptr(),
                self.indicies.len() as u32,
                UNNAMED,
            );
        }
        ptr
    }
}

impl<'ll> From<MemLoc<'ll>> for BuilderVal<'ll> {
    fn from(loc: MemLoc<'ll>) -> Self {
        BuilderVal::Load(Box::new(loc))
    }
}

#[derive(Clone)]
pub enum BuilderVal<'ll> {
    Undef,
    Eager(&'ll llvm::Value),
    Load(Box<MemLoc<'ll>>),
    Call(Box<CallbackFun<'ll>>),
}

impl<'ll> From<&'ll llvm::Value> for BuilderVal<'ll> {
    fn from(val: &'ll llvm::Value) -> Self {
        BuilderVal::Eager(val)
    }
}

impl<'ll> BuilderVal<'ll> {
    /// # Safety
    ///
    /// For Self::Load and Self::Call, the values must be valid
    pub unsafe fn get(&self, builder: &Builder<'_, '_, 'll>) -> &'ll llvm::Value {
        match self {
            BuilderVal::Undef => unreachable!("attempted to read undefined value"),
            BuilderVal::Eager(val) => val,
            BuilderVal::Load(loc) => loc.read(builder.llbuilder),
            BuilderVal::Call(call) => builder.call(call.fun_ty, call.fun, &*call.state),
        }
    }

    /// # Safety
    ///
    /// For Self::Load and Self::Call, the values must be valid
    pub unsafe fn get_ty(&self, builder: &Builder<'_, '_, 'll>) -> Option<&'ll llvm::Type> {
        let ty = match self {
            BuilderVal::Undef => return None,
            BuilderVal::Eager(val) => builder.cx.val_ty(val),
            BuilderVal::Load(loc) => loc.ty,
            BuilderVal::Call(call) => LLVMGetReturnType(call.fun_ty),
        };
        Some(ty)
    }
}

// All Builders must have an llfn associated with them
#[must_use]
pub struct Builder<'a, 'cx, 'll> {
    pub llbuilder: &'a mut llvm::Builder<'ll>,
    pub cx: &'a mut CodegenCx<'cx, 'll>,
    pub func: &'a Function,
    pub blocks: TiVec<Block, Option<&'ll llvm::BasicBlock>>,
    pub values: TiVec<Value, BuilderVal<'ll>>,
    pub params: TiVec<Param, BuilderVal<'ll>>,
    pub callbacks: TiVec<FuncRef, Option<CallbackFun<'ll>>>,
    pub prepend_pos: &'ll llvm::BasicBlock,
    pub unfinished_phis: Vec<(PhiNode, &'ll llvm::Value)>,
    fun: &'ll llvm::Value,
}

impl Drop for Builder<'_, '_, '_> {
    fn drop(&mut self) {
        unsafe {
            llvm::LLVMDisposeBuilder(&mut *(self.llbuilder as *mut _));
        }
    }
}

pub enum FastMathMode {
    Full,
    Partial,
    Disabled,
}

impl<'a, 'cx, 'll> Builder<'a, 'cx, 'll> {
    pub fn new(
        cx: &'a mut CodegenCx<'cx, 'll>,
        mir_func: &'a Function,
        llfunc: &'ll llvm::Value,
    ) -> Self {
        let entry = unsafe { llvm::LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED) };
        let llbuilder = unsafe { llvm::LLVMCreateBuilderInContext(cx.llcx) };
        let mut blocks: TiVec<_, _> = vec![None; mir_func.layout.num_blocks()].into();
        for bb in mir_func.layout.blocks() {
            blocks[bb] =
                unsafe { Some(llvm::LLVMAppendBasicBlockInContext(cx.llcx, llfunc, UNNAMED)) };
        }
        unsafe { llvm::LLVMPositionBuilderAtEnd(llbuilder, entry) };

        Builder {
            llbuilder,
            cx,
            func: mir_func,
            blocks,
            values: vec![BuilderVal::Undef; mir_func.dfg.num_values()].into(),
            params: Default::default(),
            callbacks: Default::default(),
            fun: llfunc,
            prepend_pos: entry,
            unfinished_phis: Vec::new(),
        }
    }
}

impl<'ll> Builder<'_, '_, 'll> {
    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    /// Must be called in the entry block of the function
    pub unsafe fn alloca(&self, ty: &'ll llvm::Type) -> &'ll llvm::Value {
        llvm::LLVMBuildAlloca(self.llbuilder, ty, UNNAMED)
    }

    /// # Safety
    /// Only correct llvm api calls must be performed within build_then and build_else
    /// Their return types must match and cond must be a bool
    pub unsafe fn add_branching_select(
        &mut self,
        cond: &'ll llvm::Value,
        build_then: impl FnOnce(&mut Self) -> &'ll llvm::Value,
        build_else: impl FnOnce(&mut Self) -> &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        let start = self.prepend_pos;
        let exit = llvm::LLVMAppendBasicBlockInContext(self.cx.llcx, self.fun, UNNAMED);
        let then_bb = llvm::LLVMAppendBasicBlockInContext(self.cx.llcx, self.fun, UNNAMED);
        llvm::LLVMPositionBuilderAtEnd(self.llbuilder, then_bb);
        self.prepend_pos = then_bb;
        let then_val = build_then(self);
        llvm::LLVMBuildBr(self.llbuilder, exit);

        let else_bb = llvm::LLVMAppendBasicBlockInContext(self.cx.llcx, self.fun, UNNAMED);
        llvm::LLVMPositionBuilderAtEnd(self.llbuilder, else_bb);
        self.prepend_pos = else_bb;
        let else_val = build_else(self);
        llvm::LLVMBuildBr(self.llbuilder, exit);

        llvm::LLVMPositionBuilderAtEnd(self.llbuilder, start);
        llvm::LLVMBuildCondBr(self.llbuilder, cond, then_bb, else_bb);

        self.prepend_pos = exit;
        llvm::LLVMPositionBuilderAtEnd(self.llbuilder, self.prepend_pos);
        let phi = llvm::LLVMBuildPhi(self.llbuilder, llvm::LLVMTypeOf(then_val), UNNAMED);
        llvm::LLVMAddIncoming(phi, [then_val, else_val].as_ptr(), [then_bb, else_bb].as_ptr(), 2);
        phi
    }

    /// # Safety
    /// Only correct llvm api calls must be performed within build_then and build_else
    /// Their return types must match and cond must be a bool
    pub unsafe fn select(
        &self,
        cond: &'ll llvm::Value,
        then_val: &'ll llvm::Value,
        else_val: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildSelect(self.llbuilder, cond, then_val, else_val, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn typed_gep(
        &self,
        arr_ty: &'ll llvm::Type,
        ptr: &'ll llvm::Value,
        indicies: &[&'ll llvm::Value],
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildGEP2(
            self.llbuilder,
            arr_ty,
            ptr,
            indicies.as_ptr(),
            indicies.len() as u32,
            UNNAMED,
        )
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn gep(
        &self,
        ptr: &'ll llvm::Value,
        indicies: &[&'ll llvm::Value],
    ) -> &'ll llvm::Value {
        let arr_ty = llvm::LLVMGetElementType(llvm::LLVMTypeOf(ptr));
        self.typed_gep(arr_ty, ptr, indicies)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn ptrcast(&self, ptr: &'ll llvm::Value, ty: &'ll llvm::Type) -> &'ll llvm::Value {
        llvm::LLVMBuildPointerCast(self.llbuilder, ptr, ty, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn struct_gep(&self, ptr: &'ll llvm::Value, idx: u32) -> &'ll llvm::Value {
        let struct_ty = llvm::LLVMGetElementType(llvm::LLVMTypeOf(ptr));
        self.typed_struct_gep(struct_ty, ptr, idx)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn fat_ptr_get_ptr(&self, ptr: &'ll llvm::Value) -> &'ll llvm::Value {
        self.struct_gep(ptr, 0)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn fat_ptr_get_meta(&self, ptr: &'ll llvm::Value) -> &'ll llvm::Value {
        self.struct_gep(ptr, 1)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn fat_ptr_to_parts(
        &self,
        ptr: &'ll llvm::Value,
    ) -> (&'ll llvm::Value, &'ll llvm::Value) {
        (self.fat_ptr_get_ptr(ptr), self.fat_ptr_get_meta(ptr))
    }

    /// # Safety
    /// * Must not be called when a block that already contains a terminator is selected
    /// * struct_ty must be a valid struct type for this pointer and idx must be in bounds
    pub unsafe fn typed_struct_gep(
        &self,
        struct_ty: &'ll llvm::Type,
        ptr: &'ll llvm::Value,
        idx: u32,
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildStructGEP2(self.llbuilder, struct_ty, ptr, idx, UNNAMED)
    }

    /// # Safety
    /// * Must not be called when a block that already contains a terminator is selected
    pub unsafe fn call(
        &self,
        fun_ty: &'ll llvm::Type,
        fun: &'ll llvm::Value,
        operands: &[&'ll llvm::Value],
    ) -> &'ll llvm::Value {
        let res = llvm::LLVMBuildCall2(
            self.llbuilder,
            fun_ty,
            fun,
            operands.as_ptr(),
            operands.len() as u32,
            UNNAMED,
        );

        // forgett this is a real footgun
        let cconv = llvm::LLVMGetFunctionCallConv(fun);
        llvm::LLVMSetInstructionCallConv(res, cconv);
        res
    }

    pub fn build_consts(&mut self) {
        for val in self.func.dfg.values() {
            match self.func.dfg.value_def(val) {
                ValueDef::Result(_, _) | ValueDef::Invalid => (),
                ValueDef::Param(param) => self.values[val] = self.params[param].clone(),
                ValueDef::Const(const_val) => {
                    self.values[val] = self.cx.const_val(&const_val).into();
                }
            }
        }
    }

    /// # Safety
    ///
    /// Must not be called if any block already contain any non-phi instruction (eg must not be
    /// called twice)
    pub unsafe fn build_cfg(&mut self, blocks: &[Block]) {
        let entry = self.func.layout.entry_block().unwrap();
        llvm::LLVMBuildBr(self.llbuilder, self.blocks[entry].unwrap());
        for bb in blocks.iter().rev() {
            self.build_bb(*bb)
        }

        for (phi, llval) in self.unfinished_phis.iter() {
            let (blocks, vals): (Vec<_>, Vec<_>) = self
                .func
                .dfg
                .phi_edges(phi)
                .map(|(bb, val)| {
                    self.select_bb_before_terminator(bb);
                    (self.blocks[bb].unwrap(), self.values[val].get(self))
                })
                .unzip();

            llvm::LLVMAddIncoming(llval, vals.as_ptr(), blocks.as_ptr(), vals.len() as c_uint);
        }

        self.unfinished_phis.clear();
    }

    pub fn select_bb(&self, bb: Block) {
        unsafe {
            llvm::LLVMPositionBuilderAtEnd(self.llbuilder, self.blocks[bb].unwrap());
        }
    }

    pub fn select_bb_before_terminator(&self, bb: Block) {
        let bb = self.blocks[bb].unwrap();
        unsafe {
            let inst = llvm::LLVMGetLastInstruction(bb);
            llvm::LLVMPositionBuilder(self.llbuilder, bb, inst);
        };
    }

    /// # Safety
    ///
    /// Must not be called if any non phi instruction has already been build for `bb`
    /// The means it must not be called twice for the same bloc
    pub unsafe fn build_bb(&mut self, bb: Block) {
        self.select_bb(bb);

        for inst in self.func.layout.block_insts(bb) {
            let fast_math = self.func.srclocs.get(inst).map_or(false, |loc| loc.0 < 0);
            self.build_inst(
                inst,
                if fast_math { FastMathMode::Partial } else { FastMathMode::Disabled },
            )
        }
    }

    /// # Safety
    /// must not be called multiple times
    /// a terminator must not be build for the exit bb trough other means
    pub unsafe fn ret(&mut self, val: &'ll llvm::Value) {
        llvm::LLVMBuildRet(self.llbuilder, val);
    }

    /// # Safety
    /// must not be called multiple times
    /// a terminator must not be build for the exit bb trough other means
    pub unsafe fn ret_void(&mut self) {
        llvm::LLVMBuildRetVoid(self.llbuilder);
    }

    /// # Safety
    /// Must only be called when after the builder has been positioned
    /// Not Phis may be constructed for the current block after this function has been called
    /// Must not be called when the builder has selected a block that already contains a terminator
    pub unsafe fn build_inst(&mut self, inst: Inst, fast_math_mode: FastMathMode) {
        let (opcode, args) = match self.func.dfg.insts[inst] {
            mir::InstructionData::Unary { opcode, ref arg } => (opcode, slice::from_ref(arg)),
            mir::InstructionData::Binary { opcode, ref args } => (opcode, args.as_slice()),
            mir::InstructionData::Branch { cond, then_dst, else_dst, .. } => {
                llvm::LLVMBuildCondBr(
                    self.llbuilder,
                    self.values[cond].get(self),
                    self.blocks[then_dst].unwrap(),
                    self.blocks[else_dst].unwrap(),
                );
                return;
            }
            mir::InstructionData::PhiNode(ref phi) => {
                // TODO does this always produce a valid value?
                let ty = self
                    .func
                    .dfg
                    .phi_edges(phi)
                    .find_map(|(_, val)| self.values[val].get_ty(self))
                    .unwrap();
                let llval = llvm::LLVMBuildPhi(self.llbuilder, ty, UNNAMED);
                self.unfinished_phis.push((phi.clone(), llval));
                let res = self.func.dfg.first_result(inst);
                self.values[res] = llval.into();
                return;
            }
            mir::InstructionData::Jump { destination } => {
                llvm::LLVMBuildBr(self.llbuilder, self.blocks[destination].unwrap());
                return;
            }
            mir::InstructionData::Call { func_ref, ref args } => {
                let callback = if let Some(res) = self.callbacks[func_ref].as_ref() {
                    res
                } else {
                    return; // assume nooop
                };

                let args = args.as_slice(&self.func.dfg.insts.value_lists);

                let operands: Vec<_> = callback
                    .state
                    .iter()
                    .copied()
                    .chain(args.iter().map(|operand| self.values[*operand].get(self)))
                    .collect();
                let res = self.call(callback.fun_ty, callback.fun, &operands);
                let inst_res = self.func.dfg.inst_results(inst);
                match inst_res {
                    [] => (),
                    [val] => self.values[*val] = res.into(),
                    vals => {
                        for (i, val) in vals.iter().enumerate() {
                            let res = LLVMBuildExtractValue(self.llbuilder, res, i as u32, UNNAMED);
                            self.values[*val] = res.into();
                        }
                    }
                }
                return;
            }
        };

        let val = match opcode {
            Opcode::Inot | Opcode::Bnot => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildNot(self.llbuilder, arg, UNNAMED)
            }

            Opcode::Ineg => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildNeg(self.llbuilder, arg, UNNAMED)
            }
            Opcode::Fneg => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildFNeg(self.llbuilder, arg, UNNAMED)
            }
            Opcode::IFcast => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildSIToFP(self.llbuilder, arg, self.cx.ty_real(), UNNAMED)
            }
            Opcode::BFcast => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildUIToFP(self.llbuilder, arg, self.cx.ty_real(), UNNAMED)
            }
            Opcode::BIcast => {
                let arg = self.values[args[0]].get(self);
                llvm::LLVMBuildIntCast2(self.llbuilder, arg, self.cx.ty_int(), llvm::False, UNNAMED)
            }
            Opcode::IBcast => self.build_int_cmp(&[args[0], ZERO], llvm::IntPredicate::IntNE),
            Opcode::FBcast => self.build_real_cmp(&[args[0], F_ZERO], llvm::RealPredicate::RealONE),
            Opcode::Iadd => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildAdd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Isub => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildSub(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Imul => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildMul(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Idiv => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildSDiv(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Irem => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildSRem(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Ishl => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildShl(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Ishr => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildLShr(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Ixor => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildXor(self.llbuilder, lhs, rhs, UNNAMED)
            }

            Opcode::Iand => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildAnd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Ior => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildOr(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Fadd => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildFAdd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Fsub => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildFSub(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Fmul => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildFMul(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Fdiv => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildFDiv(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Frem => {
                let lhs = self.values[args[0]].get(self);
                let rhs = self.values[args[1]].get(self);
                llvm::LLVMBuildFRem(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Opcode::Ilt => self.build_int_cmp(args, llvm::IntPredicate::IntSLT),
            Opcode::Igt => self.build_int_cmp(args, llvm::IntPredicate::IntSGT),
            Opcode::Flt => self.build_real_cmp(args, llvm::RealPredicate::RealOLT),
            Opcode::Fgt => self.build_real_cmp(args, llvm::RealPredicate::RealOGT),
            Opcode::Ile => self.build_int_cmp(args, llvm::IntPredicate::IntSLE),
            Opcode::Ige => self.build_int_cmp(args, llvm::IntPredicate::IntSGE),
            Opcode::Fle => self.build_real_cmp(args, llvm::RealPredicate::RealOLE),
            Opcode::Fge => self.build_real_cmp(args, llvm::RealPredicate::RealOGE),
            Opcode::Ieq | Opcode::Beq => self.build_int_cmp(args, llvm::IntPredicate::IntEQ),
            Opcode::Feq => self.build_real_cmp(args, llvm::RealPredicate::RealOEQ),
            Opcode::Fne => self.build_real_cmp(args, llvm::RealPredicate::RealONE),
            Opcode::Bne | Opcode::Ine => self.build_int_cmp(args, llvm::IntPredicate::IntNE),
            Opcode::FIcast => self.intrinsic(args, "llvm.llround.i32.f64"),
            Opcode::Seq => self.strcmp(args, false),
            Opcode::Sne => self.strcmp(args, true),
            Opcode::Sqrt => self.intrinsic(args, "llvm.sqrt.f64"),
            Opcode::Exp => self.intrinsic(args, "llvm.exp.f64"),
            Opcode::Ln => self.intrinsic(args, "llvm.log.f64"),
            Opcode::Log => self.intrinsic(args, "llvm.log10.f64"),
            Opcode::Clog2 => {
                let leading_zeros = self.intrinsic(&[args[0], true.into()], "llvm.ctlz");
                let total_bits = self.cx.const_int(32);
                llvm::LLVMBuildSub(self.llbuilder, total_bits, leading_zeros, UNNAMED)
            }
            Opcode::Floor => self.intrinsic(args, "llvm.floor.f64"),
            Opcode::Ceil => self.intrinsic(args, "llvm.ceil.f64"),
            Opcode::Sin => self.intrinsic(args, "llvm.sin.f64"),
            Opcode::Cos => self.intrinsic(args, "llvm.cos.f64"),
            Opcode::Tan => self.intrinsic(args, "tan"),
            Opcode::Hypot => self.intrinsic(args, "hypot"),
            Opcode::Asin => self.intrinsic(args, "asin"),
            Opcode::Acos => self.intrinsic(args, "acos"),
            Opcode::Atan => self.intrinsic(args, "atan"),
            Opcode::Atan2 => self.intrinsic(args, "atan2"),
            Opcode::Sinh => self.intrinsic(args, "sinh"),
            Opcode::Cosh => self.intrinsic(args, "cosh"),
            Opcode::Tanh => self.intrinsic(args, "tanh"),
            Opcode::Asinh => self.intrinsic(args, "asinh"),
            Opcode::Acosh => self.intrinsic(args, "acosh"),
            Opcode::Atanh => self.intrinsic(args, "atanh"),
            Opcode::Pow => self.intrinsic(args, "llvm.pow.f64"),
            Opcode::OptBarrier => self.values[args[0]].get(self),
            Opcode::Br | Opcode::Jmp | Opcode::Call | Opcode::Phi => unreachable!(),
        };

        let res = self.func.dfg.first_result(inst);
        self.values[res] = val.into();

        if matches!(
            opcode,
            Opcode::Fneg
                | Opcode::Feq
                | Opcode::Fne
                | Opcode::Fadd
                | Opcode::Fsub
                | Opcode::Fmul
                | Opcode::Fdiv
                | Opcode::Frem
                | Opcode::Flt
                | Opcode::Fgt
                | Opcode::Fle
                | Opcode::Fge
                | Opcode::Sqrt
                | Opcode::Exp
                | Opcode::Ln
                | Opcode::Log
                | Opcode::Clog2
                | Opcode::Floor
                | Opcode::Ceil
                | Opcode::Sin
                | Opcode::Cos
                | Opcode::Tan
                | Opcode::Hypot
                | Opcode::Asin
                | Opcode::Acos
                | Opcode::Atan
                | Opcode::Atan2
                | Opcode::Sinh
                | Opcode::Cosh
                | Opcode::Tanh
                | Opcode::Asinh
                | Opcode::Acosh
                | Opcode::Atanh
                | Opcode::Pow
        ) {
            match fast_math_mode {
                FastMathMode::Full => llvm::LLVMSetFastMath(val),
                FastMathMode::Partial => llvm::LLVMSetPartialFastMath(val),
                FastMathMode::Disabled => (),
            }
        }
    }

    unsafe fn strcmp(&mut self, args: &[Value], invert: bool) -> &'ll llvm::Value {
        let res = self.intrinsic(args, "strcmp");
        let predicate = if invert { llvm::IntPredicate::IntNE } else { llvm::IntPredicate::IntEQ };

        LLVMBuildICmp(self.llbuilder, predicate, res, self.cx.const_int(0), UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn store(&self, ptr: &'ll llvm::Value, val: &'ll llvm::Value) {
        LLVMBuildStore(self.llbuilder, val, ptr);
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn load(&self, ty: &'ll llvm::Type, ptr: &'ll llvm::Value) -> &'ll llvm::Value {
        LLVMBuildLoad2(self.llbuilder, ty, ptr, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn imul(&self, val1: &'ll llvm::Value, val2: &'ll llvm::Value) -> &'ll llvm::Value {
        llvm::LLVMBuildMul(self.llbuilder, val1, val2, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn ptr_diff(
        &self,
        ptr1: &'ll llvm::Value,
        ptr2: &'ll llvm::Value,
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildPtrDiff(self.llbuilder, ptr1, ptr2, UNNAMED)
    }

    /// # Safety
    ///
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn is_null_ptr(&self, ptr: &'ll llvm::Value) -> &'ll llvm::Value {
        let ty = llvm::LLVMTypeOf(ptr);
        let null_ptr = self.cx.const_null_ptr(ty);

        LLVMBuildICmp(self.llbuilder, llvm::IntPredicate::IntEQ, null_ptr, ptr, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    unsafe fn build_int_cmp(
        &mut self,
        args: &[Value],
        predicate: llvm::IntPredicate,
    ) -> &'ll llvm::Value {
        let lhs = self.values[args[0]].get(self);
        let rhs = self.values[args[1]].get(self);
        self.int_cmp(lhs, rhs, predicate)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn int_cmp(
        &self,
        lhs: &'ll llvm::Value,
        rhs: &'ll llvm::Value,
        predicate: llvm::IntPredicate,
    ) -> &'ll llvm::Value {
        LLVMBuildICmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    unsafe fn build_real_cmp(
        &mut self,
        args: &[Value],
        predicate: llvm::RealPredicate,
    ) -> &'ll llvm::Value {
        let lhs = self.values[args[0]].get(self);
        let rhs = self.values[args[1]].get(self);
        self.real_cmp(lhs, rhs, predicate)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn real_cmp(
        &mut self,
        lhs: &'ll llvm::Value,
        rhs: &'ll llvm::Value,
        predicate: llvm::RealPredicate,
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildFCmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    unsafe fn intrinsic(&mut self, args: &[Value], name: &'static str) -> &'ll llvm::Value {
        let (ty, fun) =
            self.cx.intrinsic(name).unwrap_or_else(|| unreachable!("intrinsic {} not found", name));
        let args: ArrayVec<_, 2> = args.iter().map(|arg| self.values[*arg].get(self)).collect();

        llvm::LLVMBuildCall2(self.llbuilder, ty, fun, args.as_ptr(), args.len() as u32, UNNAMED)
    }
}
