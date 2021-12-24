use std::mem::take;

use arrayvec::ArrayVec;
use cfg::{
    BasicBlock, Callback, CfgParam, ControlFlowGraph, InstrDst, Local, Op, Operand, Phi, Place,
    Terminator,
};
use libc::c_uint;
use llvm::UNNAMED;
use typed_index_collections::TiVec;

use crate::callbacks::CallbackFun;
use crate::CodegenCx;

// All Builders must have an llfn associated with them
#[must_use]
pub struct Builder<'a, 'cx, 'll> {
    llbuilder: &'ll mut llvm::Builder<'ll>,
    pub cx: &'a mut CodegenCx<'cx, 'll>,
    pub cfg: &'a ControlFlowGraph,
    pub blocks: TiVec<BasicBlock, &'ll llvm::BasicBlock>,
    pub locals: TiVec<Local, Option<&'ll llvm::Value>>,
    pub places: TiVec<Place, (&'ll llvm::Type, &'ll llvm::Value)>,
    pub params: TiVec<CfgParam, &'ll llvm::Value>,
    pub callbacks: TiVec<Callback, CallbackFun<'ll>>,
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
        cfg: &'a ControlFlowGraph,
        fun: &'ll llvm::Value,
    ) -> Self {
        let llbuilder = unsafe { llvm::LLVMCreateBuilderInContext(cx.llcx) };
        let blocks: TiVec<_, _> = cfg
            .blocks
            .keys()
            .map(|_| unsafe { llvm::LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED) })
            .collect();
        unsafe { llvm::LLVMPositionBuilderAtEnd(llbuilder, blocks[cfg.entry()]) };

        Builder {
            llbuilder,
            cx,
            cfg,
            blocks,
            locals: vec![None; cfg.next_local.into()].into(),
            places: Default::default(),
            params: Default::default(),
            callbacks: Default::default(),
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
    pub unsafe fn struct_gep(&self, ptr: &'ll llvm::Value, idx: u32) -> &'ll llvm::Value {
        let struct_ty = llvm::LLVMGetElementType(llvm::LLVMTypeOf(ptr));
        self.typed_struct_gep(struct_ty, ptr, idx)
    }

    /// # Safety
    /// * Must not be called when a block that already contains a terminator is selected
    /// * struct_ty must be a valid struct type for this pointer and
    pub unsafe fn typed_struct_gep(
        &self,
        struct_ty: &'ll llvm::Type,
        ptr: &'ll llvm::Value,
        idx: u32,
    ) -> &'ll llvm::Value {
        llvm::LLVMBuildStructGEP2(self.llbuilder, struct_ty, ptr, idx, UNNAMED)
    }

    /// # Safety
    /// Must not be called if any block already contain any non-phi instruction (eg must not be
    /// called twice)
    pub unsafe fn build_cfg(&mut self) {
        for bb in self.cfg.reverse_postorder() {
            self.build_bb(bb)
        }
    }

    pub fn select_bb(&self, bb: BasicBlock) {
        unsafe {
            llvm::LLVMPositionBuilderAtEnd(self.llbuilder, self.blocks[bb]);
        }
    }

    /// # Safety
    ///
    /// Must not be called if any non phi instruction has already been build for `bb`
    /// The means it must not be called twice for the same bloc
    pub unsafe fn build_bb(&mut self, bb: BasicBlock) {
        self.select_bb(bb);
        let block = &self.cfg.blocks[bb];
        for phi in &block.phis {
            self.build_phi(phi);
        }

        for instr in &block.instructions {
            self.build_instr(
                instr.dst,
                instr.op,
                &instr.args,
                if instr.src < 0 { FastMathMode::Partial } else { FastMathMode::Disabled },
            )
        }

        self.build_terminator(block.terminator());
    }

    unsafe fn build_phi(&mut self, phi: &Phi) {
        let (blocks, vals): (Vec<_>, Vec<_>) = phi
            .sources
            .iter()
            .map(|(bb, val)| {
                (
                    self.blocks[*bb],
                    self.locals[*val].unwrap_or_else(|| {
                        unreachable!(
                            "local {:?} used before initialization in {:?} -> {:?} \n\n {}",
                            val,
                            bb,
                            phi,
                            self.cfg.dump(Some(self.cx.literals)),
                        )
                    }),
                )
            })
            .unzip();
        let ty = self.cx.val_ty(vals[0]);
        let val = llvm::LLVMBuildPhi(self.llbuilder, ty, UNNAMED);
        llvm::LLVMAddIncoming(val, vals.as_ptr(), blocks.as_ptr(), vals.len() as c_uint);
        self.locals[phi.dst] = Some(val);
    }

    /// # Safety
    /// must not be called multiple times
    /// a terminator must not be build for the exit bb trough other means
    pub unsafe fn ret(&mut self, val: &'ll llvm::Value) {
        llvm::LLVMBuildRet(self.llbuilder, val);
    }

    unsafe fn build_terminator(&mut self, term: &Terminator) {
        match *term {
            Terminator::Goto(bb) => {
                llvm::LLVMBuildBr(self.llbuilder, self.blocks[bb]);
            }
            Terminator::Split { ref condition, true_block, false_block, .. } => {
                let cond = self.operand(condition);
                {
                    llvm::LLVMBuildCondBr(
                        self.llbuilder,
                        cond,
                        self.blocks[true_block],
                        self.blocks[false_block],
                    );
                }
            }
            Terminator::Ret => (), // End has to be terminated by calle
        }
    }

    /// # Safety
    /// must not be called multiple times
    /// a terminator must not be build for the exit bb trough other means
    pub unsafe fn build_returns(
        &mut self,
        mut f: impl FnMut(&mut Self, BasicBlock) -> &'ll llvm::Value,
    ) {
        for (bb, data) in self.cfg.postorder_iter() {
            if data.terminator() == &Terminator::Ret {
                let val = f(self, bb);
                self.ret(val);
            }
        }
    }

    /// # Safety
    /// Must only be called when after the builder has been positioned
    /// Not Phis may be constructed for the current block after this function has been called
    /// Must not be called when the builder has selected a block that already contains a terminator
    pub unsafe fn build_instr(
        &mut self,
        dst: InstrDst,
        op: Op,
        args: &[Operand],
        fast_math_mode: FastMathMode,
    ) {
        let val = match op {
            Op::NoOp => return,
            Op::Copy => self.operand(&args[0]),
            Op::IntBitNegate | Op::BoolBitNegate => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildNot(self.llbuilder, arg, UNNAMED)
            }

            Op::IntArithNeg => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildNeg(self.llbuilder, arg, UNNAMED)
            }
            Op::RealArtihNeg => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildFNeg(self.llbuilder, arg, UNNAMED)
            }
            Op::IntToReal => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildSIToFP(self.llbuilder, arg, self.cx.ty_real(), UNNAMED)
            }
            Op::BoolToReal => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildUIToFP(self.llbuilder, arg, self.cx.ty_real(), UNNAMED)
            }
            Op::BoolToInt => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildIntCast2(self.llbuilder, arg, self.cx.ty_int(), llvm::False, UNNAMED)
            }
            Op::IntToBool => self.int_cmp(&[args[0], 0i32.into()], llvm::IntPredicate::IntNE),

            Op::RealToBool => self.real_cmp(&[args[0], 0f64.into()], llvm::RealPredicate::RealONE),
            Op::IntAdd => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildAdd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntSub => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildSub(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntMul => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildMul(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntDiv => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildSDiv(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntRem => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildSRem(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntShl => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildShl(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntShr => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildLShr(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntXor => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildXor(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntNXor => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                let val = llvm::LLVMBuildXor(self.llbuilder, lhs, rhs, UNNAMED);
                llvm::LLVMBuildNot(self.llbuilder, val, UNNAMED)
            }
            Op::IntAnd => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildAnd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntOr => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildOr(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::RealAdd => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildFAdd(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::RealSub => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildFSub(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::RealMul => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildFMul(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::RealDiv => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildFDiv(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::RealRem => {
                let lhs = self.operand(&args[0]);
                let rhs = self.operand(&args[1]);
                llvm::LLVMBuildFRem(self.llbuilder, lhs, rhs, UNNAMED)
            }
            Op::IntLessThen => self.int_cmp(args, llvm::IntPredicate::IntSLT),
            Op::IntGreaterThen => self.int_cmp(args, llvm::IntPredicate::IntSGT),
            Op::RealLessThen => self.real_cmp(args, llvm::RealPredicate::RealOLT),
            Op::RealGreaterThen => self.real_cmp(args, llvm::RealPredicate::RealOGT),
            Op::IntLessEqual => self.int_cmp(args, llvm::IntPredicate::IntSLE),
            Op::IntGreaterEqual => self.int_cmp(args, llvm::IntPredicate::IntSGE),
            Op::RealLessEqual => self.real_cmp(args, llvm::RealPredicate::RealOLE),
            Op::RealGreaterEqual => self.real_cmp(args, llvm::RealPredicate::RealOGE),
            Op::IntEq | Op::BoolEq => self.int_cmp(args, llvm::IntPredicate::IntEQ),
            Op::RealEq => self.real_cmp(args, llvm::RealPredicate::RealOEQ),
            Op::RealNeq => self.real_cmp(args, llvm::RealPredicate::RealONE),
            Op::BoolNeq | Op::IntNeq => self.int_cmp(args, llvm::IntPredicate::IntNE),
            Op::RealToInt => self.intrinsic(args, "llvm.llround.i32.f64"),
            Op::StringEq => self.strcmp(args, false),
            Op::StringNeq => self.strcmp(args, true),
            Op::Sqrt => self.intrinsic(args, "llvm.sqrt.f64"),
            Op::Exp => self.intrinsic(args, "llvm.exp.f64"),
            Op::Ln => self.intrinsic(args, "llvm.log.f64"),
            Op::Log => self.intrinsic(args, "llvm.log10.f64"),
            Op::Clog2 => todo!(),
            Op::Floor => self.intrinsic(args, "llvm.floor.f64"),
            Op::Ceil => self.intrinsic(args, "llvm.ceil.f64"),
            Op::Sin => self.intrinsic(args, "llvm.sin.f64"),
            Op::Cos => self.intrinsic(args, "llvm.cos.f64"),
            Op::Tan => self.intrinsic(args, "tan"),
            Op::Hypot => self.intrinsic(args, "hypot"),
            Op::ArcSin => self.intrinsic(args, "asin"),
            Op::ArcCos => self.intrinsic(args, "acos"),
            Op::ArcTan => self.intrinsic(args, "atan"),
            Op::ArcTan2 => self.intrinsic(args, "atan2"),
            Op::SinH => self.intrinsic(args, "sinh"),
            Op::CosH => self.intrinsic(args, "cosh"),
            Op::TanH => self.intrinsic(args, "tanh"),
            Op::ArcSinH => self.intrinsic(args, "asinh"),
            Op::ArcCosH => self.intrinsic(args, "acosh"),
            Op::ArcTanH => self.intrinsic(args, "atanh"),
            Op::RealPow => self.intrinsic(args, "llvm.pow.f64"),
            Op::Call(callback) => {
                let callbacks = take(&mut self.callbacks);
                let callback = &callbacks[callback];
                let operands: Vec<_> = callback
                    .state
                    .iter()
                    .copied()
                    .chain(args.iter().map(|operand| self.operand(operand)))
                    .collect();
                let res = llvm::LLVMBuildCall2(
                    self.llbuilder,
                    callback.fun_ty,
                    callback.fun,
                    operands.as_ptr(),
                    operands.len() as u32,
                    UNNAMED,
                );
                self.callbacks = callbacks;
                res
            }
        };

        if matches!(
            op,
            Op::RealArtihNeg
                | Op::RealAdd
                | Op::RealSub
                | Op::RealMul
                | Op::RealDiv
                | Op::RealRem
                | Op::RealLessThen
                | Op::RealGreaterThen
                | Op::RealLessEqual
                | Op::RealGreaterEqual
                | Op::Sqrt
                | Op::Exp
                | Op::Ln
                | Op::Log
                | Op::Clog2
                | Op::Floor
                | Op::Ceil
                | Op::Sin
                | Op::Cos
                | Op::Tan
                | Op::Hypot
                | Op::ArcSin
                | Op::ArcCos
                | Op::ArcTan
                | Op::ArcTan2
                | Op::SinH
                | Op::CosH
                | Op::TanH
                | Op::ArcSinH
                | Op::ArcCosH
                | Op::ArcTanH
                | Op::RealPow
        ) {
            match fast_math_mode {
                FastMathMode::Full => llvm::LLVMSetFastMath(val),
                FastMathMode::Partial => llvm::LLVMSetPartialFastMath(val),
                FastMathMode::Disabled => (),
            }
        }

        match dst {
            InstrDst::Local(local) => self.locals[local] = Some(val),
            InstrDst::Place(place) => self.store(self.places[place].1, val),
            InstrDst::Ignore => (),
        }
    }

    unsafe fn strcmp(&mut self, args: &[Operand], invert: bool) -> &'ll llvm::Value {
        let res = self.intrinsic(args, "strcmp");
        let predicate = if invert { llvm::IntPredicate::IntNE } else { llvm::IntPredicate::IntEQ };

        llvm::LLVMBuildICmp(self.llbuilder, predicate, res, self.cx.const_int(0), UNNAMED)
    }

    /// # Safety
    /// Must only be called when after the builder has been positioned
    /// Not Phis may be constructed for the current block after this function has been called
    /// Must not be called when the builder has selected a block that already contains a terminator
    pub unsafe fn operand(&mut self, operand: &Operand) -> &'ll llvm::Value {
        match *operand {
            Operand::Const(ref val) => self.cx.const_val(val),
            Operand::Local(local) => self.locals[local].unwrap_or_else(|| {
                unreachable!(
                    "local {:?} used before initialization \n\n {}",
                    local,
                    self.cfg.dump(Some(self.cx.literals)),
                )
            }),

            Operand::Place(place) => {
                let (ty, ptr) = self.places[place];
                self.load(ty, ptr)
            }
            Operand::CfgParam(param) => self.params[param],
        }
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn store(&self, ptr: &'ll llvm::Value, val: &'ll llvm::Value) {
        llvm::LLVMBuildStore(self.llbuilder, val, ptr);
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    pub unsafe fn load(&self, ty: &'ll llvm::Type, ptr: &'ll llvm::Value) -> &'ll llvm::Value {
        llvm::LLVMBuildLoad2(self.llbuilder, ty, ptr, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    unsafe fn int_cmp(
        &mut self,
        args: &[Operand],
        predicate: llvm::IntPredicate,
    ) -> &'ll llvm::Value {
        let lhs = self.operand(&args[0]);
        let rhs = self.operand(&args[1]);
        llvm::LLVMBuildICmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    /// # Safety
    /// Must not be called when a block that already contains a terminator is selected
    unsafe fn real_cmp(
        &mut self,
        args: &[Operand],
        predicate: llvm::RealPredicate,
    ) -> &'ll llvm::Value {
        let lhs = self.operand(&args[0]);
        let rhs = self.operand(&args[1]);
        llvm::LLVMBuildFCmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    unsafe fn intrinsic(&mut self, args: &[Operand], name: &'static str) -> &'ll llvm::Value {
        let (ty, fun) =
            self.cx.intrinsic(name).unwrap_or_else(|| unreachable!("intrinsic {} not found", name));
        let args: ArrayVec<_, 2> = args.iter().map(|arg| self.operand(arg)).collect();

        llvm::LLVMBuildCall2(self.llbuilder, ty, fun, args.as_ptr(), args.len() as u32, UNNAMED)
    }
}
