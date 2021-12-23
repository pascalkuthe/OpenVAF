use arrayvec::ArrayVec;
use cfg::{
    BasicBlock, CfgParam, ControlFlowGraph, InstrDst, Local, Op, Operand, Phi, Place, Terminator,
};
use libc::c_uint;
use llvm::{Type, Value, UNNAMED};
use typed_index_collections::TiVec;

use crate::CodegenCx;

// All Builders must have an llfn associated with them
#[must_use]
pub struct Builder<'a, 'll> {
    llbuilder: &'ll mut llvm::Builder<'ll>,
    pub cx: &'a mut CodegenCx<'a, 'll>,
    pub cfg: &'a ControlFlowGraph,
    blocks: TiVec<BasicBlock, &'ll llvm::BasicBlock>,
    locals: TiVec<Local, Option<&'ll Value>>,
    places: TiVec<Place, (&'ll Type, &'ll Value)>,
    params: TiVec<CfgParam, &'ll Value>,
}

impl Drop for Builder<'_, '_> {
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

impl<'a, 'll> Builder<'a, 'll> {
    /// # Safety
    /// At least one function must be created within the current module before a builder can be
    /// created
    pub unsafe fn new(
        cx: &'a mut CodegenCx<'a, 'll>,
        cfg: &'a ControlFlowGraph,
        fun: &'ll Value,
        places: TiVec<Place, (&'ll Type, &'ll Value)>,
        params: TiVec<CfgParam, &'ll Value>,
    ) -> Builder<'a, 'll> {
        let llbuilder = llvm::LLVMCreateBuilderInContext(cx.llcx);
        let blocks = cfg
            .blocks
            .keys()
            .map(|_| llvm::LLVMAppendBasicBlockInContext(cx.llcx, fun, UNNAMED))
            .collect();
        Builder {
            llbuilder,
            cx,
            cfg,
            blocks,
            locals: vec![None; cfg.next_place.into()].into(),
            places,
            params,
        }
    }

    pub fn build_bb(&mut self, bb: BasicBlock) {
        unsafe { llvm::LLVMPositionBuilderAtEnd(self.llbuilder, self.blocks[bb]) }
        let block = &self.cfg.blocks[bb];
        for phi in &block.phis {
            unsafe { self.build_phi(phi) }
        }

        for instr in &block.instructions {
            unsafe {
                self.build_instr(
                    instr.dst,
                    instr.op,
                    &instr.args,
                    if instr.src < 0 { FastMathMode::Partial } else { FastMathMode::Disabled },
                )
            }
        }

        unsafe { self.build_terminator(block.terminator()) }
    }

    unsafe fn build_phi(&mut self, phi: &Phi) {
        let (blocks, vals): (Vec<_>, Vec<_>) = phi
            .sources
            .iter()
            .map(|(bb, val)| (self.blocks[*bb], self.locals[*val].unwrap()))
            .unzip();
        let ty = self.cx.val_ty(vals[0]);
        let val = llvm::LLVMBuildPhi(self.llbuilder, ty, UNNAMED);
        llvm::LLVMAddIncoming(val, vals.as_ptr(), blocks.as_ptr(), vals.len() as c_uint);
        self.locals[phi.dst] = Some(val);
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
            Terminator::End => (), // End has to be terminated by the
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
            Op::IntToBool => {
                let arg = self.operand(&args[0]);
                llvm::LLVMBuildIntCast2(self.llbuilder, arg, self.cx.ty_int(), llvm::False, UNNAMED)
            }
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
            Op::RealToInt => self.intrinsic(args, "llvm.sqrt.f64"),
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
            Op::Call(_) => todo!(),
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

    unsafe fn strcmp(&mut self, args: &[Operand], invert: bool) -> &'ll Value {
        let res = self.intrinsic(args, "strcmp");
        let predicate = if invert { llvm::IntPredicate::IntNE } else { llvm::IntPredicate::IntEQ };

        llvm::LLVMBuildICmp(self.llbuilder, predicate, res, self.cx.const_int(0), UNNAMED)
    }

    /// # Safety
    /// Must only be called when after the builder has been positioned
    /// Not Phis may be constructed for the current block after this function has been called
    /// Must not be called when the builder has selected a block that already contains a terminator
    pub unsafe fn operand(&mut self, operand: &Operand) -> &'ll Value {
        match *operand {
            Operand::Const(ref val) => self.cx.const_val(val),
            Operand::Local(local) => self.locals[local].unwrap(),
            Operand::Place(place) => {
                let (ty, ptr) = self.places[place];
                self.load(ty, ptr)
            }
            Operand::CfgParam(param) => self.params[param],
        }
    }

    unsafe fn store(&self, ptr: &'ll Value, val: &'ll Value) {
        llvm::LLVMBuildStore(self.llbuilder, ptr, val);
    }

    unsafe fn load(&self, ty: &'ll Type, ptr: &'ll Value) -> &'ll Value {
        llvm::LLVMBuildLoad2(self.llbuilder, ty, ptr, UNNAMED)
    }

    unsafe fn int_cmp(&mut self, args: &[Operand], predicate: llvm::IntPredicate) -> &'ll Value {
        let lhs = self.operand(&args[0]);
        let rhs = self.operand(&args[1]);
        llvm::LLVMBuildICmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    unsafe fn real_cmp(&mut self, args: &[Operand], predicate: llvm::RealPredicate) -> &'ll Value {
        let lhs = self.operand(&args[0]);
        let rhs = self.operand(&args[1]);
        llvm::LLVMBuildFCmp(self.llbuilder, predicate, lhs, rhs, UNNAMED)
    }

    unsafe fn intrinsic(&mut self, args: &[Operand], name: &'static str) -> &'ll Value {
        let (ty, fun) =
            self.cx.intrinsic(name).unwrap_or_else(|| unreachable!("intrinsic {} not found", name));
        let args: ArrayVec<_, 2> = args.iter().map(|arg| self.operand(arg)).collect();

        llvm::LLVMBuildCall2(self.llbuilder, ty, fun, args.as_ptr(), args.len() as u32, UNNAMED)
    }
}
