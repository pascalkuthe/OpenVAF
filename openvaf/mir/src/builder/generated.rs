//! Generated by `gen_instr_builder`, do not edit by hand.

use super::*;
#[doc = r" Convenience methods for building instructions."]
#[doc = r""]
#[doc = r" The `InstBuilder` trait has one method per instruction opcode for"]
#[doc = r" conveniently constructing the instruction with minimum arguments."]
#[doc = r" Polymorphic instructions infer their result types from the input"]
#[doc = r" arguments when possible. In some cases, an explicit `ctrl_typevar`"]
#[doc = r" argument is required."]
#[doc = r""]
#[doc = r" The opcode methods return the new instruction's result values, or"]
#[doc = r" the `Inst` itself for instructions that don't have any results."]
#[doc = r""]
#[doc = r" There is also a method per instruction format. These methods all"]
#[doc = r" return an `Inst`."]
pub trait InstBuilder<'f>: InstBuilderBase<'f> {
    fn unary(self, opcode: Opcode, arg: Value) -> (Inst, &'f mut DataFlowGraph) {
        let data = InstructionData::Unary { opcode, arg };
        self.build(data)
    }
    fn binary(self, opcode: Opcode, arg1: Value, arg2: Value) -> (Inst, &'f mut DataFlowGraph) {
        let data = InstructionData::Binary { opcode, args: [arg1, arg2] };
        self.build(data)
    }
    fn binary1(self, opcode: Opcode, arg1: Value, arg2: Value) -> Value {
        let (inst, dfg) = self.binary(opcode, arg1, arg2);
        dfg.first_result(inst)
    }
    fn branch(
        self,
        cond: Value,
        then_dst: Block,
        else_dst: Block,
        loop_entry: bool,
    ) -> (Inst, &'f mut DataFlowGraph) {
        let data = InstructionData::Branch { cond, then_dst, else_dst, loop_entry };
        self.build(data)
    }
    fn br(self, cond: Value, then_dst: Block, else_dst: Block) -> Inst {
        self.branch(cond, then_dst, else_dst, false).0
    }
    fn br_loop(self, cond: Value, then_dst: Block, else_dst: Block) -> Inst {
        self.branch(cond, then_dst, else_dst, true).0
    }
    fn jump(self, destination: Block) -> Inst {
        let data = InstructionData::Jump { destination };
        self.build(data).0
    }
    fn build_call(self, func_ref: FuncRef, args: ValueList) -> (Inst, &'f mut DataFlowGraph) {
        let data = InstructionData::Call { args, func_ref };
        self.build(data)
    }
    fn call(mut self, func_ref: FuncRef, args: &[Value]) -> Inst {
        let pool = &mut self.data_flow_graph_mut().insts.value_lists;
        let args = ValueList::from_slice(args, pool);
        self.build_call(func_ref, args).0
    }
    fn call1(mut self, func_ref: FuncRef, args: &[Value]) -> Value {
        let pool = &mut self.data_flow_graph_mut().insts.value_lists;
        let args = ValueList::from_slice(args, pool);
        let (inst, dfg) = self.build_call(func_ref, args);
        dfg.first_result(inst)
    }
    #[inline]
    fn phi(mut self, edges: &[(Block, Value)]) -> Value {
        let mut args = ValueList::new();
        let mut blocks = PhiMap::new();
        let dfg = self.data_flow_graph_mut();
        for (i, (block, val)) in edges.iter().enumerate() {
            args.push(*val, &mut dfg.insts.value_lists);
            blocks.insert(*block, i as u32, &mut dfg.phi_forest, &());
        }
        let (inst, dfg) = self.build(PhiNode { args, blocks }.into());
        dfg.first_result(inst)
    }
    fn inot(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Inot, arg0);
        dfg.first_result(inst)
    }
    fn bnot(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Bnot, arg0);
        dfg.first_result(inst)
    }
    fn fneg(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Fneg, arg0);
        dfg.first_result(inst)
    }
    fn ineg(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Ineg, arg0);
        dfg.first_result(inst)
    }
    fn ficast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::FIcast, arg0);
        dfg.first_result(inst)
    }
    fn ifcast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::IFcast, arg0);
        dfg.first_result(inst)
    }
    fn bicast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::BIcast, arg0);
        dfg.first_result(inst)
    }
    fn ibcast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::IBcast, arg0);
        dfg.first_result(inst)
    }
    fn fbcast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::FBcast, arg0);
        dfg.first_result(inst)
    }
    fn bfcast(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::BFcast, arg0);
        dfg.first_result(inst)
    }
    fn optbarrier(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::OptBarrier, arg0);
        dfg.first_result(inst)
    }
    fn sqrt(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Sqrt, arg0);
        dfg.first_result(inst)
    }
    fn exp(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Exp, arg0);
        dfg.first_result(inst)
    }
    fn ln(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Ln, arg0);
        dfg.first_result(inst)
    }
    fn log(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Log, arg0);
        dfg.first_result(inst)
    }
    fn clog2(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Clog2, arg0);
        dfg.first_result(inst)
    }
    fn floor(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Floor, arg0);
        dfg.first_result(inst)
    }
    fn ceil(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Ceil, arg0);
        dfg.first_result(inst)
    }
    fn sin(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Sin, arg0);
        dfg.first_result(inst)
    }
    fn cos(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Cos, arg0);
        dfg.first_result(inst)
    }
    fn tan(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Tan, arg0);
        dfg.first_result(inst)
    }
    fn asin(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Asin, arg0);
        dfg.first_result(inst)
    }
    fn acos(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Acos, arg0);
        dfg.first_result(inst)
    }
    fn atan(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Atan, arg0);
        dfg.first_result(inst)
    }
    fn sinh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Sinh, arg0);
        dfg.first_result(inst)
    }
    fn cosh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Cosh, arg0);
        dfg.first_result(inst)
    }
    fn tanh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Tanh, arg0);
        dfg.first_result(inst)
    }
    fn asinh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Asinh, arg0);
        dfg.first_result(inst)
    }
    fn acosh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Acosh, arg0);
        dfg.first_result(inst)
    }
    fn atanh(self, arg0: Value) -> Value {
        let (inst, dfg) = self.unary(Opcode::Atanh, arg0);
        dfg.first_result(inst)
    }
    fn iadd(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Iadd, arg0, arg1);
        dfg.first_result(inst)
    }
    fn isub(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Isub, arg0, arg1);
        dfg.first_result(inst)
    }
    fn imul(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Imul, arg0, arg1);
        dfg.first_result(inst)
    }
    fn idiv(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Idiv, arg0, arg1);
        dfg.first_result(inst)
    }
    fn irem(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Irem, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ishl(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ishl, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ishr(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ishr, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ixor(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ixor, arg0, arg1);
        dfg.first_result(inst)
    }
    fn iand(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Iand, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ior(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ior, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fadd(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fadd, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fsub(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fsub, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fmul(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fmul, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fdiv(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fdiv, arg0, arg1);
        dfg.first_result(inst)
    }
    fn frem(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Frem, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ilt(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ilt, arg0, arg1);
        dfg.first_result(inst)
    }
    fn igt(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Igt, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ige(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ige, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ile(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ile, arg0, arg1);
        dfg.first_result(inst)
    }
    fn flt(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Flt, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fgt(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fgt, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fge(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fge, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fle(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fle, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ieq(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ieq, arg0, arg1);
        dfg.first_result(inst)
    }
    fn feq(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Feq, arg0, arg1);
        dfg.first_result(inst)
    }
    fn seq(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Seq, arg0, arg1);
        dfg.first_result(inst)
    }
    fn beq(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Beq, arg0, arg1);
        dfg.first_result(inst)
    }
    fn ine(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Ine, arg0, arg1);
        dfg.first_result(inst)
    }
    fn fne(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Fne, arg0, arg1);
        dfg.first_result(inst)
    }
    fn sne(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Sne, arg0, arg1);
        dfg.first_result(inst)
    }
    fn bne(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Bne, arg0, arg1);
        dfg.first_result(inst)
    }
    fn hypot(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Hypot, arg0, arg1);
        dfg.first_result(inst)
    }
    fn atan2(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Atan2, arg0, arg1);
        dfg.first_result(inst)
    }
    fn pow(self, arg0: Value, arg1: Value) -> Value {
        let (inst, dfg) = self.binary(Opcode::Pow, arg0, arg1);
        dfg.first_result(inst)
    }
}