use std::ffi::c_void;
use std::mem::size_of_val;
use std::slice;

use mir::{Block, FuncRef, Function, Inst, Opcode, Param, Value};
use typed_index_collections::{TiSlice, TiVec};

pub use crate::data::Data;

mod data;

pub struct InterpreterState {
    vals: TiVec<Value, Data>,
    prev_bb: Block,
    next_inst: Option<Inst>,
}

impl InterpreterState {
    pub fn write(&mut self, dst: Value, val: impl Into<Data>) {
        self.vals[dst] = val.into()
    }

    pub fn read<T: From<Data>>(&self, val: Value) -> T {
        self.vals[val].into()
    }
}

pub type Func<'a> = fn(&mut InterpreterState, &[Value], &[Value], *mut c_void);

pub struct Interpreter<'a> {
    pub state: InterpreterState,
    calls: &'a TiSlice<FuncRef, (Func<'a>, *mut c_void)>,
    func: &'a Function,
}

impl<'a> Interpreter<'a> {
    pub fn test(func: &'a Function) -> Interpreter<'a> {
        Interpreter::new(func, TiSlice::from_ref(&[]), TiSlice::from_ref(&[]))
    }

    pub fn new(
        func: &'a Function,
        calls: &'a TiSlice<FuncRef, (Func<'a>, *mut c_void)>,
        args: &TiSlice<Param, Data>,
    ) -> Interpreter<'a> {
        let vals = func
            .dfg
            .values()
            .map(|val| match func.dfg.value_def(val) {
                mir::ValueDef::Result(_, _) => Data::UNDEF,
                mir::ValueDef::Param(param) => args[param],
                mir::ValueDef::Const(val) => val.into(),
            })
            .collect();

        let entry =
            func.layout.entry_block().expect("Function without entry block can not be interpreted");

        let state =
            InterpreterState { vals, prev_bb: entry, next_inst: func.layout.first_inst(entry) };

        Interpreter { state, calls, func }
    }

    pub fn run(&mut self) {
        while let Some(inst) = self.state.next_inst {
            self.eval(inst)
        }
    }

    fn jmp(&mut self, src: Inst, dst: Block) {
        self.state.prev_bb = self.func.layout.inst_block(src).unwrap();
        self.state.next_inst = self.func.layout.first_inst(dst);
    }

    pub fn eval(&mut self, inst: Inst) {
        let inst_data = self.func.dfg.insts[inst];
        let (opcode, args) = match inst_data {
            mir::InstructionData::Unary { opcode, ref arg } => (opcode, slice::from_ref(arg)),
            mir::InstructionData::Binary { opcode, ref args } => (opcode, args.as_slice()),
            mir::InstructionData::Branch { cond, then_dst, else_dst, .. } => {
                let dst = if self.state.vals[cond].into() { then_dst } else { else_dst };
                self.jmp(inst, dst);
                return;
            }
            mir::InstructionData::PhiNode(phi) => {
                let val = self.func.dfg.phi_edge_val(phi, self.state.prev_bb).unwrap();
                let res = self.func.dfg.first_result(inst);
                self.state.vals[res] = self.state.vals[val];
                (Opcode::Phi, [].as_slice())
            }
            mir::InstructionData::Jump { destination } => {
                self.jmp(inst, destination);
                return;
            }
            mir::InstructionData::Call { func_ref, args } => {
                let (fun, data) = self.calls[func_ref];
                let args = args.as_slice(&self.func.dfg.insts.value_lists);
                let rets = self.func.dfg.inst_results(inst);
                fun(&mut self.state, args, rets, data);
                self.state.next_inst = self.func.layout.next_inst(inst);
                return;
            }
        };

        // advance first so we can early return later
        self.state.next_inst = self.func.layout.next_inst(inst);

        let args = |i| self.state.vals[args[i]];
        let res = self.func.dfg.first_result(inst);
        // let write = |sel: &mut Self, val| sel.state.write(res, val);

        let val = match opcode {
            mir::Opcode::Inot => (!args(0).i32()).into(),
            mir::Opcode::Bnot => (!args(0).bool()).into(),
            mir::Opcode::Fneg => (-args(0).f64()).into(),
            mir::Opcode::Ineg => (-args(0).i32()).into(),
            mir::Opcode::FIcast => (args(0).f64() as i32).into(),
            mir::Opcode::IFcast => (args(0).i32() as f64).into(),
            mir::Opcode::BIcast => (args(0).bool() as i32).into(),
            mir::Opcode::IBcast => (args(0).i32() != 0).into(),
            mir::Opcode::FBcast => (args(0).f64().round() as i32).into(),
            mir::Opcode::BFcast => (args(0).bool() as i32 as f64).into(),
            mir::Opcode::OptBarrier => args(0),
            mir::Opcode::Sqrt => f64::sqrt(args(0).f64()).into(),
            mir::Opcode::Exp => f64::exp(args(0).f64()).into(),
            mir::Opcode::Ln => f64::ln(args(0).f64()).into(),
            mir::Opcode::Log => f64::log10(args(0).f64()).into(),
            mir::Opcode::Clog2 => {
                let val = args(0).i32();
                let val = 8 * size_of_val(&val) as i32 - val.leading_zeros() as i32;
                val.into()
            }
            mir::Opcode::Floor => f64::floor(args(0).f64()).into(),
            mir::Opcode::Ceil => f64::ceil(args(0).f64()).into(),
            mir::Opcode::Sin => f64::sin(args(0).f64()).into(),
            mir::Opcode::Cos => f64::cos(args(0).f64()).into(),
            mir::Opcode::Tan => f64::tan(args(0).f64()).into(),
            mir::Opcode::Asin => f64::asin(args(0).f64()).into(),
            mir::Opcode::Acos => f64::acos(args(0).f64()).into(),
            mir::Opcode::Atan => f64::atan(args(0).f64()).into(),
            mir::Opcode::Sinh => f64::sinh(args(0).f64()).into(),
            mir::Opcode::Cosh => f64::cosh(args(0).f64()).into(),
            mir::Opcode::Tanh => f64::tanh(args(0).f64()).into(),
            mir::Opcode::Asinh => f64::asinh(args(0).f64()).into(),
            mir::Opcode::Acosh => f64::acosh(args(0).f64()).into(),
            mir::Opcode::Atanh => f64::atanh(args(0).f64()).into(),
            mir::Opcode::Iadd => (args(0).i32() + args(1).i32()).into(),
            mir::Opcode::Isub => (args(0).i32() - args(1).i32()).into(),
            mir::Opcode::Imul => (args(0).i32() * args(1).i32()).into(),
            mir::Opcode::Idiv => (args(0).i32() / args(1).i32()).into(),
            mir::Opcode::Irem => (args(0).i32() % args(1).i32()).into(),
            mir::Opcode::Ishl => (args(0).i32() << args(1).i32()).into(),
            mir::Opcode::Ishr => (args(0).i32() >> args(1).i32()).into(),
            mir::Opcode::Ixor => (args(0).i32() ^ args(1).i32()).into(),
            mir::Opcode::Iand => (args(0).i32() & args(1).i32()).into(),
            mir::Opcode::Ior => (args(0).i32() | args(1).i32()).into(),
            mir::Opcode::Fadd => (args(0).f64() + args(1).f64()).into(),
            mir::Opcode::Fsub => (args(0).f64() - args(1).f64()).into(),
            mir::Opcode::Fmul => (args(0).f64() * args(1).f64()).into(),
            mir::Opcode::Fdiv => (args(0).f64() / args(1).f64()).into(),
            mir::Opcode::Frem => (args(0).f64() % args(1).f64()).into(),
            mir::Opcode::Ilt => (args(0).i32() < args(1).i32()).into(),
            mir::Opcode::Igt => (args(0).i32() > args(1).i32()).into(),
            mir::Opcode::Ige => (args(0).i32() >= args(1).i32()).into(),
            mir::Opcode::Ile => (args(0).i32() <= args(1).i32()).into(),
            mir::Opcode::Flt => (args(0).f64() < args(1).f64()).into(),
            mir::Opcode::Fgt => (args(0).f64() > args(1).f64()).into(),
            mir::Opcode::Fge => (args(0).f64() >= args(1).f64()).into(),
            mir::Opcode::Fle => (args(0).f64() <= args(1).f64()).into(),
            mir::Opcode::Ieq => (args(0).i32() == args(1).i32()).into(),
            mir::Opcode::Feq => (args(0).f64() == args(1).f64()).into(),
            mir::Opcode::Seq => (args(0).str() == args(1).str()).into(),
            mir::Opcode::Beq => (args(0).bool() == args(1).bool()).into(),
            mir::Opcode::Ine => (args(0).i32() != args(1).i32()).into(),
            mir::Opcode::Fne => (args(0).f64() != args(1).f64()).into(),
            mir::Opcode::Sne => (args(0).str() != args(1).str()).into(),
            mir::Opcode::Bne => (args(0).bool() != args(1).bool()).into(),
            mir::Opcode::Hypot => f64::hypot(args(0).f64(), args(1).f64()).into(),
            mir::Opcode::Atan2 => f64::atan2(args(0).f64(), args(1).f64()).into(),
            mir::Opcode::Pow => f64::powf(args(0).f64(), args(1).f64()).into(),
            mir::Opcode::Br | mir::Opcode::Jmp => unreachable!(),
            mir::Opcode::Call | mir::Opcode::Phi => return,
        };
        self.state.vals[res] = val;
    }
}
