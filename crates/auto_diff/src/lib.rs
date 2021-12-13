mod live_derivatives;
mod unkowns;

pub use live_derivatives::{LiveDerivativeAnalysis, LiveDerivatives};
pub use unkowns::{FirstOrderUnkown, Unkown, Unkowns};

// use std::mem::take;
// use std::vec;

// use ahash::AHashMap;
// use cfg::{
//     Callback, CfgBuilder, CfgParam, ControlFlowGraph, InstrDst, Instruction, Local, Op, Operand,
//     Place,
// };
// use stdx::impl_idx_from;
// use typed_index_collections::TiVec;
// use typed_indexmap::TiMap;

// pub type RequestedDerivatives = AHashMap<Local, (Operand, Vec<(CfgParam, f64)>)>;

// pub struct AutoDiffCtx<'a> {
//     old_cfg: ControlFlowGraph,
//     cfg: &'a mut CfgBuilder,
//     local_derivatives: AHashMap<Local, AHashMap<Unkown, Local>>,
//     place_derivatives: AHashMap<Place, AHashMap<Unkown, Place>>,
// }

// impl AutoDiffCtx<'_> {
//     fn operand_derivative(&mut self, op: &Operand, unkown: Unkown) -> Operand {
//         match *op {
//             Operand::Const(_) => 0f64.into(),
//             Operand::Local(local) => (*self
//                 .local_derivatives
//                 .entry(local)
//                 .or_default()
//                 .entry(unkown)
//                 .or_insert_with(|| self.cfg.new_local()))
//             .into(),
//             Operand::Place(place) => (*self
//                 .place_derivatives
//                 .entry(place)
//                 .or_default()
//                 .entry(unkown)
//                 .or_insert_with(|| self.cfg.new_place()))
//             .into(),
//             Operand::CfgParam(param) => self.unkowns[unkown]
//                 .iter()
//                 .find(|(it, _)| *it == param)
//                 .map_or(0.0, |(_, val)| f64::from_bits(*val))
//                 .into(),
//         }
//     }

//     fn fold_instruction(&mut self, mut instr: Instruction) {
//         if let Op::Call(callback) = instr.op {
//             if let Some(unkown) = self.unkowns.index(&callback) {
//                 instr.op = Op::Copy;
//                 instr.args[0] = self.operand_derivative(&instr.args[0], unkown);
//             }
//         }

//         if let InstrDst::Place(place) = instr.dst {
//             if self
//                 .place_derivatives
//                 .get(&place)
//                 .map_or(false, |derivatives| !derivatives.is_empty())
//             {
//                 // Places can depend upon themselves
//                 // In that case the write has to occur after all derivatives have been calculated
//                 // As a solution we copy the value to a temporary first and write to the place
//                 // after the derivatives have been calculated
//                 // This case likely never occurs in practice because all writes to places are
//                 // actually copys but if the code generation is improved in the future this is
//                 // good to have
//                 if instr.args.iter().any(|arg| arg == &Operand::Place(place)) {
//                     let tmp = self.cfg.new_local();
//                     instr.dst = tmp.into();
//                     self.cfg.build_assign(
//                         place.into(),
//                         Op::Copy,
//                         vec![Operand::Local(tmp)],
//                         instr.src,
//                     );
//                 };
//             }
//         }

//         self.generate_instr_derivatives(&instr);
//         self.cfg.add_instr(instr);
//     }

//     fn build_derivative(
//         &mut self,
//         dst: InstrDst,
//         op: Operand,
//         args: &[Operand],
//         src: i32,
//         original_val: Operand,
//         unkown: Unkown,
//     ) {
//         todo!()
//         // x = 0;
//         // z = 0;
//         // `while (i < 100) begin
//         //   z = x +1
//         //   x = y+2
//         // end
//     }

//     fn generate_instr_derivatives(&mut self, instr: &Instruction) {
//         match instr.dst {
//             InstrDst::Local(local) => {
//                 // TODO in postorder a local should never be referenced
//                 // before assignment so we can just take the hashmap here and avoid the clone
//                 if let Some(derivatives) = self.local_derivatives.get_mut(&local).map(|it|take(it)) {
//                     for (unkown, dst) in derivatives {
//                         self.build_derivative(
//                             dst.into(),
//                             instr.op,
//                             &instr.args,
//                             &instr.src,
//                             local.into(),
//                         )
//                     }
//                 }
//             }
//             InstrDst::Place(place) => {
//                 if let Some(derivatives) = self.place_derivatives.get(&place). {
//                     for (unkown, dst) in derivatives {
//                         self.build_derivative(
//                             dst.into(),
//                             instr.op,
//                             &instr.args,
//                             &instr.src,
//                             place.into(),
//                         )
//                     }
//                 }
//             }
//             InstrDst::Ignore => (),
//         };
//     }
// }
