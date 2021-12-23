use std::f64::consts::LN_2;
use std::mem::{replace, take};
use std::ops::Range;

use ahash::AHashMap;
use arena::{Arena, IdxRange};
use arrayvec::ArrayVec;
use bitset::GrowableSparseBitMatrix;
use cfg::{
    BasicBlock, BasicBlockData, CfgBuilder, Const, ControlFlowGraph, InstIdx, InstrDst,
    Instruction, Local, Op, Operand, Phi, PhiIdx, Place,
};
use data_flow::{Results, ResultsVisitor};
use indexmap::IndexMap;
use stdx::iter::zip;

#[cfg(test)]
mod tests;

use crate::{FirstOrderUnkown, LiveDerivativeAnalysis, Unkown};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
struct CacheInfo {
    instrs: Range<InstIdx>,
    data: IdxRange<CacheData>,
}

type CacheData = ArrayVec<Operand, 2>;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
struct ResolvedDerivative {
    instrs: Range<InstIdx>,
    cache_instrs: Range<InstIdx>,
}

impl ResolvedDerivative {
    fn root_instr() -> ResolvedDerivative {
        // The original instruction (so something the user typed) never has a cache and is always the first
        // instruction.
        ResolvedDerivative {
            instrs: InstIdx::from(0u32)..InstIdx::from(1u32),
            cache_instrs: InstIdx::from(0u32)..InstIdx::from(0u32),
        }
    }

    fn instructions(&self) -> impl Iterator<Item = InstIdx> {
        let instrs: Range<u32> = self.instrs.start.into()..self.instrs.end.into();
        let cache_instrs: Range<u32> = self.cache_instrs.start.into()..self.cache_instrs.end.into();
        cache_instrs.chain(instrs).map(InstIdx::from)
    }
}

pub(crate) struct CfgTransform<'a> {
    cfg: CfgBuilder<'a>,
    analysis: &'a LiveDerivativeAnalysis,
    locals: AHashMap<(Local, Unkown), Local>,
    places: AHashMap<(Place, Unkown), Place>,

    // Kept in a struct so we can reuse the capacity
    delayed_writes: IndexMap<Local, Place, ahash::RandomState>,
    cache_data: Arena<CacheData>,
    derivative_cache: AHashMap<Option<Unkown>, CacheInfo>,
    resolved_derivatives: AHashMap<Option<Unkown>, ResolvedDerivative>,
}

impl<'a> CfgTransform<'a> {
    pub(crate) fn new(
        new_cfg: &'a mut ControlFlowGraph,
        analysis: &'a LiveDerivativeAnalysis,
    ) -> Self {
        CfgTransform {
            cfg: cfg::CfgBuilder { cfg: new_cfg, current: 0u32.into() },
            analysis,
            locals: AHashMap::new(),
            places: AHashMap::new(),
            delayed_writes: IndexMap::default(),
            cache_data: Arena::new(),
            derivative_cache: AHashMap::new(),
            resolved_derivatives: AHashMap::new(),
        }
    }
}

impl ResultsVisitor for CfgTransform<'_> {
    type FlowState = GrowableSparseBitMatrix<Place, Unkown>;

    fn visit_block_end(
        &mut self,
        _state: &Self::FlowState,
        _block_data: &BasicBlockData,
        block: BasicBlock,
    ) {
        // The control flow remains unchanged so we always
        // stay in the same block as the original
        self.cfg.current = block;
    }

    fn visit_block_start(
        &mut self,
        _state: &Self::FlowState,
        block_data: &BasicBlockData,
        _block: BasicBlock,
    ) {
        self.cfg.current_block_mut().instructions.reverse();
        if let Some(terminator) = block_data.terminator.clone() {
            self.cfg.terminate(terminator)
        }
    }

    fn visit_phi_before_effect(
        &mut self,
        _state: &Self::FlowState,
        phi: &Phi,
        _block: BasicBlock,
        _id: PhiIdx,
    ) {
        if let Some(required_unkowns) =
            self.analysis.live_local_derivatives.borrow().matrix.row(phi.dst)
        {
            for unkown in required_unkowns.iter() {
                let dst = self.derivative_local(phi.dst, unkown);
                let sources = phi
                    .sources
                    .iter()
                    .map(|(bb, local)| (*bb, self.derivative_local(*local, unkown)))
                    .collect();

                self.cfg.add_phi(dst, sources);
            }
        }

        self.cfg.add_phi(phi.dst, phi.sources.clone());
    }

    #[inline(always)]
    fn visit_instruction_before_effect(
        &mut self,
        state: &Self::FlowState,
        instr_src: &Instruction,
        _block: BasicBlock,
        _id: InstIdx,
    ) {
        let borrow;

        let dst = instr_src.dst;

        let unkowns = match instr_src.dst {
            InstrDst::Local(local) => {
                borrow = self.analysis.live_local_derivatives.borrow();
                borrow.matrix.row(local)
            }

            InstrDst::Place(place) => state.matrix.row(place),
            InstrDst::Ignore => None,
        };

        let is_self_referential = if let InstrDst::Place(place) = dst {
            unkowns.is_some() && instr_src.args.iter().any(|arg| arg == &Operand::Place(place))
        } else {
            false
        };

        let mut instr = instr_src.clone();
        if let Op::Call(call) = instr.op {
            if let Some(ddx_unkown) = self.analysis.unkowns.callback_unkown(call) {
                instr.op = Op::Copy;
                instr.args[0] = self.operand_derivative(&instr.args[0], ddx_unkown);
            }
        }

        if let Some(unkowns) = unkowns {
            // Derivatives are generated in forward direction but globally speaking we generate
            // backwards -> create all derivatives in their own block -> reverse them after and
            // append to the rest
            let instructions = take(&mut self.cfg.current_block_mut().instructions);

            // add the original instruction
            self.cfg.add_instr(instr);
            self.resolved_derivatives.insert(None, ResolvedDerivative::root_instr());

            // make the borrow checker happy
            let mut cache_data = take(&mut self.cache_data);
            let mut derivative_cache = take(&mut self.derivative_cache);

            // Derivatives get negative sources so that we can apply float some fast math (mostly float reassociation) to them (but not the
            // rest of the program)
            let src = -instr_src.src;

            for unkown in unkowns.iter() {
                let prev_order = self.analysis.unkowns.previous_order(unkown);
                let origin = self.resolved_derivatives[&prev_order].clone();
                let cache = self.ensure_cache(
                    prev_order,
                    &origin,
                    &mut derivative_cache,
                    &mut cache_data,
                    is_self_referential,
                    src,
                );

                let instr_start = self.cfg.current_block().instructions.len().into();

                // let val_unkown = match ddx_unkown {
                //     Some(next_unkown) => self.analysis.unkowns.raise_order(unkown, next_unkown),
                //     None => unkown,
                // };

                let base = self.analysis.unkowns.to_first_order(unkown);

                for (instr, cache_data_i) in zip(origin.instructions(), cache.data.clone()) {
                    let Instruction { dst, op, ref mut args, .. } =
                        self.cfg.current_block_mut().instructions[instr];
                    let args = replace(args, vec![].into_boxed_slice());
                    let dst = self.derivative_dst(dst, base);

                    self.instr_derivative(dst, op, &args, base, src, &cache_data[cache_data_i]);

                    self.cfg.current_block_mut().instructions[instr].args = args;
                }

                let instr_end = self.cfg.current_block().instructions.len().into();

                self.resolved_derivatives.insert(
                    Some(unkown),
                    ResolvedDerivative {
                        instrs: instr_start..instr_end,
                        cache_instrs: cache.instrs.clone(),
                    },
                );
            }

            self.build_delayed_assign(src);

            // reverse derivative, append to all instructions of the current block and restore full
            // block to cfg
            let mut instructions =
                replace(&mut self.cfg.current_block_mut().instructions, instructions);
            instructions.reverse();
            self.cfg.current_block_mut().instructions.append(&mut instructions);

            // make the borrow checker happy
            self.cache_data = cache_data;
            self.derivative_cache = derivative_cache;

            // clean up data so we don't bleed state/avoid unnecessary large allocations
            self.cache_data.clear();
            self.derivative_cache.clear();
            self.resolved_derivatives.clear();
        } else {
            // Just add the instructions no derivatives required ;)
            self.cfg.add_instr(instr)
        }
    }
}

impl CfgTransform<'_> {
    fn derivative_local(&mut self, mut local: Local, unkown: Unkown) -> Local {
        // TODO benchmark: is this worht it
        if let Some(local) = self.locals.get(&(local, unkown)) {
            return *local;
        }
        for unkown in self.analysis.unkowns.first_order_unkowns_rev(unkown) {
            local = self.derivative_local_1(local, unkown);
        }
        local
    }

    fn derivative_local_1(&mut self, local: Local, unkown: FirstOrderUnkown) -> Local {
        *self.locals.entry((local, unkown.into())).or_insert_with(|| self.cfg.cfg.new_local())
    }

    fn derivative_place_1(&mut self, place: Place, unkown: FirstOrderUnkown) -> Place {
        *self.places.entry((place, unkown.into())).or_insert_with(|| self.cfg.cfg.new_place())
    }

    fn operand_derivative(&mut self, operand: &Operand, unkown: FirstOrderUnkown) -> Operand {
        match *operand {
            Operand::Const(_) => 0f64.into(),
            Operand::Local(local) => self.derivative_local_1(local, unkown).into(),
            Operand::Place(place) => self.derivative_place_1(place, unkown).into(),
            Operand::CfgParam(param) => {
                self.analysis.unkowns.param_derivative(param, unkown).into()
            }
        }
    }

    fn derivative_dst(&mut self, dst: InstrDst, unkown: FirstOrderUnkown) -> InstrDst {
        match dst {
            InstrDst::Local(local) => match self.delayed_writes.get(&local).copied() {
                Some(place) => self.derivative_place_1(place, unkown).into(),
                None => self.derivative_local_1(local, unkown).into(),
            },
            InstrDst::Place(place) => self.derivative_place_1(place, unkown).into(),
            InstrDst::Ignore => InstrDst::Ignore,
        }
    }

    /// For efficiency reasons derivatives reuse the calculated value of the original instruction.
    /// Therefore derivatives are generated after the original instruction.
    /// In the edge case that a Statement is self-referential (x = f(x)) this causes incorrect
    /// behaviour as the operands used during the derivative may still depend on the original
    /// derivative (the same problem also occurs for higher order derivatives).
    ///
    /// As a solution all write into places (which are only generated for the original
    /// instructions and its final derivatives) are replaced with writes in locals.
    /// After all derivatives are generated these locals are then written into these places.
    ///
    /// This function generates these assignments
    fn build_delayed_assign(&mut self, src: i32) {
        for (local, place) in self.delayed_writes.drain(..) {
            self.cfg.build_assign(InstrDst::Place(place), Op::Copy, vec![local.into()], src)
        }
    }

    fn ensure_cache(
        &mut self,
        prev_order: Option<Unkown>,
        prev_order_instr: &ResolvedDerivative,
        derivative_cache: &mut AHashMap<Option<Unkown>, CacheInfo>,
        cache_data: &mut Arena<CacheData>,
        is_self_referential: bool,
        src: i32,
    ) -> CacheInfo {
        derivative_cache
            .entry(prev_order)
            .or_insert_with(|| {
                let instr_start = self.cfg.current_block().instructions.len().into();
                let data_start = cache_data.len().into();

                let new_cache_data = prev_order_instr.instructions().map(|instr| {
                    let Instruction { dst, op, ref mut args, .. } =
                        self.cfg.current_block_mut().instructions[instr];

                    // make the borrow checker happy
                    let args = replace(args, vec![].into_boxed_slice());

                    // determine origin
                    let original = match dst {
                        InstrDst::Place(place) if is_self_referential => {
                            // delay write for self referential assign
                            // see `Self::build_delayed_assign`
                            let local = self.cfg.cfg.new_local();
                            self.delayed_writes.insert(local, place);
                            self.cfg.current_block_mut().instructions[instr].dst = local.into();
                            local.into()
                        }
                        InstrDst::Place(place) => place.into(),
                        InstrDst::Ignore => Operand::Const(Const::Zst),
                        InstrDst::Local(local) => local.into(),
                    };

                    let cache = self.instr_cache(op, &args, original, src);

                    // restore arguments
                    self.cfg.current_block_mut().instructions[instr].args = args;

                    cache
                });
                cache_data.extend(new_cache_data);

                let instr_end = self.cfg.current_block().instructions.len().into();
                let data_end = cache_data.len().into();

                CacheInfo {
                    instrs: instr_start..instr_end,
                    data: IdxRange::new(data_start..data_end),
                }
            })
            .to_owned()
    }

    fn instr_cache(&mut self, op: Op, args: &[Operand], original: Operand, src: i32) -> CacheData {
        let mut res = ArrayVec::new();
        let op = match op {
            Op::IntDiv => {
                let val =
                    self.cfg.build_val(Op::IntMul, vec![args[1].clone(), args[1].clone()], src);
                self.cfg.build_val(Op::IntToReal, vec![val.into()], src).into()
            }
            Op::RealDiv => {
                self.cfg.build_val(Op::RealMul, vec![args[1].clone(), args[1].clone()], src).into()
            }
            // Op::CmplxDiv => {
            //     self.cfg.build_val(Op::CmplxDiv, vec![args[1].clone(), args[1].clone()], src).into()
            // }

            // Technically not required but makes code look nicer..
            // exp(x) -> exp(x)
            Op::Exp => original,

            // sqrt(x) -> 1/2sqrt(x)
            Op::Sqrt => self.cfg.build_val(Op::RealMul, vec![2f64.into(), original], src).into(),
            // ln(x) -> 1/x
            Op::Ln => original,
            // log(x) -> log(e)/x
            Op::Log => self
                .cfg
                .build_val(
                    Op::RealDiv,
                    vec![std::f64::consts::LOG10_E.into(), args[0].clone()],
                    src,
                )
                .into(),
            // sin(x) -> cos(x)
            Op::Sin => self.cfg.build_val(Op::Cos, vec![args[0].clone()], src).into(),
            // cos(x) -> -sin(x)
            Op::Cos => {
                let sin = self.cfg.build_val(Op::Cos, vec![args[0].clone()], src);
                self.cfg.build_val(Op::RealArtihNeg, vec![sin.into()], src).into()
            }
            // tan(x) -> 1 + tan^2(x)
            Op::Tan => {
                let tan_2 = self.cfg.build_val(Op::RealMul, vec![original.clone(), original], src);
                self.cfg.build_val(Op::RealAdd, vec![1f64.into(), tan_2.into()], src).into()
            }

            // hypot(x,y) -> (x' + y')/2hypot(x,y)
            Op::Hypot => self.cfg.build_val(Op::RealMul, vec![2f64.into(), original], src).into(),
            // asin(x) -> 1/sqrt(1-x^2)
            Op::ArcSin => {
                // 1 - x^2
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                let sqrt_arg =
                    self.cfg.build_val(Op::RealSub, vec![1f64.into(), arg_squared.into()], src);

                // sqrt(1-x^2)
                self.cfg.build_val(Op::Sqrt, vec![sqrt_arg.into()], src).into()
            }
            // acos(x) -> -1/sqrt(1-x^2)
            Op::ArcCos => {
                // 1 - x^2
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                let sqrt_arg =
                    self.cfg.build_val(Op::RealSub, vec![arg_squared.into(), 1f64.into()], src);

                // sqrt(1-x^2)
                let sqrt = self.cfg.build_val(Op::Sqrt, vec![sqrt_arg.into()], src);
                self.cfg.build_val(Op::RealArtihNeg, vec![sqrt.into()], src).into()
            }
            // arctan(x) -> 1/(1 + x^2)
            Op::ArcTan => {
                // 1 + x^2
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                self.cfg.build_val(Op::RealAdd, vec![1f64.into(), arg_squared.into()], src).into()
            }
            // arctan2(x,y) => (x'*y - y'*x)/(x^2+y^2)
            Op::ArcTan2 => {
                let lhs_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                let rhs_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[1].clone(), args[1].clone()], src);

                let bot = self.cfg.build_val(
                    Op::RealAdd,
                    vec![lhs_squared.into(), rhs_squared.into()],
                    src,
                );

                let lhs_cache =
                    self.cfg.build_val(Op::RealDiv, vec![args[1].clone(), bot.into()], src);
                let rhs_cache =
                    self.cfg.build_val(Op::RealDiv, vec![args[0].clone(), bot.into()], src);
                res.push(lhs_cache.into());
                rhs_cache.into()
            }

            // sinh(x) -> cosh(x)
            Op::SinH => self.cfg.build_val(Op::CosH, vec![args[0].clone()], src).into(),
            // cosh(x) -> sinh(x)
            Op::CosH => self.cfg.build_val(Op::SinH, vec![args[0].clone()], src).into(),

            // tanh(x) -> 1 - tanh^2(x)
            Op::TanH => {
                let tan_2 = self.cfg.build_val(Op::RealMul, vec![original.clone(), original], src);
                self.cfg.build_val(Op::RealSub, vec![1f64.into(), tan_2.into()], src).into()
            }
            Op::ArcSinH => {
                // 1 + x^2
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                let sqrt_arg =
                    self.cfg.build_val(Op::RealAdd, vec![1f64.into(), arg_squared.into()], src);

                // sqrt(1 + x^2)
                self.cfg.build_val(Op::Sqrt, vec![sqrt_arg.into()], src).into()
            }
            // acosh(x) -> 1/sqrt(x^2 - 1)
            Op::ArcCosH => {
                // x^2 - 1
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                let sqrt_arg =
                    self.cfg.build_val(Op::RealSub, vec![arg_squared.into(), 1f64.into()], src);

                // sqrt(x^2 - 1)
                self.cfg.build_val(Op::Sqrt, vec![sqrt_arg.into()], src).into()
            }

            // arctanh(x) -> 1/(1-x^2)
            Op::ArcTanH => {
                // 1 - x^2
                let arg_squared =
                    self.cfg.build_val(Op::RealMul, vec![args[0].clone(), args[0].clone()], src);
                self.cfg.build_val(Op::RealSub, vec![1f64.into(), arg_squared.into()], src).into()
            }

            // x << y = x*pow(2,y)-> ln(2) * x * y'* pow(2,y)  + x' * pow(2,y)
            // = ln(2) * y' * x<<y + x' * 1<<y
            Op::IntShl => {
                let original = self.cfg.build_val(Op::IntToReal, vec![original], src);
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, vec![LN_2.into(), original.into()], src);
                res.push(lhs_cache.into());

                let rhs_cache =
                    self.cfg.build_val(Op::IntShl, vec![1.into(), args[1].clone()], src);
                let rhs_cache = self.cfg.build_val(Op::IntToReal, vec![rhs_cache.into()], src);

                rhs_cache.into()
            }
            // x >> y = x*pow(2,-y)-> -ln(2) * x * y'* pow(2,-y)  + x' * pow(2,-y)
            // = -ln(2) * y' * x>>y + x' * 1>>y
            Op::IntShr => {
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, vec![(-LN_2).into(), original], src);
                let rhs_cache =
                    self.cfg.build_val(Op::IntShl, vec![1.into(), args[1].clone()], src);
                res.push(lhs_cache.into());
                rhs_cache.into()
            }

            // pow(x,y) -> (y/x * x' + ln(x) * y') * pow(x,y)
            Op::RealPow => {
                let y_x =
                    self.cfg.build_val(Op::RealDiv, vec![args[1].clone(), args[0].clone()], src);
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, vec![y_x.into(), original.clone()], src);

                let ln = self.cfg.build_val(Op::Ln, vec![args[0].clone()], src);
                let rhs_cache = self.cfg.build_val(Op::RealMul, vec![ln.into(), original], src);

                res.push(lhs_cache.into());
                rhs_cache.into()
            }
            _ => return res,
        };

        res.push(op);

        res
    }

    fn instr_derivative(
        &mut self,
        dst: InstrDst,
        op: Op,
        args: &[Operand],
        unkown: FirstOrderUnkown,
        src: i32,
        cache: &CacheData,
    ) {
        let arg_derivatrive = |sel: &mut CfgTransform, i| sel.operand_derivative(&args[i], unkown);

        let gen_mul_derivative = |sel: &mut CfgTransform, add, convert: bool| {
            let (lhs, rhs) = if convert {
                let lhs = sel.cfg.build_val(Op::IntToReal, vec![args[0].clone()], src).into();
                let rhs = sel.cfg.build_val(Op::IntToReal, vec![args[1].clone()], src).into();
                (lhs, rhs)
            } else {
                (args[0].clone(), args[1].clone())
            };
            let drhs = arg_derivatrive(sel, 1);
            let dlhs = arg_derivatrive(sel, 0);
            let sum1 = sel.cfg.build_val(op, vec![lhs, drhs], src);
            let sum2 = sel.cfg.build_val(op, vec![dlhs, rhs], src);
            (add, vec![sum1.into(), sum2.into()])
        };

        // (f/g)' -> (f'*g - g' *f) / g^2 = f'/g - g'*f/g^2
        let gen_div_derivative = |sel: &mut CfgTransform, sub, mul, convert: bool| {
            let (lhs, rhs) = if convert {
                let lhs = sel.cfg.build_val(Op::IntToReal, vec![args[0].clone()], src).into();
                let rhs = sel.cfg.build_val(Op::IntToReal, vec![args[1].clone()], src).into();
                (lhs, rhs)
            } else {
                (args[0].clone(), args[1].clone())
            };

            // f'/g
            let dlhs = arg_derivatrive(sel, 0);
            let sum1 = sel.cfg.build_val(op, vec![dlhs, rhs], src);

            // f*g'/g^2
            let drhs = arg_derivatrive(sel, 1);
            let top = sel.cfg.build_val(mul, vec![lhs, drhs], src);
            let sum2 = sel.cfg.build_val(op, vec![top.into(), cache[0].clone()], src);

            (sub, vec![sum1.into(), sum2.into()])
        };

        let (op, args) = match op {
            Op::NoOp => return,
            Op::Call(_) => return, // TODO handle calls?
            
            Op::RealArtihNeg
                => (Op::RealArtihNeg, vec![arg_derivatrive(self, 0)]),

            Op::IntArithNeg => (Op::RealArtihNeg, vec![arg_derivatrive(self, 0)]),

            // All derivatives are REAL to all casts are essentially just copies
            Op::Copy | Op::IntToReal | Op::RealToInt | Op::BoolToInt | Op::IntToBool | Op::BoolToReal => {
                (Op::Copy, vec![arg_derivatrive(self, 0)])
            }

            // TODO error?
            Op::IntRem
            // TODO 0 is technically the correct thing
            // but intuetively the other thing is correct
            // TODO discuss with committee what to do with integer derivatives
            // | Op::RealToInt 
            | Op::IntBitNegate
            | Op::IntXor
            | Op::IntNXor
            | Op::IntAnd
            | Op::IntOr
            | Op::Clog2
            | Op::RealRem
            | Op::Floor
            | Op::Ceil
            | Op::BoolBitNegate
            | Op::IntLessThen
            | Op::IntGreaterThen
            | Op::RealLessThen
            | Op::RealGreaterThen
            | Op::IntLessEqual
            | Op::IntGreaterEqual
            | Op::RealLessEqual
            | Op::RealGreaterEqual
            | Op::IntEq
            | Op::RealEq
            | Op::StringEq
            | Op::BoolEq
            | Op::IntNeq
            | Op::RealNeq
            | Op::StringNeq
            | Op::BoolNeq => (Op::Copy, vec![0.0.into()]),

            Op::IntAdd
            | Op::IntSub
            | Op::RealAdd
            | Op::RealSub
            // | Op::CmplxPlus
            // | Op::CmplxMinus
                => (op, vec![arg_derivatrive(self, 0), arg_derivatrive(self, 1)]),

            Op::IntMul => gen_mul_derivative(self, Op::RealAdd, true),
            Op::RealMul => gen_mul_derivative(self, Op::RealAdd, false),
            // Op::CmplxMul
                // => gen_mul_derivative(self, Op::CmplxPlus, false),

            // Op::CmplxDiv => gen_div_derivative(self, Op::CmplxMinus, Op::CmplxMul, false),
            Op::IntDiv => gen_div_derivative(self, Op::RealSub, Op::IntMul, true),
            Op::RealDiv => gen_div_derivative(self,  Op::RealSub, Op::RealMul, false),


            Op::Exp | Op::Log | Op::Sin | Op::Cos | Op::SinH | Op::CosH | Op::Tan | Op::TanH => {
                (Op::RealMul, vec![cache[0].clone(), arg_derivatrive(self, 0)])
            }
            Op::Ln
            | Op::Sqrt
            | Op::ArcSin
            | Op::ArcCos
            | Op::ArcTan
            | Op::ArcTan2
            | Op::ArcSinH
            | Op::ArcCosH
            | Op::ArcTanH => (Op::RealDiv, vec![cache[0].clone(), arg_derivatrive(self, 0)]),
             Op::RealPow | Op::IntShl | Op::IntShr => {
                 let dlhs = arg_derivatrive(self, 0);
                 let drhs = arg_derivatrive(self, 1);
                let lhs = self.cfg.build_val(Op::RealMul, vec![cache[0].clone(), dlhs], src);
                let rhs = self.cfg.build_val(Op::RealMul, vec![cache[1].clone(), drhs], src);
                (Op::RealAdd, vec![lhs.into(),rhs.into()])
            },
            Op::Hypot => todo!(),

        };

        self.cfg.build_assign(dst, op, args, src);
    }
}

pub type LocalDerivatives = AHashMap<(Local, Unkown), Local>;
pub type PlaceDerivatives = AHashMap<(Place, Unkown), Place>;

pub fn gen_derivatives(
    cfg: &mut ControlFlowGraph,
    live_derivatives: &Results<LiveDerivativeAnalysis>,
) -> (LocalDerivatives, PlaceDerivatives) {
    let mut new_cfg = ControlFlowGraph {
        next_local: cfg.next_local,
        next_place: cfg.next_place,
        // This transformation does not create any new branches so the amount of blocks and their
        // predecessors will stay the same
        blocks: vec![BasicBlockData::default(); cfg.blocks.len()].into(),
        predecessor_cache: take(&mut cfg.predecessor_cache),
        is_cyclic: take(&mut cfg.is_cyclic),
    };

    let mut transform = CfgTransform::new(&mut new_cfg, &live_derivatives.analysis);
    live_derivatives.visit_with(cfg, &mut transform);
    let res = (transform.locals, transform.places);
    *cfg = new_cfg;
    res
}
