use std::f64::consts::LN_2;
use std::mem::{replace, take};
use std::ops::Range;

use ahash::AHashMap;
use arena::{Arena, IdxRange};
use arrayvec::ArrayVec;
use cfg::{
    smallvec, BasicBlock, CfgBuilder, Const, ControlFlowGraph, InstIdx, InstrDst, Instruction,
    Local, Op, Operand, Operands, Phi, Place,
};
use indexmap::{IndexMap, IndexSet};
use program_dependence::def_use::Def;
use program_dependence::{AssigmentInterner, AssigmentLoc};
use stdx::iter::zip;

#[cfg(test)]
mod tests;

use crate::{FirstOrderUnkown, LiveDerivatives, Unkown, Unkowns};

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
    fn root_instr(pos: InstIdx) -> ResolvedDerivative {
        // The original instruction (so something the user typed) never has a cache and is always the first
        // instruction.
        ResolvedDerivative {
            instrs: InstIdx::from(u32::from(pos))..InstIdx::from(u32::from(pos) + 1),
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
    live_derivatives: &'a LiveDerivatives,
    interner: &'a AssigmentInterner,
    unkowns: &'a Unkowns,
    locals: AHashMap<(Local, FirstOrderUnkown), Local>,
    places: IndexSet<(Place, FirstOrderUnkown)>,
    zero: Local,

    // Kept in a struct so we can reuse the capacity
    delayed_writes: IndexMap<Local, Place, ahash::RandomState>,
    resolved_derivatives: AHashMap<Option<Unkown>, ResolvedDerivative>,
}

impl<'a> CfgTransform<'a> {
    pub(crate) fn new(
        cfg: &'a mut ControlFlowGraph,
        live_derivatives: &'a LiveDerivatives,
        unkowns: &'a Unkowns,
        pdg: &'a AssigmentInterner,
    ) -> Self {
        let mut cfg = cfg::CfgBuilder { current: cfg.entry(), cfg };
        let zero = cfg.build_val(Op::Copy, smallvec![0f64.into()], -1);
        CfgTransform {
            cfg,
            live_derivatives,
            interner: pdg,
            unkowns,
            locals: AHashMap::new(),
            places: IndexSet::new(),
            zero,
            delayed_writes: IndexMap::default(),
            resolved_derivatives: AHashMap::new(),
        }
    }

    pub fn run(&mut self) {
        // reverse postorder walk so we can assume locals to be initalized
        for bb in self.cfg.cfg.reverse_postorder() {
            self.cfg.current = bb;
            self.add_phis(bb);
            self.add_instructions(bb)
        }
    }

    fn add_phis(&mut self, bb: BasicBlock) {
        let mut res = Vec::new();
        let mut phis = take(&mut self.cfg.cfg[bb].phis);
        for phi in phis.iter() {
            if let Some(required_unkowns) = self.live_derivatives.local_derivatives.row(phi.dst) {
                for unkown in required_unkowns.iter() {
                    let prev_order = match self.unkowns.previous_order(unkown) {
                        Some(unknown) => self.derivative_local::<true>(phi.dst, unknown),
                        None => phi.dst,
                    };

                    let dst = self.cfg.cfg.new_local();
                    let old =
                        self.locals.insert((prev_order, self.unkowns.to_first_order(unkown)), dst);
                    debug_assert_eq!(old, None);

                    let sources = phi
                        .sources
                        .iter()
                        .map(|(bb, local)| (*bb, self.derivative_local::<false>(*local, unkown)))
                        .collect();

                    res.push(Phi { dst, sources })
                }
            }
        }
        phis.extend(res);
        self.cfg.cfg[bb].phis = phis
    }

    fn add_instructions(&mut self, bb: BasicBlock) {
        let mut cache_data = Arena::new();
        let mut derivative_cache = AHashMap::new();
        let old = take(&mut self.cfg.cfg.blocks[bb].instructions);
        self.cfg.cfg.blocks[bb].instructions.reserve(old.len());
        for (idx, instr) in old.into_iter().enumerate() {
            self.add_instr(instr, bb, idx.into(), &mut cache_data, &mut derivative_cache);
        }
    }

    fn add_instr(
        &mut self,
        mut instr: Instruction,
        bb: BasicBlock,
        idx: InstIdx,
        cache_data: &mut Arena<CacheData>,
        derivative_cache: &mut AHashMap<Option<Unkown>, CacheInfo>,
    ) {
        let dst = instr.dst;
        let (mut unkowns, def): (_, Def) = match instr.dst {
            InstrDst::Local(local) => {
                (self.live_derivatives.local_derivatives.row(local), local.into())
            }

            InstrDst::Place(_) => {
                let assigment = self
                    .interner
                    .assigment_locations
                    .unwrap_index(&AssigmentLoc { bb, instr: idx });
                (self.live_derivatives.assign_derivatives.row(assigment), assigment.into())
            }
            InstrDst::Ignore => {
                self.cfg.add_instr(instr);
                return;
            }
        };

        if let Some((ddx_unkown, relevant_unkowns)) = self.live_derivatives.ddx.get(&def) {
            instr.op = Op::Copy;
            unkowns = if relevant_unkowns.is_empty() { None } else { Some(relevant_unkowns) };
            instr.args[0] = self.operand_derivative(&instr.args[0], *ddx_unkown);
        }

        let is_self_referential = if let InstrDst::Place(place) = dst {
            unkowns.is_some() && instr.args.iter().any(|arg| arg == &Operand::Place(place))
        } else {
            false
        };

        // Derivatives get negative sources so that we can apply float some fast math (mostly float reassociation) to them (but not the
        // rest of the program)
        let src = -instr.src;

        if let Some(unkowns) = unkowns {
            // add the original instruction
            self.resolved_derivatives.insert(
                None,
                ResolvedDerivative::root_instr(self.cfg.current_block().instructions.len().into()),
            );
            self.cfg.add_instr(instr);

            for unkown in unkowns.iter() {
                let prev_order = self.unkowns.previous_order(unkown);
                let origin = self.resolved_derivatives[&prev_order].clone();
                let cache = self.ensure_cache(
                    prev_order,
                    &origin,
                    derivative_cache,
                    cache_data,
                    is_self_referential,
                    src,
                );

                let instr_start = self.cfg.current_block().instructions.len().into();

                let base = self.unkowns.to_first_order(unkown);

                for (instr, cache_data_i) in zip(origin.instructions(), cache.data.clone()) {
                    let Instruction { dst, op, ref mut args, .. } =
                        self.cfg.current_block_mut().instructions[instr];
                    let args = replace(args, Operands::new());
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

            // clean up data so we don't bleed state/avoid unnecessary large allocations
            cache_data.clear();
            derivative_cache.clear();
            self.resolved_derivatives.clear();
        } else {
            // Just add the instructions no derivatives required ;)
            self.cfg.add_instr(instr)
        }
    }

    fn derivative_local<const WRITE: bool>(&mut self, mut local: Local, unkown: Unkown) -> Local {
        for unkown in self.unkowns.first_order_unkowns_rev(unkown) {
            if let Some(derivative) = self.locals.get(&(local, unkown)) {
                local = *derivative
            } else if WRITE {
                unreachable!()
            } else {
                return self.zero;
            }
        }
        local
    }

    fn derivative_place_1(&mut self, place: Place, unkown: FirstOrderUnkown) -> Place {
        self.cfg.cfg.next_place + self.places.insert_full((place, unkown)).0
    }

    fn operand_derivative(&mut self, operand: &Operand, unkown: FirstOrderUnkown) -> Operand {
        match *operand {
            Operand::Const(_) => 0f64.into(),
            Operand::Local(local) => self
                .locals
                .get(&(local, unkown))
                // We walk the graph in reverse postorder so all locals must exists. Those that do
                // not have a derivative of zero (do not depend on any parameter that belong to an unkown)
                .map_or(Operand::Const(Const::Real(0f64)), |local| (*local).into()),
            Operand::Place(place) => self.derivative_place_1(place, unkown).into(),
            Operand::CfgParam(param) => self.unkowns.param_derivative(param, unkown).into(),
        }
    }

    fn derivative_dst(&mut self, dst: InstrDst, unkown: FirstOrderUnkown) -> InstrDst {
        match dst {
            InstrDst::Local(local) => match self.delayed_writes.get(&local).copied() {
                Some(place) => self.derivative_place_1(place, unkown).into(),
                None => {
                    let derivative = self.cfg.cfg.new_local();
                    let old = self.locals.insert((local, unkown), derivative);
                    debug_assert_eq!(old, None);
                    derivative.into()
                }
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
            self.cfg.build_assign(InstrDst::Place(place), Op::Copy, smallvec![local.into()], src)
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
                    let args = replace(args, Operands::new());

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
                let val = self.cfg.build_val(Op::IntMul, smallvec![args[1], args[1]], src);
                self.cfg.build_val(Op::IntToReal, smallvec![val.into()], src).into()
            }
            Op::RealDiv => self.cfg.build_val(Op::RealMul, smallvec![args[1], args[1]], src).into(),
            // Op::CmplxDiv => {
            //     self.cfg.build_val(Op::CmplxDiv, smallvec![args[1].clone(), args[1].clone()], src).into()
            // }

            // Technically not required but makes code look nicer..
            // exp(x) -> exp(x)
            Op::Exp => original,

            // hypot(x,y) -> (x' + y')/2hypot(x,y)
            // sqrt(x) -> 1/2sqrt(x)
            Op::Hypot | Op::Sqrt => {
                self.cfg.build_val(Op::RealMul, smallvec![2f64.into(), original], src).into()
            }
            // ln(x) -> 1/x
            Op::Ln => args[0],
            // log(x) -> log(e)/x
            Op::Log => self
                .cfg
                .build_val(Op::RealDiv, smallvec![std::f64::consts::LOG10_E.into(), args[0]], src)
                .into(),
            // sin(x) -> cos(x)
            Op::Sin => self.cfg.build_val(Op::Cos, smallvec![args[0]], src).into(),
            // cos(x) -> -sin(x)
            Op::Cos => {
                let sin = self.cfg.build_val(Op::Sin, smallvec![args[0]], src);
                self.cfg.build_val(Op::RealArtihNeg, smallvec![sin.into()], src).into()
            }
            // tan(x) -> 1 + tan^2(x)
            Op::Tan => {
                let tan_2 = self.cfg.build_val(Op::RealMul, smallvec![original, original], src);
                self.cfg.build_val(Op::RealAdd, smallvec![1f64.into(), tan_2.into()], src).into()
            }

            // asin(x) -> 1/sqrt(1-x^2)
            Op::ArcSin => {
                // 1 - x^2
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                let sqrt_arg = self.cfg.build_val(
                    Op::RealSub,
                    smallvec![1f64.into(), arg_squared.into()],
                    src,
                );

                // sqrt(1-x^2)
                self.cfg.build_val(Op::Sqrt, smallvec![sqrt_arg.into()], src).into()
            }
            // acos(x) -> -1/sqrt(1-x^2)
            Op::ArcCos => {
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                let sqrt_arg = self.cfg.build_val(
                    Op::RealSub,
                    smallvec![1f64.into(), arg_squared.into()],
                    src,
                );

                // sqrt(1-x^2)
                let sqrt = self.cfg.build_val(Op::Sqrt, smallvec![sqrt_arg.into()], src);
                self.cfg.build_val(Op::RealArtihNeg, smallvec![sqrt.into()], src).into()
            }
            // arctan(x) -> 1/(1 + x^2)
            Op::ArcTan => {
                // 1 + x^2
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                self.cfg
                    .build_val(Op::RealAdd, smallvec![1f64.into(), arg_squared.into()], src)
                    .into()
            }
            // arctan2(x,y) => (x'*y - y'*x)/(x^2+y^2)
            Op::ArcTan2 => {
                let lhs_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                let rhs_squared = self.cfg.build_val(Op::RealMul, smallvec![args[1], args[1]], src);

                let bot = self.cfg.build_val(
                    Op::RealAdd,
                    smallvec![lhs_squared.into(), rhs_squared.into()],
                    src,
                );

                let lhs_cache =
                    self.cfg.build_val(Op::RealDiv, smallvec![args[1], bot.into()], src);
                let rhs_cache =
                    self.cfg.build_val(Op::RealDiv, smallvec![args[0], bot.into()], src);
                res.push(lhs_cache.into());
                rhs_cache.into()
            }

            // sinh(x) -> cosh(x)
            Op::SinH => self.cfg.build_val(Op::CosH, smallvec![args[0]], src).into(),
            // cosh(x) -> sinh(x)
            Op::CosH => self.cfg.build_val(Op::SinH, smallvec![args[0]], src).into(),

            // tanh(x) -> 1 - tanh^2(x)
            Op::TanH => {
                let tan_2 = self.cfg.build_val(Op::RealMul, smallvec![original, original], src);
                self.cfg.build_val(Op::RealSub, smallvec![1f64.into(), tan_2.into()], src).into()
            }
            // acsinh(x) -> 1/sqrt(x^2 + 1)
            Op::ArcSinH => {
                // 1 + x^2
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                let sqrt_arg = self.cfg.build_val(
                    Op::RealAdd,
                    smallvec![1f64.into(), arg_squared.into()],
                    src,
                );

                // sqrt(1 + x^2)
                self.cfg.build_val(Op::Sqrt, smallvec![sqrt_arg.into()], src).into()
            }
            // acosh(x) -> 1/sqrt(x^2 - 1)
            Op::ArcCosH => {
                // x^2 - 1
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                let sqrt_arg = self.cfg.build_val(
                    Op::RealSub,
                    smallvec![arg_squared.into(), 1f64.into()],
                    src,
                );

                // sqrt(x^2 - 1)
                self.cfg.build_val(Op::Sqrt, smallvec![sqrt_arg.into()], src).into()
            }

            // arctanh(x) -> 1/(1-x^2)
            Op::ArcTanH => {
                // 1 - x^2
                let arg_squared = self.cfg.build_val(Op::RealMul, smallvec![args[0], args[0]], src);
                self.cfg
                    .build_val(Op::RealSub, smallvec![1f64.into(), arg_squared.into()], src)
                    .into()
            }

            // x << y = x*pow(2,y)-> ln(2) * x * y'* pow(2,y)  + x' * pow(2,y)
            // = ln(2) * y' * x<<y + x' * 1<<y
            Op::IntShl => {
                let original = self.cfg.build_val(Op::IntToReal, smallvec![original], src);
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, smallvec![LN_2.into(), original.into()], src);
                res.push(lhs_cache.into());

                let rhs_cache = self.cfg.build_val(Op::IntShl, smallvec![1.into(), args[1]], src);
                let rhs_cache = self.cfg.build_val(Op::IntToReal, smallvec![rhs_cache.into()], src);

                rhs_cache.into()
            }
            // x >> y = x*pow(2,-y)-> -ln(2) * x * y'* pow(2,-y)  + x' * pow(2,-y)
            // = -ln(2) * y' * x>>y + x' * 1>>y
            Op::IntShr => {
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, smallvec![(-LN_2).into(), original], src);
                let rhs_cache = self.cfg.build_val(Op::IntShl, smallvec![1.into(), args[1]], src);
                res.push(lhs_cache.into());
                rhs_cache.into()
            }

            // pow(x,y) -> (y/x * x' + ln(x) * y') * pow(x,y)
            Op::RealPow => {
                let y_x = self.cfg.build_val(Op::RealDiv, smallvec![args[1], args[0]], src);
                let lhs_cache =
                    self.cfg.build_val(Op::RealMul, smallvec![y_x.into(), original], src);

                let ln = self.cfg.build_val(Op::Ln, smallvec![args[0]], src);
                let rhs_cache =
                    self.cfg.build_val(Op::RealMul, smallvec![ln.into(), original], src);

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
                let lhs = sel.cfg.build_val(Op::IntToReal, smallvec![args[0]], src).into();
                let rhs = sel.cfg.build_val(Op::IntToReal, smallvec![args[1]], src).into();
                (lhs, rhs)
            } else {
                (args[0], args[1])
            };
            let drhs = arg_derivatrive(sel, 1);
            let dlhs = arg_derivatrive(sel, 0);
            let sum1 = sel.cfg.build_val(Op::RealMul, smallvec![lhs, drhs], src);
            let sum2 = sel.cfg.build_val(Op::RealMul, smallvec![dlhs, rhs], src);
            (add, smallvec![sum1.into(), sum2.into()])
        };

        // (f/g)' -> (f'*g - g' *f) / g^2 = f'/g - g'*f/g^2
        let gen_div_derivative = |sel: &mut CfgTransform, sub, mul, convert: bool| {
            let (lhs, rhs) = if convert {
                let lhs = sel.cfg.build_val(Op::IntToReal, smallvec![args[0]], src).into();
                let rhs = sel.cfg.build_val(Op::IntToReal, smallvec![args[1]], src).into();
                (lhs, rhs)
            } else {
                (args[0], args[1])
            };

            // f'/g
            let dlhs = arg_derivatrive(sel, 0);
            let sum1 = sel.cfg.build_val(Op::RealDiv, smallvec![dlhs, rhs], src);

            // f*g'/g^2
            let drhs = arg_derivatrive(sel, 1);
            let top = sel.cfg.build_val(mul, smallvec![lhs, drhs], src);
            let sum2 = sel.cfg.build_val(Op::RealDiv, smallvec![top.into(), cache[0]], src);

            (sub, smallvec![sum1.into(), sum2.into()])
        };

        let (op, args) = match op {
            Op::NoOp | Op::Call(_) => return, // TODO handle calls?
            Op::RealArtihNeg | Op::IntArithNeg
                => (Op::RealArtihNeg, smallvec![arg_derivatrive(self, 0)]),

            // All derivatives are REAL to all casts are essentially just copies
            Op::Copy | Op::IntToReal | Op::RealToInt | Op::BoolToInt | Op::IntToBool | Op::RealToBool | Op::BoolToReal => {
                (Op::Copy, smallvec![arg_derivatrive(self, 0)])
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
            | Op::BoolNeq => (Op::Copy, smallvec![0.0.into()]),

            Op::IntAdd
            | Op::RealAdd
                => (Op::RealAdd, smallvec![arg_derivatrive(self, 0), arg_derivatrive(self, 1)]),

            Op::IntSub
            | Op::RealSub
                => (Op::RealSub, smallvec![arg_derivatrive(self, 0), arg_derivatrive(self, 1)]),

            Op::IntMul => gen_mul_derivative(self, Op::RealAdd, true),
            Op::RealMul => gen_mul_derivative(self, Op::RealAdd, false),
            // Op::CmplxMul
                // => gen_mul_derivative(self, Op::CmplxPlus, false),

            // Op::CmplxDiv => gen_div_derivative(self, Op::CmplxMinus, Op::CmplxMul, false),
            Op::IntDiv => gen_div_derivative(self, Op::RealSub, Op::RealMul, true),
            Op::RealDiv => gen_div_derivative(self,  Op::RealSub, Op::RealMul, false),


            Op::Exp | Op::Log | Op::Sin | Op::Cos | Op::SinH | Op::CosH | Op::Tan | Op::TanH => {
                (Op::RealMul, smallvec![cache[0], arg_derivatrive(self, 0)])
            }
            Op::Ln
            | Op::Sqrt
            | Op::ArcSin
            | Op::ArcCos
            | Op::ArcTan
            | Op::ArcTan2
            | Op::ArcSinH
            | Op::ArcCosH
            | Op::ArcTanH => (Op::RealDiv, smallvec![arg_derivatrive(self, 0),cache[0],]),
             Op::RealPow | Op::IntShl | Op::IntShr => {
                 let dlhs = arg_derivatrive(self, 0);
                 let drhs = arg_derivatrive(self, 1);
                let lhs = self.cfg.build_val(Op::RealMul, smallvec![cache[0], dlhs], src);
                let rhs = self.cfg.build_val(Op::RealMul, smallvec![cache[1], drhs], src);
                (Op::RealAdd, smallvec![lhs.into(),rhs.into()])
            },
            Op::Hypot => {
                 let dlhs = arg_derivatrive(self, 0);
                 let drhs = arg_derivatrive(self, 1);
                 let sum = self.cfg.build_val(Op::RealAdd, smallvec![dlhs,drhs], src);
                 (Op::RealDiv, smallvec![sum.into(),cache[0]])
            }

        };

        self.cfg.build_assign(dst, op, args, src);
    }
}

pub type LocalDerivatives = AHashMap<(Local, FirstOrderUnkown), Local>;
pub type PlaceDerivatives = IndexSet<(Place, FirstOrderUnkown)>;

pub fn gen_derivatives(
    cfg: &mut ControlFlowGraph,
    live_derivatives: &LiveDerivatives,
    unkowns: &Unkowns,
    intern: &AssigmentInterner,
) -> (LocalDerivatives, PlaceDerivatives) {
    let mut transform = CfgTransform::new(cfg, live_derivatives, unkowns, intern);
    transform.run();
    let res = (transform.locals, transform.places);
    cfg.next_place += res.1.len();
    res
}
