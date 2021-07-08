/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use crate::frontend::{GeneralOsdiCall, GeneralOsdiInput, SimParamKind};
use crate::subfuncitons::automatic_slicing::TaintedLocations;
use crate::subfuncitons::load_functions::dc_load::DcLoadFunctionCall;
use crate::subfuncitons::load_functions::LoadFunctions;
use crate::{optimize_cfg, CircuitTopology};
use openvaf_data_structures::index_vec::{IndexSlice, IndexVec};
use openvaf_data_structures::{bit_set::BitSet, WorkQueue};
use openvaf_hir::{BranchId, NetId, Type, Unknown};
use openvaf_ir::ids::{PortId, SyntaxCtx};
use openvaf_ir::{PrintOnFinish, Spanned, StopTaskKind};
use openvaf_middle::cfg::{ControlFlowGraph, IntLocation, InternedLocations, LocationKind};
use openvaf_middle::derivatives::RValueAutoDiff;
use openvaf_middle::dfa::lattice::FlatSet;
use openvaf_middle::RValue::{Comparison, DoubleArgMath, SingleArgMath};
use openvaf_middle::{
    BinOp, COperand, COperandData, CallArg, CfgConversion, CfgFunctions, CfgInputs, ConstVal,
    Derivative, LocalDeclaration, LocalKind, Mir, Operand, OperandData, ParameterInput, RValue,
    SimpleConstVal, StmntKind,
};
use openvaf_pass::{
    program_dependence::{InvProgramDependenceGraph, ProgramDependenceGraph},
    BackwardSlice, Strip,
};
use openvaf_session::sourcemap::{Span, StringLiteral};
use std::collections::VecDeque;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::FromIterator;

use openvaf_data_structures::index_vec::index_vec;
use openvaf_session::sourcemap::span::DUMMY_SP;

#[derive(PartialEq, Eq, Clone)]
pub enum AcLoadFunctionCall {
    StopTask(StopTaskKind, PrintOnFinish),
}

impl CfgFunctions for AcLoadFunctionCall {
    type I = AcLoadInput;

    fn const_fold(&self, _call: &[FlatSet]) -> FlatSet {
        unreachable!()
    }

    fn derivative<C: CfgFunctions>(
        &self,
        _args: &IndexSlice<CallArg, [COperand<Self>]>,
        _ad: &mut RValueAutoDiff<Self, C>,
        _span: Span,
    ) -> Option<RValue<Self>> {
        unreachable!()
    }
}

impl Display for AcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // LoadFunctionCall::TimeDerivative => write!(f, "ddt"),
            AcLoadFunctionCall::StopTask(StopTaskKind::Stop, finish) => {
                write!(f, "$stop({:?})", finish)
            }
            AcLoadFunctionCall::StopTask(StopTaskKind::Finish, finish) => {
                write!(f, "$finish({:?})", finish)
            }
        }
    }
}

impl Debug for AcLoadFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OmegaKind {
    Real,
    Imaginary,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AcLoadInput {
    Parameter(ParameterInput),
    PortConnected(PortId),
    SimParam(StringLiteral, SimParamKind),
    Voltage(NetId, NetId),
    Current(BranchId),
    PortFlow(PortId),
    Omega(OmegaKind), // Analysis frequency
    Temperature,
}

impl Display for AcLoadInput {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(param) => Debug::fmt(param, f),
            Self::PortConnected(port) => write!(f, "$port_connected({:?})", port),
            Self::SimParam(name, kind) => write!(f, "$simparam({}, {:?})", name, kind),
            Self::Current(branch) => write!(f, "flow({:?})", branch),
            Self::Voltage(hi, lo) => write!(f, "pot({:?}, {:?})", hi, lo),
            Self::PortFlow(port) => write!(f, "flow({:?})", port),
            Self::Temperature => f.write_str("$temp"),
            Self::Omega(OmegaKind::Real) => write!(f, "$omega"),
            Self::Omega(OmegaKind::Imaginary) => write!(f, "1j * $omega"),
        }
    }
}

impl CfgInputs for AcLoadInput {
    fn derivative<C: CfgFunctions>(&self, _unknown: Unknown, _mir: &Mir<C>) -> Derivative<Self> {
        unimplemented!()
    }

    fn ty<C: CfgFunctions>(&self, mir: &Mir<C>) -> Type {
        match self {
            Self::Parameter(ParameterInput::Value(param)) => mir[*param].ty,
            Self::Voltage(_, _)
            | Self::Current(_)
            | Self::PortFlow(_)
            | Self::Temperature
            | Self::SimParam(_, SimParamKind::RealOptional)
            | Self::SimParam(_, SimParamKind::Real)
            | Self::Omega(OmegaKind::Real) => Type::REAL,

            Self::Parameter(ParameterInput::Given(_))
            | Self::PortConnected(_)
            | Self::SimParam(_, SimParamKind::RealOptionalGiven) => Type::BOOL,
            Self::SimParam(_, SimParamKind::String) => Type::STRING,

            Self::Omega(OmegaKind::Imaginary) => Type::CMPLX,
        }
    }
}

pub struct GeneralToAcLoad;

impl CfgConversion<GeneralOsdiCall, AcLoadFunctionCall> for GeneralToAcLoad {
    fn map_input(
        &mut self,
        src: <GeneralOsdiCall as CfgFunctions>::I,
    ) -> COperandData<AcLoadFunctionCall> {
        let res = match src {
            GeneralOsdiInput::Parameter(port) => AcLoadInput::Parameter(port),
            GeneralOsdiInput::PortConnected(conncted) => AcLoadInput::PortConnected(conncted),
            GeneralOsdiInput::SimParam(name, kind) => AcLoadInput::SimParam(name, kind),
            GeneralOsdiInput::Voltage(hi, lo) => AcLoadInput::Voltage(hi, lo),
            GeneralOsdiInput::Temperature => AcLoadInput::Temperature,
        };
        OperandData::Read(res)
    }

    fn map_call_val(
        &mut self,
        call: GeneralOsdiCall,
        mut args: IndexVec<CallArg, COperand<GeneralOsdiCall>>,
        span: Span,
    ) -> RValue<AcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::Noise => RValue::Use(Operand {
                span,
                contents: OperandData::Constant(ConstVal::Scalar(SimpleConstVal::Real(0.0))),
            }),
            GeneralOsdiCall::TimeDerivative => {
                // BLOCK allow generating additional statements here
                todo!("Time derivatives of something other than charges")
                // debug_assert_eq!(args.len(),1);
                // let omega = Operand{ span, contents: OperandData::Read(AcLoadInput::Omega(OmegaKind::Imaginary))};
                // let admittance = omega * args[0]
                // RValue::BinaryOperation(Spanned{ span, contents: BinOp::Multiply }, admittance, voltage)
            }
            GeneralOsdiCall::SymbolicDerivativeOfTimeDerivative => {
                let omega = Operand {
                    span,
                    contents: OperandData::Read(AcLoadInput::Omega(OmegaKind::Imaginary)),
                };

                debug_assert_eq!(args.len(), 1);
                RValue::BinaryOperation(
                    Spanned {
                        contents: BinOp::Multiply,
                        span,
                    },
                    omega,
                    self.map_operand(args.pop().unwrap()),
                )
            }

            GeneralOsdiCall::StopTask(_, _) | GeneralOsdiCall::NodeCollapse(_, _) => unreachable!(),
            GeneralOsdiCall::Lim { hi, lo, .. } => RValue::Use(Spanned {
                span,
                contents: OperandData::Read(AcLoadInput::Voltage(hi, lo)),
            }),
        }
    }

    fn map_call_stmnt(
        &mut self,
        call: GeneralOsdiCall,
        _args: IndexVec<CallArg, COperand<DcLoadFunctionCall>>,
        span: Span,
    ) -> StmntKind<AcLoadFunctionCall> {
        match call {
            GeneralOsdiCall::StopTask(kind, print) => StmntKind::Call(
                AcLoadFunctionCall::StopTask(kind, print),
                IndexVec::new(),
                span,
            ),
            GeneralOsdiCall::NodeCollapse(_, _) => StmntKind::NoOp,
            _ => unreachable!(),
        }
    }
}

impl LoadFunctions {
    pub fn gen_ac_load(
        cfg: &ControlFlowGraph<GeneralOsdiCall>,
        tainted: &TaintedLocations,
        assumed_locations: &BitSet<IntLocation>,
        locations: &InternedLocations,
        dc_load_output: BitSet<IntLocation>,
        pdg: &ProgramDependenceGraph,
        inv_pdg: &InvProgramDependenceGraph,
        topology: &CircuitTopology,
    ) -> ControlFlowGraph<AcLoadFunctionCall> {
        let mut ac_load = cfg.clone();

        let mut ac_assumed = dc_load_output;
        ac_assumed.union_with(assumed_locations);
        ac_assumed.difference_with(&tainted.by_time_derivative);
        ac_assumed.difference_with(&tainted.by_stamp_write);

        let ac_load_locations = ac_load.modify(BackwardSlice {
            relevant_locations: tainted.by_stamp_write.clone(),
            assumed_locations: ac_assumed,
            pdg,
            locations,
        });

        let mut ac_assumed = ac_load_locations.clone();
        //ac_assumed.union_with(&tainted.by_time_derivative);
        ac_assumed.toggle_all();

        // let mut rev_queue = WorkQueue {
        //     deque: VecDeque::new(),
        //     set: ac_assumed.clone(),
        // };

        let mut work_queue = WorkQueue {
            deque: VecDeque::from_iter(tainted.by_time_derivative.ones()),
            set: ac_assumed.clone(),
        };

        // Convert all stamps to cmplx
        for local in topology.matrix_stamp_locals.ones() {
            ac_load.locals[local].ty = Type::CMPLX
        }

        let mut new_locals = index_vec![None; cfg.locals.len()];

        // Make everything cmplx that is calculated from time derivatives (and is therefore potentially cmplx)
        while let Some(location) = work_queue.take() {
            let bb = locations[location].block;
            if let LocationKind::Statement(stmnt) = locations[location].kind {
                if let StmntKind::Assignment(ref mut dst, ref mut val) =
                    ac_load.blocks[bb].statements[stmnt].0
                {
                    if matches!(val, SingleArgMath(_, _) | DoubleArgMath(_, _, _)) {
                        todo!("Complex math")
                    }

                    if matches!(val, Comparison(_, _, _, _)) {
                        todo!("No error that complex comparisons are not defined")
                    }

                    debug_assert!(matches!(ac_load.locals[*dst].ty, Type::REAL | Type::CMPLX));

                    if matches!(ac_load.locals[*dst].kind, LocalKind::Variable(_, _)) {
                        let locals = &mut ac_load.locals;
                        *dst = *new_locals[*dst].get_or_insert_with(|| {
                            locals.push(LocalDeclaration {
                                kind: locals[*dst].kind.clone(),
                                ty: Type::CMPLX,
                            })
                        })
                    } else {
                        // Branch locals are already cmplx
                        // Temporaries are only written once we can simply change the type :)
                        ac_load.locals[*dst].ty = Type::CMPLX
                    }

                    if let RValue::Use(op) = val {
                        *val = RValue::Cast(op.clone())
                    }

                    if let Some(ref dependents) = inv_pdg.data_dependencies.def_use_chains[location]
                    {
                        work_queue.extend(dependents.ones())
                    }
                } else {
                    unreachable!()
                }
            } else {
                todo!("Nice errors for cmplx number comparisons (maybe even allow to skip SS)");
                todo!("Can phis with real values acctually occure? Far as I am aware they are only used for logical or/and so they are always integers")
            }
        }

        // Handle stamps without imaginary component (because analog filter may not appear conditinally we can assume a stamp is either always imaginary or never)
        work_queue.extend(tainted.by_stamp_write.ones());

        while let Some(location) = work_queue.take() {
            let bb = locations[location].block;
            if let LocationKind::Statement(stmnt) = locations[location].kind {
                if let StmntKind::Assignment(ref mut dst, ref mut val) =
                    ac_load.blocks[bb].statements[stmnt].0
                {
                    let locals = &mut ac_load.locals;
                    *dst = *new_locals[*dst].get_or_insert_with(|| {
                        locals.push(LocalDeclaration {
                            kind: locals[*dst].kind.clone(),
                            ty: Type::REAL,
                        })
                    });
                }
            } else {
                todo!("Nice errors for cmplx number comparisons (maybe even allow to skip SS)");
                todo!("Can phis with real values acctually occure? Far as I am aware they are only used for logical or/and so they are always integers")
            }
        }

        // Update statements that were converted to to use new variables
        for location in work_queue.set.difference(&ac_assumed) {
            let bb = locations[location].block;
            if let LocationKind::Statement(stmnt) = locations[location].kind {
                if let StmntKind::Assignment(_, ref mut val) =
                    ac_load.blocks[bb].statements[stmnt].0
                {
                    for local in val.locals_mut() {
                        if let Some(new_val) = new_locals[*local] {
                            *local = new_val
                        }
                    }
                }
            } else {
                todo!("Nice errors for cmplx number comparisons (maybe even allow to skip SS)");
                todo!("Can phis with real values acctually occure? Far as I am aware they are only used for logical or/and so they are always integers")
            }
        }

        ac_load.modify(Strip {
            retain: &ac_load_locations,
            locations,
        });

        let real_to_cmplx = topology.matrix_stamp_locals.ones().filter_map(|stamp| {
            new_locals[stamp].map(|real_stamp| {
                (
                    StmntKind::Assignment(
                        stamp,
                        RValue::Cast(Spanned {
                            span: DUMMY_SP,
                            contents: OperandData::Copy(real_stamp),
                        }),
                    ),
                    SyntaxCtx::ROOT,
                )
            })
        });
        let end = ac_load.end();
        ac_load.blocks[end].statements.extend(real_to_cmplx);

        let mut ac_load = ac_load.map(&mut GeneralToAcLoad);

        optimize_cfg(&mut ac_load);

        ac_load
    }
}
