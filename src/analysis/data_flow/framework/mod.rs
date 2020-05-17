//  * ******************************************************************************************
//  * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
//  * It is subject to the license terms in the LICENSE file found in the top-level directory
//  *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
//  *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
//  *  distributed except according to the terms contained in the LICENSE file.
//  * *******************************************************************************************

use crate::ir::mir::control_flow_graph::BasicBlockId;
use crate::ir::mir::ControlFlowGraph;
use crate::ir::{StatementId, VariableId};

pub use engine::BackwardEngine;
pub use engine::DirtyList;
pub use engine::Engine;
pub use engine::ForwardEngine;
pub use graph::Graph as DataFlowGraph;

use fixedbitset::FixedBitSet as BitSet;

mod engine;
mod graph;

use crate::compact_arena::{invariant_lifetime, TinyHeapArena};

pub trait GenKillAnalysis<'engine, 'cfg: 'engine, 'mir: 'engine>: Sized + 'engine {
    type Engine: Engine<'engine, 'cfg, 'mir, GenKillEngine<'engine, 'cfg, Self>>;
    type EngineArgs;

    fn transfer_function(
        &mut self,
        gen_kill_set: &mut GenKillSet,
        basic_bock: BasicBlockId<'cfg>,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    );

    fn set_size(&self) -> usize;

    fn engine_builder(
        &'engine mut self,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    ) -> GenKillEngine<'engine, 'cfg, Self> {
        GenKillEngine::new(cfg, self)
    }

    fn engine(
        analysis: &'engine mut GenKillEngine<'engine, 'cfg, Self>,
        cfg: &'engine mut ControlFlowGraph<'cfg, 'mir>,
        args: <Self::Engine as Engine<'engine, 'cfg, 'mir, GenKillEngine<'engine, 'cfg, Self>>>::Args,
        additional_args: Self::EngineArgs,
    ) -> Self::Engine;
}

pub trait Analysis<'engine, 'cfg: 'engine, 'mir: 'engine>: Sized + 'engine {
    type Engine: Engine<'engine, 'cfg, 'mir, Self>;
    type EngineArgs;

    fn transfer_function(
        &mut self,
        in_set: &BitSet,
        out_set: &mut BitSet,
        basic_bock: BasicBlockId<'cfg>,
        cfg: &mut ControlFlowGraph<'cfg, 'mir>,
    );

    fn join(&mut self, inout_set: &mut BitSet, in_set: &BitSet) {
        inout_set.union_with(in_set);
    }

    fn set_size(&self) -> usize;

    fn engine(
        &'engine mut self,
        cfg: &'engine mut ControlFlowGraph<'cfg, 'mir>,
        args: <Self::Engine as Engine<'engine, 'cfg, 'mir, Self>>::Args,
        additional_args: Self::EngineArgs,
    ) -> Self::Engine;
}

pub struct GenKillEngine<'lt, 'cfg, A> {
    analysis: &'lt mut A,
    pub transfer_functions: TinyHeapArena<'cfg, GenKillSet>,
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: GenKillAnalysis<'lt, 'cfg, 'mir>> GenKillEngine<'lt, 'cfg, A> {
    pub fn new(cfg: &mut ControlFlowGraph<'cfg, 'mir>, analysis: &'lt mut A) -> Self {
        let gen_kill_set = GenKillSet::new(analysis.set_size());
        let mut transfer_functions = unsafe {
            TinyHeapArena::new_with(invariant_lifetime(), cfg.block_count(), || {
                gen_kill_set.clone()
            })
        };

        cfg.for_all_blocks_mut(|cfg, block| {
            analysis.transfer_function(&mut transfer_functions[block], block, cfg)
        });

        Self {
            analysis,
            transfer_functions,
        }
    }
}

impl<'lt, 'cfg: 'lt, 'mir: 'lt, A: GenKillAnalysis<'lt, 'cfg, 'mir>> Analysis<'lt, 'cfg, 'mir>
    for GenKillEngine<'lt, 'cfg, A>
{
    type Engine = A::Engine;
    type EngineArgs = A::EngineArgs;

    fn transfer_function(
        &mut self,
        in_set: &BitSet,
        out_set: &mut BitSet,
        basic_bock: BasicBlockId<'cfg>,
        _: &mut ControlFlowGraph<'cfg, 'mir>,
    ) {
        out_set.clear();
        out_set.union_with(in_set);
        out_set.difference_with(&self.transfer_functions[basic_bock].kill);
        out_set.union_with(&self.transfer_functions[basic_bock].gen);
    }

    fn set_size(&self) -> usize {
        self.analysis.set_size()
    }

    fn engine(
        &'lt mut self,
        cfg: &'lt mut ControlFlowGraph<'cfg, 'mir>,
        args: <A::Engine as Engine<'lt, 'cfg, 'mir, GenKillEngine<'lt, 'cfg, A>>>::Args,
        additional_args: A::EngineArgs,
    ) -> A::Engine {
        A::engine(self, cfg, args, additional_args)
    }
}

#[derive(Clone)]
pub struct GenKillSet {
    gen: BitSet,
    kill: BitSet,
}

impl GenKillSet {
    #[inline]
    pub fn new(set_size: usize) -> Self {
        let gen = BitSet::with_capacity(set_size);
        Self {
            kill: gen.clone(),
            gen,
        }
    }

    #[inline]
    pub fn gen(&mut self, x: usize) {
        self.gen.insert(x);
        self.kill.set(x, false);
    }

    #[inline]
    pub fn kill(&mut self, x: usize) {
        self.kill.insert(x);
        self.gen.set(x, false);
    }

    #[inline]
    pub fn gen_variable(&mut self, x: VariableId) {
        self.gen(x.unwrap().index() as usize)
    }

    #[inline]
    pub fn kill_variable(&mut self, x: VariableId) {
        self.kill(x.unwrap().index() as usize)
    }

    #[inline]
    pub fn gen_statement(&mut self, x: StatementId) {
        self.gen(x.unwrap().index() as usize)
    }

    #[inline]
    pub fn kill_statement(&mut self, x: StatementId) {
        self.kill(x.unwrap().index() as usize)
    }

    #[inline]
    pub fn kill_all(&mut self, kill: &BitSet) {
        self.kill.union_with(kill);
        self.gen.difference_with(kill);
    }

    #[inline]
    pub fn gen_all(&mut self, gen: &BitSet) {
        self.gen.union_with(gen);
        self.kill.difference_with(gen);
    }
}
