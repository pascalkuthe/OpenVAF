/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast_lowering::error::MockSymbolDeclaration;
use crate::ir::{
    BlockId, BranchId, DisciplineId, FunctionId, ModuleId, NatureId, NetId, ParameterId, PortId,
    VariableId,
};
use crate::symbol::Symbol;
use crate::Span;
use rustc_hash::FxHashMap;

use super::ast::*;

pub type SymbolTable = FxHashMap<Symbol, SymbolDeclaration>;
#[derive(Clone, Copy, Debug)]
pub enum SymbolDeclaration {
    Module(ModuleId),
    Block(BlockId),
    Variable(VariableId),
    Branch(BranchId),
    Net(NetId),
    Port(PortId),
    Function(FunctionId),
    Discipline(DisciplineId),
    Nature(NatureId),
    Parameter(ParameterId),
}
impl SymbolDeclaration {
    pub fn span(self, ast: &Ast) -> Span {
        match self {
            Self::Module(id) => ast[id].source,
            Self::Block(id) => ast[id].source,
            Self::Variable(id) => ast[id].source,
            Self::Net(id) => ast[id].source,
            Self::Branch(id) => ast[id].source,
            Self::Port(id) => ast[id].source,
            Self::Function(id) => ast[id].source,
            Self::Discipline(id) => ast[id].source,
            Self::Nature(id) => ast[id].source,
            Self::Parameter(id) => ast[id].source,
        }
    }
    pub fn name(self, ast: &Ast) -> Symbol {
        match self {
            Self::Module(id) => ast[id].contents.name.name,
            Self::Block(id) => ast[id].contents.scope.as_ref().unwrap().name.name,
            Self::Variable(id) => ast[id].contents.name.name,
            Self::Net(id) => ast[id].contents.name.name,
            Self::Branch(id) => ast[id].contents.name.name,
            Self::Port(id) => ast[id].contents.name.name,
            Self::Function(id) => ast[id].contents.name.name,
            Self::Discipline(id) => ast[id].contents.name.name,
            Self::Nature(id) => ast[id].contents.name.name,
            Self::Parameter(id) => ast[id].contents.name.name,
        }
    }
    pub fn mock(self) -> MockSymbolDeclaration {
        match self {
            Self::Module(_) => MockSymbolDeclaration::Module,
            Self::Block(_) => MockSymbolDeclaration::Block,
            Self::Variable(_) => MockSymbolDeclaration::Variable,
            Self::Net(_) => MockSymbolDeclaration::Net,
            Self::Branch(_) => MockSymbolDeclaration::Branch,
            Self::Port(_) => MockSymbolDeclaration::Port,
            Self::Function(_) => MockSymbolDeclaration::Function,
            Self::Discipline(_) => MockSymbolDeclaration::Discipline,
            Self::Nature(_) => MockSymbolDeclaration::Nature,
            Self::Parameter(_) => MockSymbolDeclaration::Parameter,
        }
    }
}
