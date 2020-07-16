/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast_lowering::error::MockSymbolDeclaration;
use crate::ir::{
    BlockId, BranchId, DisciplineId, FunctionId, ModuleId, NatureId, NetId, ParameterId, PortId,
    VariableId,
};
use crate::sourcemap::Span;
use crate::symbol::{Ident, Symbol};

use super::ast::Ast;
use crate::HashMap;

pub type SymbolTable = HashMap<Symbol, SymbolDeclaration>;
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
    #[must_use]
    pub fn span(self, ast: &Ast) -> Span {
        match self {
            Self::Module(id) => ast[id].span,
            Self::Block(id) => ast[id].span,
            Self::Variable(id) => ast[id].span,
            Self::Net(id) => ast[id].span,
            Self::Branch(id) => ast[id].span,
            Self::Port(id) => ast[id].span,
            Self::Function(id) => ast[id].span,
            Self::Discipline(id) => ast[id].span,
            Self::Nature(id) => ast[id].span,
            Self::Parameter(id) => ast[id].span,
        }
    }

    #[must_use]
    pub fn ident(self, ast: &Ast) -> Ident {
        match self {
            Self::Module(id) => ast[id].contents.name,
            Self::Block(id) => ast[id].contents.scope.as_ref().unwrap().name,
            Self::Variable(id) => ast[id].contents.name,
            Self::Net(id) => ast[id].contents.name,
            Self::Branch(id) => ast[id].contents.name,
            Self::Port(id) => ast[id].contents.ident,
            Self::Function(id) => ast[id].contents.name,
            Self::Discipline(id) => ast[id].contents.name,
            Self::Nature(id) => ast[id].contents.name,
            Self::Parameter(id) => ast[id].contents.name,
        }
    }

    #[must_use]
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
