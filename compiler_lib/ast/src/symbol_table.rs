/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

use openvaf_ir::ids::{
    BlockId, BranchId, DisciplineId, FunctionId, ModuleId, NatureId, NetId, ParameterId,
    PortBranchId, PortId, VariableId,
};
use openvaf_session::sourcemap::Span;
use openvaf_session::symbols::{Ident, Symbol};

use crate::Ast;
use openvaf_data_structures::HashMap;

pub type SymbolTable = HashMap<Symbol, SymbolDeclaration>;

#[derive(Clone, Copy, Debug)]
pub enum SymbolDeclaration {
    Module(ModuleId),
    Block(BlockId),
    Variable(VariableId),
    Branch(BranchId),
    PortBranch(PortBranchId),
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
        self.ident(ast).span
    }

    #[must_use]
    pub fn ident(self, ast: &Ast) -> Ident {
        match self {
            Self::Module(id) => ast[id].contents.ident,
            Self::Block(id) => ast[id].scope.as_ref().unwrap().ident,
            Self::Variable(id) => ast[id].contents.ident,
            Self::Net(id) => ast[id].contents.ident,
            Self::Branch(id) => ast[id].contents.ident,
            Self::PortBranch(id) => ast[id].contents.ident,
            Self::Port(id) => ast[ast[id].net].contents.ident,
            Self::Function(id) => ast[id].contents.ident,
            Self::Discipline(id) => ast[id].contents.ident,
            Self::Nature(id) => ast[id].contents.ident,
            Self::Parameter(id) => ast[id].contents.ident,
        }
    }
}

impl From<ModuleId> for SymbolDeclaration {
    fn from(id: ModuleId) -> Self {
        Self::Module(id)
    }
}

impl From<BlockId> for SymbolDeclaration {
    fn from(id: BlockId) -> Self {
        Self::Block(id)
    }
}

impl From<PortBranchId> for SymbolDeclaration {
    fn from(id: PortBranchId) -> Self {
        Self::PortBranch(id)
    }
}

impl From<VariableId> for SymbolDeclaration {
    fn from(id: VariableId) -> Self {
        Self::Variable(id)
    }
}

impl From<NetId> for SymbolDeclaration {
    fn from(id: NetId) -> Self {
        Self::Net(id)
    }
}

impl From<BranchId> for SymbolDeclaration {
    fn from(id: BranchId) -> Self {
        Self::Branch(id)
    }
}

impl From<PortId> for SymbolDeclaration {
    fn from(id: PortId) -> Self {
        Self::Port(id)
    }
}

impl From<FunctionId> for SymbolDeclaration {
    fn from(id: FunctionId) -> Self {
        Self::Function(id)
    }
}

impl From<DisciplineId> for SymbolDeclaration {
    fn from(id: DisciplineId) -> Self {
        Self::Discipline(id)
    }
}

impl From<NatureId> for SymbolDeclaration {
    fn from(id: NatureId) -> Self {
        Self::Nature(id)
    }
}

impl From<ParameterId> for SymbolDeclaration {
    fn from(id: ParameterId) -> Self {
        Self::Parameter(id)
    }
}
