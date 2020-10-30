/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the OpenVAF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of OpenVAF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::error::Error::{NotAScope, NotAllowedInFunction, NotFound, NotFoundIn};
use crate::error::NotAllowedInFunction::NonLocalAccess;
use crate::error::{MockSymbolDeclaration, Result};
#[doc(inline)]
pub use crate::resolve;
#[doc(inline)]
pub use crate::resolve_hierarchical;
use openvaf_ast::symbol_table::{SymbolDeclaration, SymbolTable};
use openvaf_ast::{Ast, HierarchicalId};
use openvaf_data_structures::index_vec::IndexVec;
use openvaf_data_structures::HashMap;
use openvaf_ir::id_type;
use openvaf_ir::ids::{
    BlockId, BranchId, DisciplineId, FunctionId, ModuleId, NatureId, NetId, ParameterId,
    PortBranchId, PortId, VariableId,
};
use openvaf_session::symbols::Ident;

/// A macro that hides the boiler plate required for name resolution using the resolver struct
/// It is defined in the [`name_resolution`](crate::name_resolution) module but due to limitations of rustdoc can't be shown there in the documentation
/// If `$name` wasn't found or doesn't math any [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration)::`$declaration` the appropriate errors are added to `$self`.errors and execution continuous after the macro
///
/// # Arguments
///
/// * `$self` - reference to an ast fold (implementation of the [`Fold`](crate::Fold) trait)
///
/// * `$name` - The Identifier (type [`Ident`](openvaf_session::symbols::Ident)) that should be resolved
///
/// The following three arguments can be repeated as often as necessary (like in a match block)
///
/// * `$declaration` An identifier of an [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration) variant which you wish to resolve `$name`as
///
/// * `$id` The identifier under which the ID of the resolved declaration will be available inside the associated `$block
///
/// * `$block` A block (`{`statements`}`) which will be executed when `$name` is resolved as a [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration)::`$declaration`  
///
/// # Examples
///
/// The following tries to resolve ident as a Nature
/// ```
/// use openvaf_ast::symbol_table::SymbolDeclaration::Nature;
/// use openvaf_ast::symbol_table::SymbolDeclaration::Discipline;
/// use openvaf_ast_lowering::resolve;
/// # use openvaf_ast_lowering::Fold;
/// # use openvaf_ast::Ast;
/// # use openvaf_diagnostics::MultiDiagnostic;
/// # use openvaf_hir::Hir;
/// # use openvaf_ast_lowering::name_resolution::Resolver;
/// # use openvaf_session::symbols::Ident;
/// # use openvaf_session::openvaf_session;
///
/// # openvaf_session(||{
/// # let mut ast = Ast::default();
/// # let mut fold = Fold::new(&mut ast);
/// # let ident = Ident::from_str("test");
///
/// resolve!(fold; ident as
///    Nature(id) => {
///         println!("Resolved nature")
///    },
///
///    Discipline(id) => {
///        println!("Resolved a discipline")
///    }
/// );
///
/// println!("Not found or not a discipline/nature")
/// # });
/// ```

#[doc(inline)]
#[macro_export]
macro_rules! resolve {
    ($fold:expr; $name:ident as $($declaration:ident($id:ident) => $block:expr),+) => {
        match $fold.resolver.resolve(&$name) {
            $( #[allow(unreachable_code)] Ok($crate::name_resolution::SymbolReference::$declaration($id)) =>  {Some($block)}),+
            Err(error) => {
                $fold.error(error);
                None
            }
            Ok(found) => {
                use $crate::error::Error::DeclarationTypeMismatch;
                use $crate::error::MockSymbolDeclaration;
                use ::openvaf_diagnostics::format_list;
                $fold.error(DeclarationTypeMismatch {
                        name: $name.name,
                        found: found.mock(),
                        expected:  format_list(vec![$(MockSymbolDeclaration::$declaration),+]),
                        span: $name.span,
                    });
                None
            }
        }
    };
}

/// A macro that hides the boiler plate required for name resolution of hieraichal Identifiers using the resolver struct
/// It is defined in the [`name_resolution`](crate::name_resolution) module but due to limitations of rustdoc can't be shown there in the documentation   
/// If `$name` wasn't found or doesn't math any [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration)::`$declaration` the appropriate errors are added to `$self`.errors and execution continuous after the macro
///
/// # Arguments
///
/// * `$fold` - identifer refering to an [`Fold`](crate::Fold) instance
///
/// * `$name` - The Identifier (type [`Ident`](openvaf_session::symbols::Ident)) that should be resolved
///
/// The following three arguments can be repeated as often as necessary (like in a match block)
///
/// * `$declaration` An identifier of an [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration) variant which you wish to resolve `$name`as
///
/// * `$id` The identifier under which the ID of the resolved declaration will be available inside the associated `$block
///
/// * `$block` A block (`{`statements`}`) which will be executed when `$name` is resolved as a [`SymbolDeclaration`](openvaf_ast::symbol_table::SymbolDeclaration)::`$declaration`  
///
/// # Examples
///
/// The following tries to resolve ident as a Nature
/// ```
/// use openvaf_ast::symbol_table::SymbolDeclaration::Nature;
/// use openvaf_ast::symbol_table::SymbolDeclaration::Discipline;
/// use openvaf_ast_lowering::resolve_hierarchical;
/// # use openvaf_ast_lowering::Fold;
/// # use openvaf_ast::{Ast, HierarchicalId};
/// # use openvaf_diagnostics::MultiDiagnostic;
/// # use openvaf_hir::Hir;
/// # use openvaf_ast_lowering::name_resolution::Resolver;
/// # use openvaf_session::symbols::Ident;
/// # use openvaf_session::openvaf_session;
///
/// # openvaf_session(||{
/// # let mut ast = Ast::default();
/// # let mut fold = Fold::new(&mut ast);
/// # let hid = vec![Ident::from_str("test")];
/// # let ident = &HierarchicalId::from(hid);
///
/// resolve_hierarchical!(fold; ident as
///            Net(id) => {
///                 print!("Resolved net")
///            },
///            Port(id) => {
///                 print!("Resolved port")
///            }
/// );
///
/// println!("Not found or not a port/net")
/// # });
/// ```

#[macro_export]
macro_rules! resolve_hierarchical {
    ($fold:expr; $name:ident as $($declaration:ident($id:ident) => $block:expr),+ ) => {
        match $fold.resolver.resolve_hierarchical($name) {
            $(Ok($crate::name_resolution::SymbolReference::$declaration($id)) => {Some($block)}),+
            Err(error) => {
                $fold.error(error);
                None
            }
            Ok(found) => {
                use $crate::error::Error::DeclarationTypeMismatch;
                use $crate::error::MockSymbolDeclaration;
                use ::openvaf_diagnostics::format_list;
                $fold.error(DeclarationTypeMismatch {
                        name: $name.names.last().unwrap().name,
                        found: found.mock(),
                        expected:  format_list(vec![$(MockSymbolDeclaration::$declaration),+]),
                        span: $name.span(),
                    });
                None
            }
        }
    };
}

#[derive(Clone, Copy, Debug)]
pub enum SymbolReference {
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
    NatureAccess(NatureAccessId),
}

id_type!(NatureAccessId(u16));

impl From<SymbolDeclaration> for SymbolReference {
    fn from(declaration: SymbolDeclaration) -> Self {
        match declaration {
            SymbolDeclaration::Discipline(id) => Self::Discipline(id),
            SymbolDeclaration::Module(id) => Self::Module(id),
            SymbolDeclaration::Block(id) => Self::Block(id),
            SymbolDeclaration::Variable(id) => Self::Variable(id),
            SymbolDeclaration::Branch(id) => Self::Branch(id),
            SymbolDeclaration::PortBranch(id) => Self::PortBranch(id),
            SymbolDeclaration::Net(id) => Self::Net(id),
            SymbolDeclaration::Port(id) => Self::Port(id),
            SymbolDeclaration::Function(id) => Self::Function(id),
            SymbolDeclaration::Nature(id) => Self::Nature(id),
            SymbolDeclaration::Parameter(id) => Self::Parameter(id),
        }
    }
}

impl SymbolReference {
    #[must_use]
    pub fn mock(self) -> MockSymbolDeclaration {
        match self {
            Self::Module(_) => MockSymbolDeclaration::Module,
            Self::Block(_) => MockSymbolDeclaration::Block,
            Self::Variable(_) => MockSymbolDeclaration::Variable,
            Self::Net(_) => MockSymbolDeclaration::Net,
            Self::Branch(_) => MockSymbolDeclaration::Branch,
            Self::PortBranch(_) => MockSymbolDeclaration::PortBranch,
            Self::Port(_) => MockSymbolDeclaration::Port,
            Self::Function(_) => MockSymbolDeclaration::Function,
            Self::Discipline(_) => MockSymbolDeclaration::Discipline,
            Self::Nature(_) => MockSymbolDeclaration::Nature,
            Self::Parameter(_) => MockSymbolDeclaration::Parameter,
            Self::NatureAccess(_) => MockSymbolDeclaration::Nature,
        }
    }

    #[must_use]
    pub fn ident(self, resolver: &Resolver) -> Ident {
        match self {
            Self::Module(id) => resolver.ast[id].contents.ident,
            Self::Block(id) => resolver.ast[id].scope.as_ref().unwrap().ident,
            Self::Variable(id) => resolver.ast[id].contents.ident,
            Self::Net(id) => resolver.ast[id].contents.ident,
            Self::Branch(id) => resolver.ast[id].contents.ident,
            Self::PortBranch(id) => resolver.ast[id].contents.ident,
            Self::Port(id) => resolver.ast[resolver.ast[id].net].contents.ident,
            Self::Function(id) => resolver.ast[id].contents.ident,
            Self::Discipline(id) => resolver.ast[id].contents.ident,
            Self::Nature(id) => resolver.ast[id].contents.ident,
            Self::Parameter(id) => resolver.ast[id].contents.ident,
            Self::NatureAccess(id) => resolver.get_nature_access_ident(id),
        }
    }
}

/// Allows name resolution with the [`resolve`](crate::name_resolution::Resolver::resolve)/[`resolve_hierarchical`](crate::name_resolution::Resolver::resolve_hierarchical) methods
#[derive(Debug, Clone)]
pub struct Resolver<'lt> {
    nature_access_symbol_table: HashMap<Ident, NatureAccessId>,
    natures_access: IndexVec<NatureAccessId, (Ident, Vec<NatureId>)>,
    pub scope_stack: Vec<&'lt SymbolTable>,
    ast: &'lt Ast,
    inside_function: bool,
}

impl<'lt> Resolver<'lt> {
    #[must_use]
    pub fn new(ast: &'lt Ast) -> Self {
        Self {
            natures_access: IndexVec::with_capacity(ast.natures.len()),
            nature_access_symbol_table: HashMap::with_capacity(ast.natures.len()),
            scope_stack: Vec::with_capacity(8),
            ast,
            inside_function: false,
        }
    }

    pub fn insert_nature_access(&mut self, ident: Ident, nature: NatureId) {
        let natures_access = &mut self.natures_access;
        let id = *self
            .nature_access_symbol_table
            .entry(ident)
            .or_insert_with(|| natures_access.push((ident, Vec::with_capacity(2))));
        debug_assert!(
            !natures_access[id].1.contains(&nature),
            "Same access to same nature declared twice!"
        );
        natures_access[id].1.push(nature);
    }

    pub fn get_nature_access(&self, id: NatureAccessId) -> &[NatureId] {
        &self.natures_access[id].1
    }

    pub fn get_nature_access_ident(&self, id: NatureAccessId) -> Ident {
        self.natures_access[id].0
    }

    pub fn enter_function(&mut self, function_symbol_table: &'lt SymbolTable) {
        debug_assert!(!self.inside_function, "Already inside a function!");
        self.inside_function = true;
        self.enter_scope(function_symbol_table);
    }

    pub fn exit_function(&mut self) -> Option<&'lt SymbolTable> {
        debug_assert!(self.inside_function, "Not yet inside a function!");
        self.inside_function = false;
        self.exit_scope()
    }

    /// Tries to resolve the Identifier `ident`
    ///
    /// This functions first tries to find `ident in the current scope, then in the previous scope and so on
    /// If it can't find ident in the global (first) Scope it returns an NotFound Error
    pub fn resolve(&self, ident: &Ident) -> Result<SymbolReference> {
        for (depth, scope) in self.scope_stack.iter().rev().enumerate() {
            if let Some(&res) = scope.get(&ident.name) {
                if self.inside_function
                    && depth > 0
                    && !matches!(res,SymbolDeclaration::Parameter(_)|SymbolDeclaration::Module(_)|SymbolDeclaration::Function(_))
                {
                    return Err(NotAllowedInFunction(NonLocalAccess, ident.span));
                }
                return Ok(res.into());
            }
        }

        if let Some(&access) = self.nature_access_symbol_table.get(ident) {
            Ok(SymbolReference::NatureAccess(access))
        } else {
            Err(NotFound(*ident))
        }
    }

    /// Tries to resolve the Hierarchical Identifer `hierarchical_ident`
    ///
    /// This functions tires to resolve the first identifier using the [`resolve`](crate::name_resolution::Resolver::resolve) function
    /// Afterwards it tries to resolve all following identifier inside the scope of the previously resolved Identifier
    pub fn resolve_hierarchical(
        &self,
        hierarchical_ident: &HierarchicalId,
    ) -> Result<SymbolReference> {
        let mut identifiers = hierarchical_ident.names.iter();

        let (mut current_span, mut current_ref) = {
            let first_ident = identifiers.next().unwrap();
            (first_ident.span, self.resolve(first_ident)?)
        };

        for ident in identifiers {
            let symbol_table = match current_ref {
                SymbolReference::Module(module) => &self.ast[module].contents.symbol_table,

                SymbolReference::Block(_) if self.inside_function => {
                    return Err(NotAllowedInFunction(NonLocalAccess, current_span))
                }

                SymbolReference::Block(block_id) => {
                    &self.ast[block_id]
                        .scope
                        .as_ref()
                        .expect("Blocks always have a scope when they are named")
                        .symbols
                }

                item_without_scope => {
                    return Err(NotAScope {
                        declaration: item_without_scope.ident(self).span,
                        declaration_name: item_without_scope.ident(self).name,
                        span: current_span,
                    })
                }
            };

            if let Some(&found) = symbol_table.get(&ident.name) {
                current_ref = found.into();
                current_span = found.span(self.ast);
                if self.inside_function
                    && !matches!(found,SymbolDeclaration::Parameter(_)|SymbolDeclaration::Module(_))
                {
                    return Err(NotAllowedInFunction(NonLocalAccess, ident.span));
                }
            } else {
                return Err(NotFoundIn(*ident, current_ref.ident(self).name));
            }
        }

        Ok(current_ref)
    }
    /// Enter a new Scope
    ///
    /// This scope will be the first to be searched for identifiers until it is overshadowed by another enter_scope call or it is removed using [`exit_scope`](crate::name_resolution::Resolver::exit_scope)
    ///
    /// # Arguments
    /// * `scope_symbol_table` - Symbol table in which the resolver will look for definitions in this scope
    pub fn enter_scope(&mut self, scope_symbol_table: &'lt SymbolTable) {
        self.scope_stack.push(scope_symbol_table)
    }
    /// Leave the current Scope
    ///
    /// Items defined in the current scope will not be resolved anymore by this resolver after this method has been called
    ///
    /// # Returns
    ///
    /// * None if the Resolver doesn't hold any Scopes
    /// * The `SymbolTable` of the removed Scope
    pub fn exit_scope(&mut self) -> Option<&'lt SymbolTable> {
        self.scope_stack.pop()
    }
}
