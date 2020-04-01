/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::HierarchicalId;
use crate::ast_lowering::error::Type::NotAScope;
use crate::ast_lowering::error::*;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::Ast;

/// A macro that hides the boiler plate required for name resolution using the resolver struct
///
/// If `$name` wasn't found or doesn't math any [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration)::`$declaration` the appropriate errors are added to `$self`.errors and execution continuous after the macro
///
/// # Arguments
///
/// * `$self` - reference to an ast fold (implementation of the [`Fold`](crate::ast_lowering::ast_to_hir_fold::Fold) trait)
///
/// * `$name` - The Identifier (type [`Ident`](crate::symbol::Ident)) that should be resolved
///
/// The following three arguments can be repeated as often as necessary (like in a match block)
///
/// * `$declaration` An identifier of an [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration) variant which you wish to resolve `$name`as
///
/// * `$id` The identifier under which the ID of the resolved declaration will be available inside the associated `$block
///
/// * `$block` A block (`{`statements`}`) which will be executed when `$name` is resolved as a [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration)::`$declaration`  
///
/// # Examples
///
/// The following tries to resolve ident as a Nature
/// ```
/// use VARF::symbol_table::SymbolDeclaration::Nature;
/// use VARF::symbol_table::SymbolDeclaration::Discipline;
///
/// resolve!(fold; ident as
///            Nature(id) => {
///                 println!("Resolved nature")
///             }
///             Discipline(id) => {
///                 println!("Resolved a discipline")
///             }
/// );
///
/// println!("Not found or not a discipline/nature")
/// ```
#[macro_export]
macro_rules! resolve {
    ($fold:expr; $name:ident as $($declaration:ident($id:ident) => $block:block),+) => {
        match $fold.resolver.resolve($name) {
            $(Ok($crate::symbol_table::SymbolDeclaration::$declaration($id)) => $block),+
            Err(error) => {
                $fold.error(error);
            }
            Ok(found) => {
                use $crate::ast_lowering::error;
                $fold.error(Error {
                    error_type: error::Type::DeclarationTypeMismatch {
                        found,
                        expected: vec![$(error::MockSymbolDeclaration::$declaration),+],
                    },
                    source: $name.span,
                });
            }
        }
    };
}

/// A macro that hides the boiler plate required for name resolution of hieraichal Identifiers using the resolver struct
///
/// If `$name` wasn't found or doesn't math any [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration)::`$declaration` the appropriate errors are added to `$self`.errors and execution continuous after the macro
///
/// # Arguments
///
/// * `$fold` - identifer refering to an [`Fold`](crate::ast_lowering::ast_to_hir_fold::Fold) instance
///
/// * `$name` - The Identifier (type [`Ident`](crate::symbol::Ident)) that should be resolved
///
/// The following three arguments can be repeated as often as necessary (like in a match block)
///
/// * `$declaration` An identifier of an [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration) variant which you wish to resolve `$name`as
///
/// * `$id` The identifier under which the ID of the resolved declaration will be available inside the associated `$block
///
/// * `$block` A block (`{`statements`}`) which will be executed when `$name` is resolved as a [`SymbolDeclaration`](crate::symbol_table::SymbolDeclaration)::`$declaration`  
///
/// # Examples
///
/// The following tries to resolve ident as a Nature
/// ```
/// use VARF::symbol_table::SymbolDeclaration::Net;
/// use VARF::symbol_table::SymbolDeclaration::Port;
///
/// resolve_hierarchical!(fold; ident as
///            Net(id) => {
///                 print!("Resolved net")
///            }
///            Port(id) => {
///                 print!("Resolved port")
///            }
/// );
///
/// println!("Not found or not a port/net")
/// ```
#[macro_export]
macro_rules! resolve_hierarchical {
    ($fold:expr; $name:ident as  $($declaration:ident($id:ident) => $block:block),+) => {
        match $fold.resolver.resolve_hierarchical($name) {
            $(Ok($crate::symbol_table::SymbolDeclaration::$declaration($id)) => $block),+
            Err(error) => {
                $fold.error(error);
            }
            Ok(found) => {
                use $crate::ast_lowering::error;
                $fold.error(Error {
                    error_type: error::Type::DeclarationTypeMismatch {
                        found,
                        expected:  vec![$(error::MockSymbolDeclaration::$declaration),+],
                    },
                    source: $name.span(),
                });
            }
        }
    };
}

/// Allows name resolution with the [`resolve`](crate::ast_lowering::name_resolution::Resolver::resolve)/[`resolve_hierarchical`](crate::ast_lowering::name_resolution::Resolver::resolve_hierarchical) methods
pub struct Resolver<'tag, 'lt> {
    pub scope_stack: Vec<&'lt SymbolTable<'tag>>,
    ast: &'lt Ast<'tag>,
}

impl<'tag, 'lt> Resolver<'tag, 'lt> {
    pub fn new(ast: &'lt Ast<'tag>) -> Self {
        Self {
            scope_stack: Vec::with_capacity(8),
            ast,
        }
    }

    /// Tries to resolve the Identifier `ident`
    ///
    /// This functions first tries to find `ident in the current scope, then in the previous scope and so on
    /// If it can't find ident in the global (first) Scope it returns an NotFound Error
    pub fn resolve(&self, ident: &Ident) -> Result<'tag, SymbolDeclaration<'tag>> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(&ident.name) {
                return Ok(*res);
            }
        }
        Err(Error {
            error_type: Type::NotFound(ident.name),
            source: ident.span,
        })
    }

    /// Tries to resolve the Hierarchical Identifer `hierarchical_ident`
    ///
    /// This functions tires to resolve the first identifier using the [`resolve`](crate::ast_lowering::name_resolution::Resolver::resolve) function
    /// Afterwards it tries to resolve all following identifier inside the scope of the previously resolved Identifier
    pub fn resolve_hierarchical(
        &self,
        hierarchical_ident: &HierarchicalId,
    ) -> Result<'tag, SymbolDeclaration<'tag>> {
        let mut identifiers = hierarchical_ident.names.iter();

        let (mut current_span, mut current_declaration) = {
            let first_ident = identifiers.next().unwrap();
            (first_ident.span, self.resolve(first_ident)?)
        };

        for ident in identifiers {
            let symbol_table = match current_declaration {
                SymbolDeclaration::Module(module) => &self.ast[module].contents.symbol_table,

                SymbolDeclaration::Block(block_id) => {
                    if let Some(scope) = &self.ast[block_id].contents.scope {
                        &scope.symbols
                    } else {
                        return Err(Error {
                            error_type: Type::NotFound(ident.name),
                            source: current_span,
                        });
                    }
                }

                item_without_scope => {
                    return Err(Error {
                        error_type: NotAScope {
                            declaration: item_without_scope.span(self.ast),
                            name: item_without_scope.name(self.ast),
                        },
                        source: current_span,
                    });
                }
            };

            if let Some(found) = symbol_table.get(&ident.name) {
                current_declaration = *found;
                current_span = found.span(self.ast)
            } else {
                return Err(Error {
                    error_type: Type::NotFound(ident.name),
                    source: ident.span,
                });
            }
        }

        Ok(current_declaration)
    }
    /// Enter a new Scope
    ///
    /// This scope will be the first to be searched for identifiers until it is overshadowed by another enter_scope call or it is removed using [`exit_scope`](crate::ast_lowering::name_resolution::Resolver::exit_scope)
    ///
    /// # Arguments
    /// * `scope_symbol_table` - Symbol table in which the resolver will look for definitions in this scope
    pub fn enter_scope(&mut self, scope_symbol_table: &'lt SymbolTable<'tag>) {
        self.scope_stack.push(scope_symbol_table)
    }
    /// Leave the current Scope
    ///
    /// Items defined in the current scope will not be resolved anymore by this resolver after this method has been called
    ///
    /// # Returns
    ///
    /// * None if the Resolver doesn't hold any Scopes
    /// * The Symbol_Table of the removed Scope
    pub fn exit_scope(&mut self) -> Option<&'lt SymbolTable<'tag>> {
        self.scope_stack.pop()
    }
}
