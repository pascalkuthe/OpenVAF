/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::ast::HierarchicalId;
use crate::ast_lowering::error::Error::{NotAScope, NotAllowedInFunction, NotFound, NotFoundIn};
use crate::ast_lowering::error::NotAllowedInFunction::NonLocalAccess;
use crate::ast_lowering::error::Result;
#[doc(inline)]
pub use crate::resolve;
#[doc(inline)]
pub use crate::resolve_hierarchical;
use crate::symbol::Ident;
use crate::symbol_table::{SymbolDeclaration, SymbolTable};
use crate::Ast;

/// A macro that hides the boiler plate required for name resolution using the resolver struct
/// It is defined in the [`name_resolution`](crate::ast_lowering::name_resolution) module but due to limitations of rustdoc can't be shown there in the documentation
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
/// use open_vaf::symbol_table::SymbolDeclaration::Nature;
/// use open_vaf::symbol_table::SymbolDeclaration::Discipline;
/// use open_vaf::resolve;
/// # use open_vaf::ast_lowering::Fold;
/// # use open_vaf::Ast;
/// # use open_vaf::diagnostic::MultiDiagnostic;
/// # use open_vaf::hir::Hir;
/// # use open_vaf::ast_lowering::name_resolution::Resolver;
/// # use open_vaf::symbol::Ident;
/// # let ast = Ast::default();
/// # let ident = Ident::from_str("test");
///
/// # let mut fold = Fold{
/// #    ast: &ast,
/// #    errors: MultiDiagnostic(Vec::new()),
/// #    hir: Hir::default(),
/// #    resolver: Resolver::new(&ast)
/// # };
///
/// resolve!(fold; ident as
///            Nature(id) => {
///                 println!("Resolved nature")
///             },
///
///             Discipline(id) => {
///                 println!("Resolved a discipline")
///             }
/// );
///
/// println!("Not found or not a discipline/nature")
/// ```
#[doc(inline)]
#[macro_export]
macro_rules! resolve {
    ($fold:expr; $name:ident as $($declaration:ident($id:ident) => $block:block),+) => {
        match $fold.resolver.resolve(&$name) {
            $(Ok($crate::symbol_table::SymbolDeclaration::$declaration($id)) => $block),+
            Err(error) => {
                $fold.error(error);
            }
            Ok(found) => {
                use $crate::ast_lowering::error::Error::DeclarationTypeMismatch;
                use $crate::ast_lowering::error::MockSymbolDeclaration;
                use $crate::util::format_list;
                $fold.error(DeclarationTypeMismatch {
                        name: $name.name,
                        found: found.mock(),
                        expected:  format_list(vec![$(MockSymbolDeclaration::$declaration),+]),
                        span: $name.span,
                    });
            }
        }
    };
}

/// A macro that hides the boiler plate required for name resolution of hieraichal Identifiers using the resolver struct
/// It is defined in the [`name_resolution`](crate::ast_lowering::name_resolution) module but due to limitations of rustdoc can't be shown there in the documentation   
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
/// use open_vaf::symbol_table::SymbolDeclaration::Net;
/// use open_vaf::symbol_table::SymbolDeclaration::Port;
/// use open_vaf::resolve_hierarchical;
/// # use open_vaf::ast_lowering::Fold;
/// # use open_vaf::Ast;
/// # use open_vaf::diagnostic::MultiDiagnostic;
/// # use open_vaf::hir::Hir;
/// # use open_vaf::ast_lowering::name_resolution::Resolver;
/// # use open_vaf::symbol::Ident;
/// # use open_vaf::ir::ast::HierarchicalId;
/// # let ast = Ast::default();
/// # let hid = vec![Ident::from_str("test")].into();
/// # let ident = &hid;
///
/// # let mut fold = Fold{
/// #    ast: &ast,
/// #    errors: MultiDiagnostic(Vec::new()),
/// #    hir: Hir::default(),
/// #    resolver: Resolver::new(&ast)
/// # };
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
                use $crate::ast_lowering::error::Error::DeclarationTypeMismatch;
                use $crate::ast_lowering::error::MockSymbolDeclaration;
                use $crate::util::format_list;
                $fold.error(DeclarationTypeMismatch {
                        name: $name.names.last().unwrap().name,
                        found: found.mock(),
                        expected:  format_list(vec![$(MockSymbolDeclaration::$declaration),+]),
                        span: $name.span(),
                    });
            }
        }
    };
}

/// Allows name resolution with the [`resolve`](crate::ast_lowering::name_resolution::Resolver::resolve)/[`resolve_hierarchical`](crate::ast_lowering::name_resolution::Resolver::resolve_hierarchical) methods
#[derive(Debug, Clone)]
pub struct Resolver<'lt> {
    pub scope_stack: Vec<&'lt SymbolTable>,
    ast: &'lt Ast,
    inside_function: bool,
}

impl<'lt> Resolver<'lt> {
    #[must_use]
    pub fn new(ast: &'lt Ast) -> Self {
        Self {
            scope_stack: Vec::with_capacity(8),
            ast,
            inside_function: false,
        }
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
    pub fn resolve(&self, ident: &Ident) -> Result<SymbolDeclaration> {
        let mut depth = 0;
        for scope in self.scope_stack.iter().rev() {
            if let Some(res) = scope.get(&ident.name) {
                if self.inside_function
                    && depth > 0
                    && !matches!(res,SymbolDeclaration::Parameter(_)|SymbolDeclaration::Module(_)|SymbolDeclaration::Function(_))
                {
                    return Err(NotAllowedInFunction(NonLocalAccess, ident.span));
                }
                return Ok(*res);
            }
            depth += 1;
        }
        Err(NotFound(*ident))
    }

    /// Tries to resolve the Hierarchical Identifer `hierarchical_ident`
    ///
    /// This functions tires to resolve the first identifier using the [`resolve`](crate::ast_lowering::name_resolution::Resolver::resolve) function
    /// Afterwards it tries to resolve all following identifier inside the scope of the previously resolved Identifier
    pub fn resolve_hierarchical(
        &self,
        hierarchical_ident: &HierarchicalId,
    ) -> Result<SymbolDeclaration> {
        let mut identifiers = hierarchical_ident.names.iter();

        let (mut current_span, mut current_declaration) = {
            let first_ident = identifiers.next().unwrap();
            (first_ident.span, self.resolve(first_ident)?)
        };

        for ident in identifiers {
            let symbol_table = match current_declaration {
                SymbolDeclaration::Module(module) => &self.ast[module].contents.symbol_table,

                SymbolDeclaration::Block(_) if self.inside_function => {
                    return Err(NotAllowedInFunction(NonLocalAccess, current_span))
                }

                SymbolDeclaration::Block(block_id) => {
                    &self.ast[block_id]
                        .contents
                        .scope
                        .as_ref()
                        .expect("Blocks always have a scope when they are named")
                        .symbols
                }

                item_without_scope => {
                    return Err(NotAScope {
                        declaration: item_without_scope.ident(self.ast).span,
                        declaration_name: item_without_scope.ident(self.ast).name,
                        span: current_span,
                    })
                }
            };

            if let Some(found) = symbol_table.get(&ident.name) {
                current_declaration = *found;
                current_span = found.span(self.ast);
                if self.inside_function
                    && !matches!(found,SymbolDeclaration::Parameter(_)|SymbolDeclaration::Module(_))
                {
                    return Err(NotAllowedInFunction(NonLocalAccess, ident.span));
                }
            } else {
                return Err(NotFoundIn(*ident, current_declaration.ident(self.ast).name));
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
