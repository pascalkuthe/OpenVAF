use std::intrinsics::transmute;
use std::iter::once;
use std::ops::Deref;
use std::sync::Arc;
use std::{fs, io};

use ahash::AHashMap;
use anyhow::{bail, Result};
use basedb::diagnostics::{
    Chars, Config, ConsoleSink, Diagnostic, DiagnosticSink, Label, LabelStyle, Report,
};
use basedb::lints::{Lint, LintLevel};
use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsPath, VfsStorage, STANDARD_FLAGS};
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::nameres::ScopeDefItem;
use hir_def::{Lookup, ModuleId, ParamId, ParamSysFun, Type, VarId};
use hir_ty::db::{Alias, HirTyDB, HirTyDatabase};
use hir_ty::{collect_diagnostics, collect_path, visit_relative_defs};
use indexmap::IndexMap;
use parking_lot::RwLock;
use paths::AbsPathBuf;
use salsa::ParallelDatabase;
use smol_str::SmolStr;
use syntax::ast::{AttrsOwner, Expr, LiteralKind, ParamDecl, VarDecl};
use syntax::sourcemap::FileSpan;
use syntax::{AstNode, TextRange};
use typed_index_collections::TiSlice;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
pub struct CompilationDB {
    storage: salsa::Storage<CompilationDB>,
    pub vfs: Arc<RwLock<Vfs>>,
    pub root_file: FileId,
}

impl Upcast<dyn HirDefDB> for CompilationDB {
    fn upcast(&self) -> &(dyn HirDefDB + 'static) {
        self
    }
}

impl Upcast<dyn BaseDB> for CompilationDB {
    fn upcast(&self) -> &(dyn BaseDB + 'static) {
        self
    }
}

impl CompilationDB {
    pub fn new(
        root_file: AbsPathBuf,
        include_dirs: &[AbsPathBuf],
        macro_flags: &[String],
        lints: &[(String, LintLevel)],
    ) -> Result<Self> {
        let contents = fs::read(&root_file);
        CompilationDB::new_(root_file.into(), contents, include_dirs, macro_flags, lints)
    }

    pub fn new_(
        root_file: VfsPath,
        contents: Result<Vec<u8>, io::Error>,
        include_dirs: &[AbsPathBuf],
        macro_flags: &[String],
        lints: &[(String, LintLevel)],
    ) -> Result<Self> {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();

        let root_file = vfs.ensure_file_id(root_file);
        vfs.set_file_contents(root_file, contents.into());

        let mut res =
            Self { storage: salsa::Storage::default(), vfs: Arc::new(RwLock::new(vfs)), root_file };

        let include_dirs: Result<Arc<[_]>> = once(Ok(VfsPath::new_virtual_path("/std".to_owned())))
            .chain(include_dirs.iter().map(|it| Ok(VfsPath::from(it.clone()))))
            .collect();
        res.set_include_dirs(root_file, include_dirs?);

        let macro_flags: Vec<_> = STANDARD_FLAGS
            .into_iter()
            .chain(macro_flags.iter().map(String::deref))
            .map(Arc::from)
            .collect();
        res.set_macro_flags(root_file, Arc::from(macro_flags));

        res.set_plugin_lints(&[]);
        let mut overwrites = res.empty_global_lint_overwrites();
        let registry = res.lint_registry();

        fn replace_lvl(
            overwrites: &mut TiSlice<Lint, Option<LintLevel>>,
            registry: &basedb::lints::LintRegistry,
            replaced_lvl: LintLevel,
            new_lvl: LintLevel,
        ) {
            for (lint, dst) in overwrites.iter_mut_enumerated() {
                let old_lvl = dst.unwrap_or_else(|| registry.lint_data(lint).default_lvl);
                if old_lvl == replaced_lvl {
                    *dst = Some(new_lvl)
                }
            }
        }

        for (lint, lvl) in lints {
            match &**lint {
                "all" => overwrites.raw.fill(Some(*lvl)),
                "warnings" => replace_lvl(&mut overwrites, &registry, LintLevel::Warn, *lvl),
                "errors" => replace_lvl(&mut overwrites, &registry, LintLevel::Deny, *lvl),
                lint => {
                    if let Some(lint) = registry.lint_from_name(lint) {
                        overwrites[lint] = Some(*lvl)
                    } else {
                        bail!("unknown lint {lint}")
                    }
                }
            }
        }

        let overwrites: Arc<[_]> = Arc::from(overwrites.as_ref());
        let overwrites = unsafe {
            transmute::<Arc<[Option<LintLevel>]>, Arc<TiSlice<Lint, Option<LintLevel>>>>(overwrites)
        };

        res.set_global_lint_overwrites(root_file, overwrites);
        Ok(res)
    }

    pub fn collect_modules(&self, all_vars_opvars: bool) -> Option<Vec<ModuleInfo>> {
        let root_file = self.root_file;
        let file_name =
            self.vfs.read().file_path(root_file).name().unwrap_or_else(|| String::from("~.va"));

        let mut config = Config { chars: Chars::ascii(), ..Config::default() };
        config.styles.header_error.set_intense(false);
        config.styles.header_warning.set_intense(false);
        config.styles.header_help.set_intense(false);
        config.styles.header_bug.set_intense(false);
        config.styles.header_note.set_intense(false);

        config.styles.note_bullet.set_bold(true).set_intense(true);
        config.styles.line_number.set_bold(true).set_intense(true);
        config.styles.source_border.set_bold(true).set_intense(true);
        config.styles.primary_label_bug.set_bold(true);
        config.styles.primary_label_note.set_bold(true);
        config.styles.primary_label_help.set_bold(true);
        config.styles.primary_label_error.set_bold(true);
        config.styles.primary_label_warning.set_bold(true);
        config.styles.secondary_label.set_bold(true);
        let mut sink = ConsoleSink::new(config, self.upcast());
        sink.add_diagnostics(&*self.preprocess(root_file).diagnostics, root_file, self);
        sink.add_diagnostics(self.parse(root_file).errors(), root_file, self);
        collect_diagnostics(self, root_file, &mut sink);

        if sink.summary(&file_name) {
            return None;
        }

        let root_def_map = self.def_map(root_file);
        let modules: Vec<_> =
            root_def_map[root_def_map.entry()]
                .declarations
                .iter()
                .filter_map(|(_, def)| {
                    if let ScopeDefItem::ModuleId(module) = def {
                        Some(module)
                    } else {
                        None
                    }
                })
                .copied()
                .collect();

        let res = modules
            .iter()
            .map(|module| ModuleInfo::collect(self, *module, &mut sink, all_vars_opvars))
            .collect();

        if sink.summary(&file_name) {
            return None;
        }

        Some(res)
    }
}

impl ParallelDatabase for CompilationDB {
    fn snapshot(&self) -> salsa::Snapshot<Self> {
        let db = CompilationDB {
            storage: self.storage.snapshot(),
            vfs: self.vfs.clone(),
            root_file: self.root_file,
        };

        salsa::Snapshot::new(db)
    }
}

/// This impl tells salsa where to find the salsa runtime.
impl salsa::Database for CompilationDB {}
impl VfsStorage for CompilationDB {
    fn vfs(&self) -> &RwLock<Vfs> {
        &self.vfs
    }
}

struct IllegalExpr {
    expr: Expr,
    expected: &'static str,
    attr: &'static str,
}

impl Diagnostic for IllegalExpr {
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let FileSpan { range, file } = db
            .parse(root_file)
            .to_file_span(self.expr.syntax().text_range(), &db.sourcemap(root_file));
        Report::error()
            .with_message(format!(
                "illegal expression supplied to '{}' attribute; expected {}",
                self.attr, self.expected
            ))
            .with_labels(vec![Label {
                style: LabelStyle::Primary,
                file_id: file,
                range: range.into(),
                message: "illegal expression".to_owned(),
            }])
    }
}

struct UnknownType<'a> {
    range: TextRange,
    allowed: &'static str,
    found: &'a str,
}

impl Diagnostic for UnknownType<'_> {
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let FileSpan { range, file } =
            db.parse(root_file).to_file_span(self.range, &db.sourcemap(root_file));
        Report::warning()
            .with_message(format!("unknown type \"{}\" {}", self.found, self.allowed))
            .with_labels(vec![Label {
                style: LabelStyle::Primary,
                file_id: file,
                range: range.into(),
                message: "unknown type".to_owned(),
            }])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamInfo {
    pub name: SmolStr,
    pub alias: Vec<SmolStr>,
    pub unit: String,
    pub description: String,
    pub group: String,
    pub ty: Type,
    pub is_instance: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpVar {
    pub name: SmolStr,
    pub unit: String,
    pub description: String,
    pub ty: Type,
}

pub struct ModuleInfo {
    pub id: ModuleId,
    pub params: IndexMap<ParamId, ParamInfo, ahash::RandomState>,
    pub sys_fun_alias: IndexMap<ParamSysFun, Vec<SmolStr>, ahash::RandomState>,
    pub op_vars: IndexMap<VarId, OpVar, ahash::RandomState>,
}

impl ModuleInfo {
    fn collect(
        db: &CompilationDB,
        module: ModuleId,
        sink: &mut ConsoleSink,
        all_vars_opvars: bool,
    ) -> ModuleInfo {
        let root_file = db.root_file;

        let mut params: IndexMap<ParamId, ParamInfo, ahash::RandomState> = IndexMap::default();
        let mut sys_fun_alias: IndexMap<ParamSysFun, Vec<SmolStr>, ahash::RandomState> =
            IndexMap::default();
        let mut op_vars = IndexMap::default();

        let parse = db.parse(root_file);
        let ast_id_map = db.ast_id_map(root_file);

        let mut resolved_param_attrs = AHashMap::new();
        let mut resolved_opvar_attrs = AHashMap::new();

        let resolve_str_attr = |sink: &mut ConsoleSink, ast: &dyn AttrsOwner, attr_name| {
            let val = ast.get_attr(attr_name)?.val()?;
            if let Expr::Literal(lit) = &val {
                if let LiteralKind::String(lit) = lit.kind() {
                    return Some(lit.unescaped_value());
                }
            }

            let diag = IllegalExpr { expr: val, expected: "a string literal", attr: attr_name };
            sink.add_diagnostic(&diag, root_file, db.upcast());
            None
        };

        // let check_numeric = |allowed, var, syntax: &SyntaxNode, sink: &mut ConsoleSink| {
        //     if !matches!(db.var_data(var).ty, Type::Real | Type::Integer) {
        //         sink.add_diagnostic(
        //             &IllegalType { range: syntax.text_range(), allowed },
        //             root_file,
        //             db,
        //         );
        //     }
        // };

        visit_relative_defs(db, module.lookup(db.upcast()).scope, |path, name, def| match def {
            ScopeDefItem::VarId(var) => {
                let loc = var.lookup(db.upcast());
                let ast = loc.item_tree(db.upcast())[loc.id].ast_id;
                let ast = ast_id_map.get(ast).to_node(parse.tree().syntax());
                let ast = VarDecl::cast(ast.syntax().parent().unwrap()).unwrap();
                let name = collect_path(path, name);

                // TODO actually save description/units?

                // 3.2.1 Output variables
                //
                // operating point variables must fulfill two properties
                // * belong to a module (not a block/function) -> no path
                // * have a description or units attribute

                if !path.is_empty() {
                    return;
                }

                let range = ast.syntax().text_range();

                let resolve_var_info = || {
                    let units = resolve_str_attr(sink, &ast, "units");
                    let description = resolve_str_attr(sink, &ast, "desc");

                    if units.is_none() && description.is_none() && !all_vars_opvars {
                        return None;
                    }

                    let ty = db.var_data(var).ty.clone();

                    let opvar = OpVar {
                        name: SmolStr::new_inline(""),
                        unit: units.unwrap_or_default(),
                        description: description.unwrap_or_default(),
                        ty,
                    };

                    Some(opvar)
                };

                if let Some(info) =
                    resolved_opvar_attrs.entry(range).or_insert_with(resolve_var_info)
                {
                    let mut info = info.clone();
                    info.name = name;
                    op_vars.insert(var, info);
                }
            }

            ScopeDefItem::ParamId(param) => {
                // TODO ParamInfo
                let loc = param.lookup(db.upcast());
                let ast = loc.item_tree(db.upcast())[loc.id].ast_id;
                let ast = ast_id_map.get(ast).to_node(parse.tree().syntax());
                let ast = ParamDecl::cast(ast.syntax().parent().unwrap()).unwrap();

                let range = ast.syntax().text_range();

                let resolve_param_info = || {
                    let ty = db.param_ty(param);
                    let type_ = resolve_str_attr(sink, &ast, "type");
                    let is_instance = match type_.as_deref() {
                        Some("instance") => true,
                        Some("model") | None => false,
                        Some(found) => {
                            let diag = UnknownType {
                                range,
                                allowed: "expected \"model\" or \"instance\"",
                                found,
                            };
                            sink.add_diagnostic(&diag, root_file, db);
                            false
                        }
                    };
                    ParamInfo {
                        name: SmolStr::new_inline(""),
                        alias: Vec::new(),
                        unit: resolve_str_attr(sink, &ast, "units").unwrap_or_default(),
                        description: resolve_str_attr(sink, &ast, "desc").unwrap_or_default(),
                        group: resolve_str_attr(sink, &ast, "group").unwrap_or_default(),
                        ty,
                        is_instance,
                    }
                };

                let mut info =
                    resolved_param_attrs.entry(range).or_insert_with(resolve_param_info).clone();

                info.name = collect_path(path, name);
                if let Some(old) = params.remove(&param) {
                    info.alias = old.alias;
                }
                params.insert(param, info);
            }

            ScopeDefItem::AliasParamId(alias) => match db.resolve_alias(alias).unwrap() {
                Alias::Cycel => unreachable!(),
                Alias::Param(param) => {
                    let dst = params.entry(param).or_insert_with(|| ParamInfo {
                        name: SmolStr::new_inline(""),
                        alias: Vec::new(),
                        unit: String::new(),
                        description: String::new(),
                        group: String::new(),
                        ty: Type::Err,
                        is_instance: false,
                    });
                    let name = collect_path(path, &db.alias_data(alias).name);
                    dst.alias.push(name)
                }
                Alias::ParamSysFun(sys_fun) => sys_fun_alias
                    .entry(sys_fun)
                    .or_default()
                    .push(collect_path(path, &db.alias_data(alias).name)),
            },

            // ScopeDefItem::NodeId(node) => {
            //     let data = db.node_data(node);
            //     if data.is_port() {
            //         ports.push(data.name.clone().into())
            //     }
            // }
            _ => (),
        });

        ModuleInfo { id: module, params, op_vars, sys_fun_alias }
    }
}
