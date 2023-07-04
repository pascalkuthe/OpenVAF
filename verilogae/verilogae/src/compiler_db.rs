use std::fs;
use std::intrinsics::transmute;
use std::iter::{once, repeat};
use std::ops::Deref;
use std::sync::Arc;

use ahash::AHashMap;
use anyhow::{bail, Result};
use basedb::diagnostics::{
    Config, ConsoleSink, Diagnostic, DiagnosticSink, Label, LabelStyle, Report, Severity,
};
use basedb::lints::{Lint, LintLevel};
use basedb::{BaseDB, BaseDatabase, FileId, Upcast, Vfs, VfsPath, VfsStorage, STANDARD_FLAGS};
use camino::Utf8Path;
use hir_def::db::{HirDefDB, HirDefDatabase, InternDatabase};
use hir_def::nameres::ScopeDefItem;
use hir_def::{BranchId, Lookup, ModuleId, NodeId, ParamId, Path, ScopeId, Type, VarId};
use hir_lower::CurrentKind;
use hir_ty::db::{HirTyDB, HirTyDatabase};
use hir_ty::lower::BranchKind;
use hir_ty::{collect_diagnostics, collect_path, visit_relative_defs};
use indexmap::IndexMap;
use lasso::{Rodeo, Spur};
use parking_lot::RwLock;
use salsa::ParallelDatabase;
use smol_str::SmolStr;
use stdx::iter::zip;
use syntax::ast::{Attr, AttrsOwner, Expr, LiteralKind, ParamDecl, PathExpr, VarDecl};
use syntax::sourcemap::FileSpan;
use syntax::{AstNode, SyntaxNode, TextRange};
use typed_index_collections::TiSlice;

use crate::opts::abs_path;
use crate::Opts;

#[salsa::database(BaseDatabase, InternDatabase, HirDefDatabase, HirTyDatabase)]
pub(crate) struct CompilationDB {
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
    pub(crate) fn new(root_file: &Utf8Path, opts: &Opts) -> Result<Self> {
        let mut vfs = Vfs::default();
        vfs.insert_std_lib();

        let root_file = if let Some(vfs_export) = opts.vfs()? {
            for (path, data) in vfs_export {
                vfs.add_virt_file(path, data.to_owned().into());
            }
            let root_file = root_file.to_string();

            if !root_file.starts_with('/') {
                bail!("VFS paths must start with '/'")
            }

            let root_file_id = vfs.file_id(&VfsPath::new_virtual_path(root_file.to_owned()));
            match root_file_id {
                Some(id) => id,
                None => bail!("path '{}' is not present in the VFS!", root_file),
            }
        } else {
            let root_file = abs_path(root_file)?;
            let contents = fs::read(&root_file);
            let root_file = vfs.ensure_file_id(root_file.into());
            vfs.set_file_contents(root_file, contents.into());
            root_file
        };

        let mut res =
            Self { storage: salsa::Storage::default(), vfs: Arc::new(RwLock::new(vfs)), root_file };

        let include_dirs: Result<Arc<[_]>> = once(Ok(VfsPath::new_virtual_path("/std".to_owned())))
            .chain(opts.include_dirs().map(|it| Ok(VfsPath::from(it?))))
            .collect();
        res.set_include_dirs(root_file, include_dirs?);

        let macro_flags: Vec<_> =
            STANDARD_FLAGS.into_iter().chain(opts.macro_flags()).map(Arc::from).collect();
        res.set_macro_flags(root_file, Arc::from(macro_flags));

        res.set_plugin_lints(&[]);
        let mut overwrites = res.empty_global_lint_overwrites();
        let registry = res.lint_registry();

        let allow_lints = zip(opts.allow_lints(), repeat(LintLevel::Allow));
        let warn_lints = zip(opts.warn_lints(), repeat(LintLevel::Warn));
        let deny_lints = zip(opts.deny_lints(), repeat(LintLevel::Deny));

        let mut sink = ConsoleSink::new(Config::default(), &res);
        for (lint, lvl) in allow_lints.chain(warn_lints).chain(deny_lints) {
            if let Some(lint) = registry.lint_from_name(lint) {
                overwrites[lint] = Some(lvl)
            } else {
                sink.print_simple_message(
                    Severity::Warning,
                    format!("no lint named '{}' was found!", lint),
                )
            }
        }
        drop(sink);

        let overwrites: Arc<[_]> = Arc::from(overwrites.as_ref());
        let overwrites = unsafe {
            transmute::<Arc<[Option<LintLevel>]>, Arc<TiSlice<Lint, Option<LintLevel>>>>(overwrites)
        };

        res.set_global_lint_overwrites(root_file, overwrites);
        Ok(res)
    }

    pub fn voltage_name(&self, hi: NodeId, lo: Option<NodeId>) -> String {
        let mut name = format!("br_{}", &self.node_data(hi).name);
        if let Some(lo) = lo {
            name.push_str(&self.node_data(lo).name)
        }
        name
    }

    pub fn current_name(&self, kind: CurrentKind) -> String {
        match kind {
            CurrentKind::Branch(branch) => self.branch_data(branch).name.deref().to_owned(),
            CurrentKind::Unnamed { hi, lo } => {
                let mut name = format!(" {} ", &self.node_data(hi).name);
                if let Some(lo) = lo {
                    name.push_str(&self.node_data(lo).name);
                    name.push(' ');
                }
                name
            }
            CurrentKind::Port(port) => format!("< {} >", &self.node_data(port).name),
        }
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

struct IllegalType {
    range: TextRange,
    allowed: &'static str,
}

impl Diagnostic for IllegalType {
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let FileSpan { range, file } =
            db.parse(root_file).to_file_span(self.range, &db.sourcemap(root_file));
        Report::error()
            .with_message(format!("VerilogAE only supports {}", self.allowed))
            .with_labels(vec![Label {
                style: LabelStyle::Primary,
                file_id: file,
                range: range.into(),
                message: "unsupported type".to_owned(),
            }])
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncSpec {
    pub var: VarId,
    pub dependency_breaking: Box<[VarId]>,
    pub prefix: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamInfo {
    pub name: SmolStr,
    pub unit: String,
    pub description: String,
    pub group: String,
    pub ty: Type,
}

pub struct ModelInfo {
    pub params: IndexMap<ParamId, ParamInfo, ahash::RandomState>,
    pub functions: Vec<FuncSpec>,
    pub var_names: AHashMap<VarId, SmolStr>,
    pub op_vars: Vec<SmolStr>,
    pub module: ModuleId,
    pub ports: Vec<SmolStr>,
    pub optional_currents: AHashMap<BranchId, f64>,
    pub optional_voltages: AHashMap<(NodeId, Option<NodeId>), f64>,
}

impl ModelInfo {
    pub(crate) fn collect(db: &CompilationDB, file_name: &str, name: Option<&str>) -> Result<Self> {
        let root_file = db.root_file;

        let mut sink = ConsoleSink::new(Config::default(), db.upcast());
        sink.add_diagnostics(&*db.preprocess(root_file).diagnostics, root_file, db);
        sink.add_diagnostics(db.parse(root_file).errors(), root_file, db);
        collect_diagnostics(db, root_file, &mut sink);

        if sink.summary(&file_name) {
            bail!("compiation failed");
        }

        let root_def_map = db.def_map(root_file);
        let module = root_def_map[root_def_map.entry()]
            .declarations
            .iter()
            .find_map(|(def_name, def)| {
                if let ScopeDefItem::ModuleId(module) = def {
                    if let Some(name) = name {
                        if name != def_name.deref() {
                            return None;
                        }
                    }
                    Some(module)
                } else {
                    None
                }
            })
            .copied();

        let module = match module {
            Some(module) => module,
            None => {
                let msg = if let Some(name) = name {
                    format!("failed to find module {}", name)
                } else {
                    "no module was found".to_owned()
                };

                bail!("{}", msg)
            }
        };

        let mut params = IndexMap::default();
        let mut functions = Vec::new();
        let mut var_names = AHashMap::new();
        let mut op_vars = Vec::new();

        let parse = db.parse(root_file);
        let ast_id_map = db.ast_id_map(root_file);
        let sm = db.sourcemap(root_file);

        let mut resolved_retrieve_attr = AHashMap::new();
        let mut resolved_param_attrs = AHashMap::new();
        let mut resolved_branch_attrs = AHashMap::new();

        let resolve_path = |sink: &mut ConsoleSink, expr: PathExpr, scope: ScopeId| {
            let path = Path::resolve(expr.path().unwrap()).unwrap();
            match scope.resolve_item_path(db.upcast(), &path) {
                Ok(var) => Some(var),
                Err(err) => {
                    let FileSpan { range, file } =
                        parse.to_file_span(expr.syntax().text_range(), &sm);
                    let report =
                        Report::error().with_message(err.to_string()).with_labels(vec![Label {
                            style: LabelStyle::Primary,
                            file_id: file,
                            range: range.into(),
                            message: err.message(),
                        }]);

                    sink.add_report(report);
                    None
                }
            }
        };

        let resolve_str_attr = |sink: &mut ConsoleSink, ast: &ParamDecl, attr_name| {
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

        let mut counter = 0;

        let check_numeric = |allowed, var, syntax: &SyntaxNode, sink: &mut ConsoleSink| {
            if !matches!(db.var_data(var).ty, Type::Real | Type::Integer) {
                sink.add_diagnostic(
                    &IllegalType { range: syntax.text_range(), allowed },
                    root_file,
                    db,
                );
            }
        };

        let mut ports = Vec::new();
        let mut optional_currents = AHashMap::new();
        let mut optional_voltages = AHashMap::new();

        visit_relative_defs(db, module.lookup(db.upcast()).scope, |path, name, def| match def {
            ScopeDefItem::VarId(var) => {
                let loc = var.lookup(db.upcast());
                let ast = loc.item_tree(db.upcast())[loc.id].ast_id;
                let ast = ast_id_map.get(ast).to_node(parse.tree().syntax());
                let ast = VarDecl::cast(ast.syntax().parent().unwrap()).unwrap();
                let name = collect_path(path, name);

                let mut resolve_retrieve = || {
                    let attr = ast.get_attr("retrieve")?;
                    let res = if let Some(val) = attr.val() {
                        check_numeric(
                            "calculating real and integer variables",
                            var,
                            ast.syntax(),
                            &mut sink,
                        );
                        match val {
                            Expr::PathExpr(expr) => {
                                if let Some(dep) = resolve_path(&mut sink, expr.clone(), loc.scope)
                                {
                                    check_numeric("breaking dependencies on real and integer variables", dep, expr.syntax(), &mut sink);
                                    vec![dep]
                                } else {
                                    vec![]
                                }
                            }
                            Expr::ArrayExpr(expr) => expr
                                .exprs()
                                .filter_map(|expr| {
                                    if let Expr::PathExpr(expr) = expr {
                                        let res = resolve_path(&mut sink, expr.clone(), loc.scope);
                                        if let Some(dep) = res{
                                            check_numeric("breaking dependencies on real and integer variables", dep, expr.syntax(), &mut sink);
                                        }
                                        res
                                    } else {
                                        let diag = IllegalExpr {
                                            expr,
                                            expected: "a path",
                                            attr: "retrieve",
                                        };
                                        sink.add_diagnostic(&diag, root_file, db.upcast());
                                        None
                                    }
                                })
                                .collect(),
                            expr => {
                                let diag = IllegalExpr {
                                    expr,
                                    expected: "a path or an array",
                                    attr: "retrieve",
                                };
                                sink.add_diagnostic(&diag, root_file, db.upcast());
                                vec![]
                            }
                        }
                    } else {
                        vec![]
                    };

                    Some(res.into_boxed_slice())
                };

                // TODO actually save description/units?

                let resolved =
                    resolved_retrieve_attr.entry(ast.syntax().text_range()).or_insert_with(|| {
                        (
                            resolve_retrieve(),
                            // 3.2.1 Output variables
                            path.is_empty() && ast.has_attr("description") || ast.has_attr("units"),
                        )
                    });

                if let Some(resolved) = &resolved.0 {
                    functions.push(FuncSpec {
                        var,
                        dependency_breaking: resolved.clone(),
                        prefix: format!(
                            "fun.{}",
                            base_n::encode(counter, base_n::ALPHANUMERIC_ONLY)
                        ),
                    });
                    counter += 1;
                }

                if resolved.1 {
                    op_vars.push(name.clone())
                }

                var_names.insert(var, name);
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
                    if !matches!(ty, Type::Real | Type::Integer | Type::String) {
                        sink.add_diagnostic(
                            &IllegalType { range, allowed: "real, integer and string parameters" },
                            root_file,
                            db,
                        );
                    }
                    ParamInfo {
                        name: SmolStr::new_inline(""),
                        unit: resolve_str_attr(&mut sink, &ast, "units").unwrap_or_default(),
                        description: resolve_str_attr(&mut sink, &ast, "desc").unwrap_or_default(),
                        group: resolve_str_attr(&mut sink, &ast, "group").unwrap_or_default(),
                        ty,
                    }
                };

                let mut info =
                    resolved_param_attrs.entry(range).or_insert_with(resolve_param_info).clone();

                info.name = collect_path(path, name);
                params.insert(param, info);
            }

            ScopeDefItem::NodeId(node) => {
                let data = db.node_data(node);
                if data.is_port() {
                    ports.push(data.name.clone().into())
                }
            }

            ScopeDefItem::BranchId(branch) => {
                let loc = branch.lookup(db.upcast());
                let ast = loc.item_tree(db.upcast())[loc.id].ast_id;
                let ast = ast_id_map.get(ast).to_node(parse.tree().syntax());

                let mut default_val = |attr: Attr| {
                    let val = match attr.val() {
                        Some(ref expr @ Expr::Literal(ref lit)) => match lit.kind() {
                            LiteralKind::IntNumber(val) => val.value() as f64,
                            LiteralKind::SiRealNumber(val) => val.value(),
                            LiteralKind::StdRealNumber(val) => val.value(),
                            _ => {
                                let diag = IllegalExpr {
                                    expr: expr.clone(),
                                    expected: "a real literal",
                                    attr: "vae_optional",
                                };
                                sink.add_diagnostic(&diag, root_file, db.upcast());
                                return None;
                            }
                        },
                        Some(expr) => {
                            let diag = IllegalExpr {
                                expr,
                                expected: "a real number",
                                attr: "vae_optional",
                            };
                            sink.add_diagnostic(&diag, root_file, db.upcast());
                            return None;
                        }
                        None => 0f64,
                    };

                    Some(val)
                };

                let resolve_optional = || {
                    if let Some(attr) = ast.get_attr("opt_voltage") {
                        if let Some(mut val) = default_val(attr) {
                            let (hi, lo) = match db.branch_info(branch).unwrap().kind {
                                BranchKind::NodeGnd(hi) => (hi, None),
                                BranchKind::Nodes(hi, lo) if db.node_data(hi).is_gnd => {
                                    val = -val;
                                    (lo, None)
                                }

                                BranchKind::Nodes(hi, lo) if db.node_data(lo).is_gnd => (hi, None),

                                BranchKind::Nodes(hi, lo) => (hi, Some(lo)),
                                _ => unreachable!(),
                            };

                            // TODO check for multiple declaration
                            optional_voltages.insert((hi, lo), val);
                            if let Some(lo) = lo {
                                optional_voltages.insert((lo, Some(hi)), -val);
                            }
                        }
                    }
                    ast.get_attr("opt_current").and_then(default_val)
                };

                if let Some(val) = *resolved_branch_attrs
                    .entry(ast.syntax().text_range())
                    .or_insert_with(resolve_optional)
                {
                    optional_currents.insert(branch, val);
                }
            }

            _ => (),
        });

        if sink.summary(&file_name) {
            bail!("compilation failed");
        }

        Ok(ModelInfo {
            params,
            functions,
            op_vars,
            module,
            var_names,
            ports,
            optional_currents,
            optional_voltages,
        })
    }

    pub(crate) fn intern_model(&self, db: &CompilationDB, literals: &mut Rodeo) -> InternedModel {
        let params = self
            .params
            .values()
            .map(|param| {
                let name = literals.get_or_intern(&*param.name);
                let unit = literals.get_or_intern(&param.unit);
                let description = literals.get_or_intern(&param.description);
                let group = literals.get_or_intern(&param.group);
                InternedParam { name, unit, description, group, ty: &param.ty }
            })
            .collect();

        let functions = self
            .functions
            .iter()
            .map(|fun| {
                let name = literals.get_or_intern(&*self.var_names[&fun.var]);
                let prefix = literals.get_or_intern(&fun.prefix);
                InternedFunction { name, prefix }
            })
            .collect();

        let opvars = self.op_vars.iter().map(|name| literals.get_or_intern(&**name)).collect();

        let nodes = self.ports.iter().map(|name| literals.get_or_intern(&**name)).collect();

        let module_name = &*self.module.lookup(db).name(db);
        let module_name = literals.get_or_intern(module_name);

        InternedModel { params, opvars, nodes, functions, module_name }
    }
}

pub struct InternedModel<'a> {
    pub params: Vec<InternedParam<'a>>,
    pub opvars: Vec<Spur>,
    pub nodes: Vec<Spur>,
    pub functions: Vec<InternedFunction>,
    pub module_name: Spur,
}

pub struct InternedFunction {
    pub name: Spur,
    pub prefix: Spur,
}

pub struct InternedParam<'a> {
    pub name: Spur,
    pub unit: Spur,
    pub description: Spur,
    pub group: Spur,
    pub ty: &'a Type,
}
