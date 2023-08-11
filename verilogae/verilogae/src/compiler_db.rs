use std::fs;
use std::iter::repeat;

use ahash::{AHashMap, AHashSet};
use anyhow::{bail, Result};
use basedb::diagnostics::{ConsoleSink, Diagnostic, DiagnosticSink, Label, LabelStyle, Report};
use basedb::lints::LintLevel;
use basedb::{BaseDB, FileId, VfsPath};
use camino::Utf8Path;
use hir::{
    Branch, BranchKind, Module, Node, Parameter, PathResolveError, ScopeDef, Type, Variable,
};
use hir_lower::CurrentKind;
use indexmap::IndexMap;
use lasso::{Rodeo, Spur};
use smol_str::SmolStr;
use stdx::iter::zip;
use syntax::ast::{self, Attr, Expr, LiteralKind, PathExpr};
use syntax::sourcemap::FileSpan;
use syntax::{AstNode, TextRange};

use crate::opts::abs_path;
use crate::Opts;

pub use hir::CompilationDB;

pub(crate) fn new(root_file: &Utf8Path, opts: &Opts) -> Result<CompilationDB> {
    let vfs = opts.vfs()?;
    let (root_file, root_file_contents) = if vfs.is_some() {
        let root_file = root_file.to_string();
        if !root_file.starts_with('/') {
            bail!("VFS paths must start with '/'")
        }
        (VfsPath::new_virtual_path(root_file), Ok(Vec::new()))
    } else {
        let root_file = abs_path(root_file)?;
        let content = fs::read(&root_file);
        (root_file.into(), content)
    };
    let allow_lints = zip(opts.allow_lints(), repeat(LintLevel::Allow));
    let warn_lints = zip(opts.warn_lints(), repeat(LintLevel::Warn));
    let deny_lints = zip(opts.deny_lints(), repeat(LintLevel::Deny));
    let lints = allow_lints.chain(warn_lints).chain(deny_lints);
    CompilationDB::new(
        root_file,
        root_file_contents,
        opts.include_dirs().map(|path| path.map(VfsPath::from)),
        opts.macro_flags(),
        lints,
    )
}

pub fn voltage_name(db: &CompilationDB, hi: Node, lo: Option<Node>) -> String {
    let mut name = format!("br_{}", hi.name(db));
    if let Some(lo) = lo {
        name.push_str(&lo.name(db))
    }
    name
}

pub fn current_name(db: &CompilationDB, kind: CurrentKind) -> String {
    match kind {
        CurrentKind::Branch(branch) => branch.name(db),
        CurrentKind::Unnamed { hi, lo } => {
            let mut name = format!(" {} ", &hi.name(db));
            if let Some(lo) = lo {
                name.push_str(&lo.name(db));
                name.push(' ');
            }
            name
        }
        CurrentKind::Port(port) => format!("< {} >", port.name(db)),
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
    pub var: Variable,
    pub dependency_breaking: Box<[Variable]>,
    pub prefix: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParamInfo {
    pub name: SmolStr,
    pub units: String,
    pub description: String,
    pub group: String,
    pub ty: Type,
}

pub struct ModelInfo {
    pub params: IndexMap<Parameter, ParamInfo, ahash::RandomState>,
    pub functions: Vec<FuncSpec>,
    pub var_names: AHashMap<Variable, SmolStr>,
    pub op_vars: Vec<SmolStr>,
    pub module: Module,
    pub ports: Vec<SmolStr>,
    pub optional_currents: AHashMap<Branch, f64>,
    pub optional_voltages: AHashMap<(Node, Option<Node>), f64>,
}

impl ModelInfo {
    pub(crate) fn collect(db: &CompilationDB, file_name: &str, name: Option<&str>) -> Result<Self> {
        let mut sink = ConsoleSink::new(db);
        let cu = db.compilation_unit();
        cu.diagnostics(db, &mut sink);

        if sink.summary(&file_name) {
            bail!("compiation failed");
        }

        let module = match cu.modules(db).get(0) {
            Some(&module) => module,
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
        let mut functions: Vec<_> = Vec::new();
        let mut var_names = AHashMap::new();
        let mut op_vars = Vec::new();
        let ast = cu.ast(db);

        let mut resolved_attrs = AHashSet::new();
        let mut add_diagnostic = |attr: ast::Attr, diag: &dyn Diagnostic| {
            if resolved_attrs.insert(attr.syntax().text_range()) {
                sink.add_diagnostic(diag, cu.root_file(), db)
            }
        };

        let mut optional_currents = AHashMap::new();
        let mut optional_voltages = AHashMap::new();

        let mut declarations = module.rec_declarations(db);

        while let Some((name, dec)) = declarations.next() {
            match dec {
                ScopeDef::Variable(var) => {
                    // 3.2.1 Output variables
                    //
                    // operating point variables must fulfill two properties
                    // * have a description or units attribute
                    // * belong to a module (not a block/function) -> no path

                    // check for units or description
                    let name_len = name.len();
                    let path = declarations.to_path(name);

                    // TODO actually save description/units?
                    let units = var.get_attr(db, &ast, "units");
                    let desc = var.get_attr(db, &ast, "desc");
                    if (units.is_some() || desc.is_some()) && path.len() == name_len {
                        op_vars.push(path.clone())
                    }

                    var_names.insert(var, path);

                    let attr = if let Some(attr) = var.get_attr(db, &ast, "retrieve") {
                        attr
                    } else {
                        continue;
                    };
                    let dependency_breaking = if let Some(val) = attr.val() {
                        if !matches!(var.ty(db), Type::Real | Type::Integer) {
                            add_diagnostic(
                                attr.clone(),
                                &IllegalType {
                                    range: attr.syntax().text_range(),
                                    allowed: "calculating real and integer variables",
                                },
                            );
                        }
                        match val {
                            Expr::PathExpr(expr) => {
                                let path = hir::Path::resolve(expr.path().unwrap()).unwrap();
                                let var = match module.lookup_var(db, &path) {
                                    Ok(var) => Some(var),
                                    Err(err) => {
                                        add_diagnostic(attr.clone(), &IllegalPath { expr, err });
                                        None
                                    }
                                };
                                if let Some(dep) = var{
                                    if !matches!(dep.ty(db), Type::Real | Type::Integer) {
                                        add_diagnostic(
                                            attr.clone(),
                                            &IllegalType {
                                                range: attr.syntax().text_range(),
                                                allowed: "breaking dependencies on real and integer variables",
                                            },
                                        );
                                    }
                                    vec![dep]
                                } else {
                                    vec![]
                                }
                            }
                            Expr::ArrayExpr(expr) => expr
                                .exprs()
                                .filter_map(|expr| {
                                    if let Expr::PathExpr(expr) = expr {
                                        let path = hir::Path::resolve(expr.path().unwrap()).unwrap();
                                        let var = match module.lookup_var(db, &path) {
                                            Ok(var) => Some(var),
                                            Err(err) => {
                                                add_diagnostic(attr.clone(), &IllegalPath { expr, err });
                                                None
                                            }
                                        };
                                        if let Some(var) = var{
                                            if !matches!(var.ty(db), Type::Real | Type::Integer) {
                                                add_diagnostic(
                                                    attr.clone(),
                                                    &IllegalType {
                                                        range: attr.syntax().text_range(),
                                                        allowed: "breaking dependencies on real and integer variables",
                                                    },
                                                );
                                            }
                                        }
                                        var
                                    } else {
                                        add_diagnostic(attr.clone(),&IllegalExpr {
                                            expr,
                                            expected: "a path",
                                            attr: "retrieve",
                                        });
                                        None
                                    }
                                })
                                .collect(),
                            expr => {
                                add_diagnostic(attr.clone(),&IllegalExpr {
                                    expr,
                                expected: "a path or an array",
                                    attr: "retrieve",
                                });
                                vec![]
                            }
                        }
                    } else {
                        vec![]
                    };

                    let n = functions.len();
                    functions.push(FuncSpec {
                        var,
                        dependency_breaking: dependency_breaking.into_boxed_slice(),
                        prefix: format!("fun.{}", base_n::encode(n as _, base_n::CASE_INSENSITIVE)),
                    });
                }

                ScopeDef::Parameter(param) => {
                    let units = param
                        .get_attr(db, &ast, "units")
                        .and_then(|attr| {
                            let lit = attr.val().and_then(|e| e.as_str_literal());
                            if lit.is_none() {
                                add_diagnostic(attr.clone(), &IllegalAttr { attr });
                            }
                            lit
                        })
                        .unwrap_or_default();

                    let description = param
                        .get_attr(db, &ast, "desc")
                        .and_then(|attr| {
                            let lit = attr.val().and_then(|e| e.as_str_literal());
                            if lit.is_none() {
                                add_diagnostic(attr.clone(), &IllegalAttr { attr });
                            }
                            lit
                        })
                        .unwrap_or_default();

                    let group = param
                        .get_attr(db, &ast, "group")
                        .and_then(|attr| {
                            let lit = attr.val().and_then(|e| e.as_str_literal());
                            if lit.is_none() {
                                add_diagnostic(attr.clone(), &IllegalAttr { attr });
                            }
                            lit
                        })
                        .unwrap_or_default();

                    params.insert(
                        param,
                        ParamInfo {
                            name: SmolStr::new_inline(""),
                            units,
                            description,
                            group,
                            ty: param.ty(db),
                        },
                    );
                }

                ScopeDef::Branch(branch) => {
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
                                    add_diagnostic(attr, &diag);
                                    return None;
                                }
                            },
                            Some(expr) => {
                                let diag = IllegalExpr {
                                    expr,
                                    expected: "a real number",
                                    attr: "vae_optional",
                                };
                                add_diagnostic(attr, &diag);
                                return None;
                            }
                            None => 0f64,
                        };

                        Some(val)
                    };

                    if let Some(attr) = branch.get_attr(db, &ast, "opt_voltage") {
                        if let Some(mut val) = default_val(attr) {
                            let (hi, lo) = match branch.kind(db) {
                                BranchKind::NodeGnd(hi) => (hi, None),
                                BranchKind::Nodes(hi, lo) if hi.is_gnd(db) => {
                                    val = -val;
                                    (lo, None)
                                }
                                BranchKind::Nodes(hi, lo) if lo.is_gnd(db) => (hi, None),
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
                    if let Some(val) =
                        branch.get_attr(db, &ast, "opt_current").and_then(default_val)
                    {
                        optional_currents.insert(branch, val);
                    }
                }
                _ => (),
            }
        }

        if sink.summary(&file_name) {
            bail!("compilation failed");
        }

        Ok(ModelInfo {
            params,
            functions,
            op_vars,
            module,
            var_names,
            ports: module.ports(db).into_iter().map(|node| node.name(db)).collect(),
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
                let unit = literals.get_or_intern(&param.units);
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

        let module_name = &*self.module.name(db);
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

struct IllegalAttr {
    attr: ast::Attr,
}

impl Diagnostic for IllegalAttr {
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let FileSpan { range, file } = db
            .parse(root_file)
            .to_file_span(self.attr.syntax().text_range(), &db.sourcemap(root_file));
        Report::error()
            .with_message(format!(
                "illegal expression supplied to '{}' attribute; expected a string literal",
                self.attr.name().unwrap(),
            ))
            .with_labels(vec![Label {
                style: LabelStyle::Primary,
                file_id: file,
                range: range.into(),
                message: "expected a string literal".to_owned(),
            }])
    }
}

struct IllegalPath {
    expr: PathExpr,
    err: PathResolveError,
}

impl Diagnostic for IllegalPath {
    fn build_report(&self, root_file: FileId, db: &dyn BaseDB) -> Report {
        let FileSpan { range, file } = db
            .parse(root_file)
            .to_file_span(self.expr.syntax().text_range(), &db.sourcemap(root_file));
        Report::error().with_message(self.err.to_string()).with_labels(vec![Label {
            style: LabelStyle::Primary,
            file_id: file,
            range: range.into(),
            message: self.err.message(),
        }])
    }
}
