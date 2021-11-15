use std::iter::once;
use std::sync::Arc;

use crate::diagnostics::PreprocessorDiagnostic::{
    self, MacroArgumentCountMissmatch, MacroNotFound, UnexpectedToken,
};
use crate::grammar::{parse_condition, parse_define, parse_include, parse_macro_call};

use crate::parser::{CompilerDirective, Parser, PreprocessorToken};
use crate::sourcemap::{CtxSpan, FileSpan, SourceContext, SourceMap};
use crate::{Diagnostics, FileReadError, ScopedTextArea};
use crate::{SourceProvider, Token};
use ahash::AHashMap;
use stdx::{impl_debug_display, impl_idx_from};
use text_size::{TextRange, TextSize};
use tokens::parser::SyntaxKind;
// use tracing::{debug, debug_span, trace};
use typed_index_collections::{TiSlice, TiVec};
use vfs::{FileId, VfsPath};

pub(crate) struct Processor<'a> {
    pub(crate) source_map: SourceMap,
    sources: &'a dyn SourceProvider,
    arena: &'a ScopedTextArea,
    macros: AHashMap<&'a str, Macro<'a>>,
    include_dirs: Arc<[VfsPath]>,
}

impl<'a> Processor<'a> {
    pub fn new(
        storage: &'a ScopedTextArea,
        root_file: FileId,
        sources: &'a dyn SourceProvider,
    ) -> Result<Self, FileReadError> {
        let src = sources.file_text(root_file)?;
        let src = storage.ensure(src);
        let macros = sources
            .macro_flags(root_file)
            .iter()
            .map(|name| -> (&str, Macro) {
                (
                    storage.ensure(name.clone()),
                    Macro { head: 0.into(), span: CtxSpan::dummy(), body: vec![], arg_cnt: 0 },
                )
            })
            .collect();
        let res = Self {
            source_map: SourceMap::new(root_file, TextSize::of(src)),
            macros,
            arena: storage,
            sources,
            include_dirs: sources.include_dirs(root_file),
        };
        Ok(res)
    }

    pub fn run(&mut self, file: FileId) -> (Vec<Token>, Diagnostics) {
        let working_dir = self.sources.file_path(file).parent().unwrap();

        let mut err = Diagnostics::new();
        let mut dst = Vec::new();
        let parser =
            Parser::new(self.arena.get(0), SourceContext::ROOT, working_dir, &mut dst, &mut err);
        self.process_file(parser, &mut err);

        (dst, err)
    }

    pub(crate) fn is_macro_defined(&mut self, name: &'a str) -> bool {
        self.macros.contains_key(name)
    }

    pub(crate) fn include_file(
        &mut self,
        path: &str,
        span: CtxSpan,
        dst: &mut Vec<Token>,
        errors: &mut Diagnostics,
        workdir: &VfsPath,
    ) -> Result<(), (FileReadError, Option<VfsPath>)> {
        let mut include_dirs = once(workdir).chain(&*self.include_dirs);
        let found = loop {
            if let Some(dir) = include_dirs.next() {
                if let Some(path) = dir.join(path) {
                    let file = self.sources.file_id(path.clone());
                    match self.sources.file_text(file) {
                        Ok(contents) => break Some((contents, file)),
                        Err(FileReadError::NotFound) => (),
                        Err(err) => return Err((err, Some(path))),
                    }
                }
            } else {
                break None;
            }
        };
        let (src, file) = found.ok_or((FileReadError::NotFound, None))?;
        let src = self.arena.ensure(src);
        let workdir = self.sources.file_path(file).parent().unwrap();

        let ctx = self
            .source_map
            .add_ctx(FileSpan { file, range: TextRange::up_to(TextSize::of(src)) }, span);

        let parser = Parser::new(src, ctx, workdir, dst, errors);
        self.process_file(parser, errors);

        Ok(())
    }

    pub(crate) fn define_macro(
        &mut self,
        name: &'a str,
        def: Macro<'a>,
        diagnostics: &mut Diagnostics,
    ) {
        let span = def.head_span();
        if let Some(old) = self.macros.insert(name, def) {
            diagnostics.push(PreprocessorDiagnostic::MacroOverwritten {
                old: old.head_span(),
                new: span,
                name: name.to_owned(),
            })
        }
    }

    fn process_macro_token(
        &mut self,
        token: &ParsedTokenKind<'a>,
        span: CtxSpan,
        args: &TiSlice<MacroArg, Vec<Token>>,
        dst: &mut Vec<Token>,
        errors: &mut Diagnostics,
    ) {
        match *token {
            ParsedTokenKind::ResolvedToken(kind) => dst.push(Token { kind, span }),
            ParsedTokenKind::ArgumentReference(arg) => {
                dst.extend(&args[arg]);
            }
            ParsedTokenKind::MacroCall(ref call) => self.call_macro(call, span, args, dst, errors),
        }
    }

    pub(crate) fn call_macro(
        &mut self,
        call: &MacroCall<'a>,
        span: CtxSpan,
        args: &TiSlice<MacroArg, Vec<Token>>,
        dst: &mut Vec<Token>,
        errors: &mut Diagnostics,
    ) {
        // TODO track recursion
        //
        let parent_ctx_span = self.source_map.ctx_data(span.ctx).decl.range.start();
        if let Some(def) = self.macros.get(&call.name).cloned() {
            let new_args: TiVec<_, _> = call
                .arg_bindings
                .iter()
                .map(|(arg, _decl)| {
                    let mut dst = Vec::new();
                    for ParsedToken { kind, range } in arg {
                        // trace!(range = debug(range), "Arg token");
                        let span = CtxSpan { range: range - parent_ctx_span, ctx: span.ctx };
                        self.process_macro_token(kind, span, args, &mut dst, errors)
                    }
                    dst
                })
                .collect();

            if new_args.len() == def.arg_cnt {
                let ctx = self.source_map.add_ctx(def.span.to_file_span(&self.source_map), span);
                for ParsedToken { kind, range } in &def.body {
                    let span = CtxSpan { range: range - def.span.range.start(), ctx };
                    self.process_macro_token(kind, span, &new_args, dst, errors)
                }
            } else {
                errors.push(MacroArgumentCountMissmatch {
                    expected: def.arg_cnt,
                    found: new_args.len(),
                    span,
                })
            }
        } else {
            errors.push(MacroNotFound { name: call.name.to_owned(), span })
        }
    }

    pub(crate) fn process_file(&mut self, mut p: Parser<'a, '_>, err: &mut Diagnostics) {
        while !p.at(PreprocessorToken::Eof) {
            self.process_token(&mut p, err)
        }
    }

    pub(crate) fn process_token(&mut self, p: &mut Parser<'a, '_>, err: &mut Diagnostics) {
        match p.current() {
            PreprocessorToken::Define { end } => {
                if let Some((name, def)) = parse_define(p, err, &mut self.source_map, end) {
                    self.define_macro(name, def, err)
                }
            }
            PreprocessorToken::CompilerDirective => match p.compiler_directive() {
                CompilerDirective::Include => {
                    if let Some((file_name, range)) = parse_include(p, err) {
                        let span = CtxSpan { range, ctx: p.ctx() };
                        match self.include_file(file_name, span, p.dst, err, &p.working_dir) {
                            Ok(_) => (),
                            Err((FileReadError::InvalidTextFormat, file)) => {
                                err.push(PreprocessorDiagnostic::InvalidTextFormat {
                                    file: file.unwrap(),
                                    span: Some(span),
                                })
                            }
                            Err((FileReadError::Io(kind), file)) => {
                                err.push(PreprocessorDiagnostic::IoError {
                                    file: file.unwrap(),
                                    error: kind,
                                    span: Some(span),
                                })
                            }

                            Err((FileReadError::NotFound, _)) => {
                                err.push(PreprocessorDiagnostic::FileNotFound {
                                    span: Some(span),
                                    file: file_name.to_owned(),
                                })
                            }
                        }
                    }
                }
                CompilerDirective::IfDef => {
                    // let _span = debug_span!("preprocessing `ifdef");
                    // let _tspan = _span.enter();
                    p.bump();
                    parse_condition(p, err, self, false);
                }
                CompilerDirective::IfNotDef => {
                    // let _span = debug_span!("preprocessing `ifndef");
                    // let _tspan = _span.enter();
                    p.bump();
                    parse_condition(p, err, self, true);
                }
                CompilerDirective::Macro => {
                    let (call, range) =
                        parse_macro_call(p, err, &[], &mut self.source_map, p.end());
                    let span = CtxSpan { range, ctx: p.ctx() };
                    self.call_macro(&call, span, TiSlice::from_ref(&[]), p.dst, err);
                }

                _ => {
                    err.push(UnexpectedToken(p.current_span()));
                    p.bump()
                }
            },

            _ => p.save_token(err),
        }
    }
}

pub(crate) type MacroArgs<'s> = TiVec<MacroArg, (Vec<ParsedToken<'s>>, TextRange)>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub(crate) struct MacroArg(u8);

impl_idx_from!(MacroArg(u8));
impl_debug_display!(c@MacroArg => "arg{}",c.0);

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ParsedToken<'s> {
    pub(crate) range: TextRange,
    pub(crate) kind: ParsedTokenKind<'s>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) enum ParsedTokenKind<'s> {
    ResolvedToken(SyntaxKind),
    ArgumentReference(MacroArg),
    MacroCall(MacroCall<'s>),
}

impl From<SyntaxKind> for ParsedTokenKind<'static> {
    fn from(value: SyntaxKind) -> ParsedTokenKind<'static> {
        ParsedTokenKind::ResolvedToken(value)
    }
}
#[derive(Debug, Clone)]
pub(crate) struct Macro<'s> {
    pub head: TextSize,
    pub span: CtxSpan,
    pub body: Vec<ParsedToken<'s>>,
    pub arg_cnt: usize,
}

impl Macro<'_> {
    pub fn head_span(&self) -> CtxSpan {
        self.span.with_len(self.head)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MacroCall<'s> {
    pub name: &'s str,
    pub arg_bindings: MacroArgs<'s>,
}
