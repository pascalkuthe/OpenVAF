use crate::diagnostic::{DiagnosticSlicePrinter, MultiDiagnostic, UserResult};
use crate::parser::tokenstream::Token as ParserToken;
use crate::parser::tokenstream::TokenStream as ParserTokenStream;
use crate::preprocessor::error::Error::{MacroNotFound, MissingToken, UnexpectedToken};
use crate::preprocessor::lexer::Token as LexicalToken;
use crate::preprocessor::lexer::{FollowedByBracket, Lexer};

use crate::literals::unesacpe_string;
use crate::preprocessor::lexer::Token::{
    LiteralInteger, LiteralRealNumber, LiteralRealNumberWithScaleChar,
};
use crate::preprocessor::lints::MacroOverwritten;
use crate::preprocessor::macros::MacroParser;
use crate::preprocessor::tokenstream::{Macro, MacroArg, Token};
use crate::sourcemap::{BytePos, FileId, SourceMap, SyntaxContext};
use crate::symbol::{Ident, Symbol};
use crate::{HashMap, Span, StringLiteral};
use core::mem::swap;
pub use error::{Error, Result};
use index_vec::{IndexSlice, IndexVec};
use log::debug;

use crate::lints::Linter;
use core::intrinsics::transmute;
use std::path::PathBuf;
use std::sync::Arc;

mod lints;

#[cfg(test)]
mod test;

macro_rules! skip_to_nested_end {
    ($preprocessor:ident, $start_span: expr, $start_token_0:path $(| $start_token:path)*, $end_token:path) => {
        let mut depth = 0;
        loop {
            match $preprocessor.next_expecting($start_span, $end_token)?.0 {
                $end_token if depth == 0 => break,
                $end_token => depth -= 1,
                $start_token_0 $(| $start_token)* => depth += 1,
                _ => (),
            }
        }
    }
}

pub mod error;
mod lexer;
mod macros;
mod tokenstream;

#[must_use]
pub fn parse_real_value(slice: &str, scale_char: Option<u8>) -> f64 {
    let source = slice.replace("_", "");
    let base: f64 = source.parse().unwrap();
    if let Some(scale_char) = scale_char {
        let scale_factor = match scale_char {
            b'T' => 12,
            b'G' => 9,
            b'M' => 6,
            b'K' | b'k' => 3,
            b'm' => -3,
            b'u' => -6,
            b'n' => -9,
            b'p' => -12,
            b'f' => -15,
            b'a' => -18,
            _ => unreachable_unchecked!("Lexer should not allow this"),
        };
        base * (10_f64).powi(scale_factor)
    } else {
        base
    }
}

#[must_use]
pub fn parse_unsigned_int_value(slice: &str) -> u32 {
    slice.replace("_", "").parse().unwrap()
}

pub fn preprocess(
    source_map: &mut SourceMap,
    main_file: FileId,
    paths: HashMap<&'static str, PathBuf>,
) -> std::result::Result<ParserTokenStream, MultiDiagnostic<Error>> {
    let mut precprocessor = Preprocessor::new(source_map, main_file, paths);
    precprocessor.run();
    if precprocessor.errors.0.is_empty() {
        Ok(precprocessor.dst)
    } else {
        Err(precprocessor.errors)
    }
}

pub fn preprocess_user_facing(
    sm: SourceMap,
    expansion_disclaimer: &'static str,
    main_file: FileId,
    paths: HashMap<&'static str, PathBuf>,
) -> UserResult<(ParserTokenStream, Arc<SourceMap>)> {
    preprocess_user_facing_with_printer(sm, expansion_disclaimer, main_file, paths)
}

pub fn preprocess_user_facing_with_printer<P: DiagnosticSlicePrinter>(
    mut sm: SourceMap,
    expansion_disclaimer: &'static str,
    main_file: FileId,
    paths: HashMap<&'static str, PathBuf>,
) -> UserResult<(ParserTokenStream, Arc<SourceMap>), P> {
    match preprocess(&mut sm, main_file, paths) {
        Ok(tokenstream) => Ok((tokenstream, Arc::new(sm))),
        Err(error) => Err(error.user_facing(&Arc::new(sm), expansion_disclaimer)),
    }
}

pub fn std_path(constants: PathBuf, disciplines: PathBuf) -> HashMap<&'static str, PathBuf> {
    vec![
        ("constants.vams", constants.clone()),
        ("constants.va", constants.clone()),
        ("constants.h", constants),
        ("disciplines.vams", disciplines.clone()),
        ("disciplines.va", disciplines.clone()),
        ("disciplines.h", disciplines),
    ]
    .into_iter()
    .collect()
}

pub struct Preprocessor<'sm> {
    pub source_map: &'sm mut SourceMap,
    pub macros: HashMap<Symbol, Macro>,
    pub lexer: Lexer<'sm>,
    pub errors: MultiDiagnostic<Error>,
    pub dst: ParserTokenStream,
    pub workingdir: PathBuf,
    /// A mapping from file names to their actual paths (used for the std libary mostly)
    pub paths: HashMap<&'static str, PathBuf>,
}

impl<'sm> Preprocessor<'sm> {
    pub fn new(
        source_map: &'sm mut SourceMap,
        file: FileId,
        paths: HashMap<&'static str, PathBuf>,
    ) -> Self {
        let workingdir = if let Some(dir) = source_map[file].path.parent() {
            dir.to_path_buf()
        } else {
            std::env::current_dir()
                .expect("Failed to read current working dir (up to caller to ensure)")
        };

        // average word length in HICUM without macro expansion and including comments is 8.1
        // as such we use 4 to have some wiggel room (especially with regards to macros) as most tokens correspond to one word
        let expected_token_count = source_map[file].contents().len() / 4;
        let mut res = Self {
            source_map,
            macros: HashMap::with_capacity(64),
            lexer: Lexer::new("", SyntaxContext::ROOT, BytePos::new(0)),
            errors: MultiDiagnostic(Vec::with_capacity(16)),
            dst: Vec::with_capacity(expected_token_count),
            workingdir,
            paths,
        };
        res.lexer = res.new_lexer(file, SyntaxContext::ROOT);
        res
    }

    pub fn run(&mut self) {
        while let Some((token, span)) = self.lexer.next() {
            match self.process_token(token, span) {
                Ok(()) => (),

                Err(error) => self.errors.add(error),
            }
        }
    }

    pub fn lookahead(&mut self, from: Span) -> Result<<Lexer as Iterator>::Item> {
        self.lexer
            .lookahead()
            .ok_or_else(|| Error::EndTooEarly(self.lexer.extend_span_to_current_end(from)))
    }

    pub fn lookahead_expecting(
        &mut self,
        from: Span,
        end: LexicalToken,
    ) -> Result<<Lexer as Iterator>::Item> {
        self.lexer.lookahead().ok_or_else(|| {
            Error::MissingTokenAtEnd(end, self.lexer.extend_span_to_current_end(from))
        })
    }

    // This does not impliment iterator because we need to be able to return a result
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Result<<Lexer as Iterator>::Item> {
        self.lexer
            .next()
            .ok_or_else(|| Error::EndTooEarly(self.lexer.span()))
    }

    pub fn next_from(&mut self, from: Span) -> Result<<Lexer as Iterator>::Item> {
        self.lexer
            .next()
            .ok_or_else(|| Error::EndTooEarly(self.lexer.extend_span_to_current_end(from)))
    }

    pub fn next_expecting(
        &mut self,
        from: Span,
        end: LexicalToken,
    ) -> Result<<Lexer as Iterator>::Item> {
        self.lexer.next().ok_or_else(|| {
            Error::MissingTokenAtEnd(end, self.lexer.extend_span_to_current_end(from))
        })
    }

    fn process_fulfilled_condition(&mut self, macro_span: Span) -> Result {
        loop {
            let (token, span) = self.next_expecting(macro_span, LexicalToken::MacroEndIf)?;
            match token {
                LexicalToken::MacroElse | LexicalToken::MacroElsif => {
                    return self.skip_until_endif(macro_span)
                }

                LexicalToken::MacroEndIf => {
                    return Ok(());
                }

                _ => {
                    if let Err(error) = self.process_token(token, span) {
                        self.errors.add(error)
                    }
                }
            }
        }
    }

    fn handle_macrocondition(
        &mut self,
        invert: bool,
        not_fullfilled: impl FnOnce(&mut Self, Ident) -> Result,
    ) -> Result {
        match self.lexer.expect_simple_ident() {
            Ok(macro_ident) => {
                if invert == self.macros.contains_key(&macro_ident.name) {
                    debug!(
                        "Preprocessor: Condition if{}def {} is not fulfilled",
                        if invert { "n" } else { "" },
                        macro_ident
                    );
                    not_fullfilled(self, macro_ident)
                } else {
                    debug!(
                        "Preprocessor: Condition if{}def {} is fulfilled",
                        if invert { "n" } else { "" },
                        macro_ident
                    );
                    self.process_fulfilled_condition(macro_ident.span)
                }
            }
            Err(error) => {
                #[allow(unused_must_use)]
                {
                    self.skip_until_endif(self.lexer.span());
                }
                Err(error)
            }
        }
    }

    fn skip_until_endif(&mut self, macro_span: Span) -> Result {
        skip_to_nested_end!(
            self,
            macro_span,
            LexicalToken::MacroIf | LexicalToken::MacroIfn,
            LexicalToken::MacroEndIf
        );
        Ok(())
    }

    fn declare_macro(&mut self, name: Symbol, def: Macro) {
        debug!("Macro {} was declared", name);
        let new_location = def.head;
        if let Some(old) = self.macros.insert(name, def) {
            Linter::dispatch_early(Box::new(MacroOverwritten {
                old: old.head,
                new: new_location,
                name,
            }))
        }
    }

    fn resolve_macro_token(
        &mut self,
        args: &IndexSlice<MacroArg, [ParserTokenStream]>,
        token: Token,
        span: Span,
    ) {
        match token {
            Token::ResolvedToken(token) => self.save_token(token, span),

            Token::ArgumentReference(arg) => {
                let ctxt = SyntaxContext::create(span);
                self.dst.extend(
                    args[arg]
                        .iter()
                        .copied()
                        .map(|(token, span_in_arg)| (token, span_in_arg.data().with_ctxt(ctxt))),
                );
            }

            Token::MacroDefinition(name, def) => self.declare_macro(name, def),

            Token::MacroCall(call) => {
                debug!("Resolving macro {}", call.name);
                let ctxt = SyntaxContext::create(span);
                if let Some(def) = self.macros.get(&call.name).cloned() {
                    debug!("Resolved macro {}!", call.name);
                    //the borrow checker hates this
                    let mut new_args = IndexVec::with_capacity(call.arg_bindings.len());
                    for arg in call.arg_bindings {
                        let mut tokens = ParserTokenStream::with_capacity(arg.len());
                        std::mem::swap(&mut tokens, &mut self.dst);
                        for (token, span) in arg {
                            // Span ctxt doesnt matter here it will be overwritten when the arg is resolve
                            self.resolve_macro_token(args, token, span)
                        }
                        std::mem::swap(&mut tokens, &mut self.dst);
                        new_args.push(tokens);
                    }

                    if new_args.len_idx() == def.arg_len_idx {
                        for (token, span) in def.body.clone() {
                            self.resolve_macro_token(&new_args, token, span.data().with_ctxt(ctxt));
                        }
                    } else {
                        self.errors.add(Error::MacroArgumentCountMissmatch {
                            expected: def.arg_len_idx,
                            found: new_args.len(),
                            span,
                        })
                    }
                } else {
                    self.errors.add(Error::MacroNotFound(call.name, span))
                }
            }

            Token::Condition(cond) => {
                let ctxt = span.data().ctxt;
                if self.macros.contains_key(&cond.if_def) == cond.inverted {
                    for (cond, body) in cond.else_ifs {
                        if self.macros.contains_key(&cond) {
                            for (token, span) in body {
                                self.resolve_macro_token(args, token, span.data().with_ctxt(ctxt))
                            }
                            return;
                        }
                    }

                    for (token, cond_span) in cond.else_tokens {
                        self.resolve_macro_token(args, token, cond_span.data().with_ctxt(ctxt))
                    }
                } else {
                    for (token, span) in cond.true_tokens {
                        self.resolve_macro_token(args, token, span.data().with_ctxt(ctxt))
                    }
                }
            }

            Token::FileInclude(path) => {
                let ctxt = SyntaxContext::create(span);
                if let Err(error) = self.process_file(path, ctxt) {
                    self.errors.add(Error::IoError(error, span))
                }
            }
        }
    }

    fn new_lexer(&mut self, file: FileId, ctxt: SyntaxContext) -> Lexer<'sm> {
        let file = &self.source_map[file];
        // This is save since the file contents live at least as long as the sourcemap
        // (there is no way to remove a file from the sourcemap)
        let contents = unsafe { transmute(file.contents()) };
        Lexer::new(contents, ctxt, file.lo)
    }

    fn process_file(&mut self, path: String, ctxt: SyntaxContext) -> std::io::Result<()> {
        let path = if let Some(path) = self.paths.get(path.as_str()) {
            path.clone()
        } else {
            self.workingdir.join(&path)
        };

        debug!("Entering {}", path.display());
        let file = self.source_map.add_file_from_fs(path)?;

        let mut lexer = self.new_lexer(file, ctxt);

        std::mem::swap(&mut self.lexer, &mut lexer);
        self.run();
        std::mem::swap(&mut self.lexer, &mut lexer);
        debug!("Done with file");
        Ok(())
    }

    fn try_parse_macro_definition(&mut self) -> Result<(Symbol, Macro)> {
        let (token, span) = self.next()?;
        let (name, parse_args, name_span) = match token {
            LexicalToken::SimpleIdentifier(followed_by_bracket) => {
                let name = self.lexer.slice(&span.data());
                (Symbol::intern(name), followed_by_bracket.0, span)
            }

            _ => {
                return Err(MissingToken(
                    LexicalToken::SimpleIdentifier(FollowedByBracket(false)),
                    span,
                ))
            }
        };

        let args = if parse_args {
            let bracket_span = self
                .lexer
                .expect(LexicalToken::ParenOpen)
                .expect("This should be known from the lexer");
            let mut args = IndexVec::with_capacity(8);
            loop {
                let ident = self.lexer.expect_simple_ident()?;
                args.push(ident.name);
                match self.next_expecting(bracket_span, LexicalToken::ParenClose)? {
                    (LexicalToken::ParenClose, _) => break,
                    (LexicalToken::Comma, _) => (),
                    (_, span) => {
                        self.errors.add(Error::UnexpectedToken(span));
                    }
                }
            }
            args
        } else {
            IndexVec::new()
        };

        let head_span = self.lexer.extend_span_to_current_end(name_span);

        let mut parser = MacroParser {
            parent: self,
            // Macros with args are usually more complex
            dst: Vec::with_capacity(if parse_args { 64 } else { 4 }),
            args,
        };

        parser.run(name);

        let definition = Macro {
            head: head_span,
            body: parser.dst,
            arg_len_idx: parser.args.len_idx(),
        };

        Ok((name, definition))
    }

    fn parse_macro_definition(&mut self) -> Result<(Symbol, Macro)> {
        let res = self.try_parse_macro_definition();
        match res {
            Err(Error::MissingTokenAtEnd(_, _)) | Err(Error::EndTooEarly(_)) | Ok(_) => (),
            Err(_) => {
                skip_to_nested_end!(
                    self,
                    self.lexer.span(),
                    LexicalToken::MacroDef,
                    LexicalToken::Newline
                );
            }
        }
        res
    }
}

impl<'lt> TokenProcessor<'lt> for Preprocessor<'lt> {
    type Res = ();

    #[inline(always)]
    fn preprocessor(&self) -> &Preprocessor<'lt> {
        self
    }

    fn save_token(&mut self, token: ParserToken, span: Span) {
        self.dst.push((token, span))
    }

    fn handle_include(&mut self, include_span: Span) -> Result {
        match self
            .lexer
            .expect_optional_at(LexicalToken::LiteralString, include_span)
        {
            Err(error) => self.errors.add(error),
            Ok(span) => {
                let mut literal_span = span.data();
                literal_span.lo += 1;
                literal_span.hi -= 1;

                let path = self.lexer.slice(&literal_span);
                let path = unesacpe_string(path);
                let call_span = include_span.extend(span);
                let ctxt = SyntaxContext::create(call_span);

                if let Err(error) = self.process_file(path, ctxt) {
                    self.errors.add(Error::IoError(error, call_span))
                }
            }
        }
        Ok(())
    }

    fn handle_macro_call(&mut self, ident: Ident, parse_args: bool) -> Result {
        let mut arg_bindings = IndexVec::with_capacity(4);
        if parse_args {
            let span = self.lexer.expect(LexicalToken::ParenOpen)?;
            'list: loop {
                let mut depth = 0;
                let mut arg = Vec::with_capacity(8);
                swap(&mut arg, &mut self.dst);
                loop {
                    let (token, span) = self.next_expecting(span, LexicalToken::ParenClose)?;

                    match token {
                        LexicalToken::Comma if depth == 0 => break,

                        LexicalToken::ParenClose if depth == 0 => {
                            swap(&mut arg, &mut self.dst);
                            arg_bindings.push(arg);
                            break 'list;
                        }

                        LexicalToken::ParenOpen => depth += 1,
                        LexicalToken::ParenClose => depth -= 1,
                        _ => (),
                    }

                    if let Err(error) = self.process_token(token, span) {
                        self.errors.add(error)
                    }
                }
                swap(&mut arg, &mut self.dst);
                arg_bindings.push(arg);
            }
        }
        debug!("Resolving macro {}", ident.name);
        let called_macro = if let Some(m) = self.macros.get(&ident.name) {
            m
        } else {
            self.errors.add(MacroNotFound(ident.name, ident.span));
            return Ok(());
        };
        debug!("Resolved macro {}!", ident.name);

        let call_span = self.lexer.extend_span_to_current_end(ident.span);

        if called_macro.arg_len_idx == arg_bindings.len_idx() {
            let ctx = SyntaxContext::create(call_span);
            let body = called_macro.body.clone();
            for (token, span) in body {
                self.resolve_macro_token(&arg_bindings, token, span.data().with_ctxt(ctx))
            }
        } else {
            self.errors.add(Error::MacroArgumentCountMissmatch {
                expected: called_macro.arg_len_idx,
                found: arg_bindings.len(),
                span: call_span,
            })
        }

        Ok(())
    }

    fn handle_macrodef(&mut self) -> Result {
        let (name, definition) = self.parse_macro_definition()?;
        self.declare_macro(name, definition);
        Ok(())
    }

    fn handle_ifdef(&mut self, inverted: bool) -> Result {
        self.handle_macrocondition(inverted, |preprocessor, macro_ident| loop {
            let (token, span) =
                preprocessor.next_expecting(macro_ident.span, LexicalToken::MacroEndIf)?;

            match token {
                LexicalToken::MacroElsif => {
                    return preprocessor.handle_macrocondition(false, |_, _| Ok(()))
                }
                LexicalToken::MacroEndIf => return Ok(()),
                LexicalToken::MacroElse => {
                    preprocessor.process_fulfilled_condition(span)?;
                    return Ok(());
                }

                LexicalToken::MacroIf | LexicalToken::MacroIfn => {
                    preprocessor.skip_until_endif(span)?
                }
                _ => (),
            }
        })
    }

    fn handle_macro_newline(&mut self, span: Span) -> Result {
        Err(Error::UnexpectedToken(span))
    }

    fn handle_simple_ident(&mut self, ident: Ident) -> Result {
        self.save_token(ParserToken::Ident(ident.name), ident.span);
        Ok(())
    }
}

pub trait TokenProcessor<'lt> {
    type Res: Default;

    fn preprocessor(&self) -> &Preprocessor<'lt>;
    fn save_token(&mut self, token: ParserToken, source_span: Span);

    fn handle_include(&mut self, include_directive_span: Span) -> Result;
    fn handle_macro_call(&mut self, name: Ident, args: bool) -> Result;
    fn handle_macrodef(&mut self) -> Result;
    fn handle_ifdef(&mut self, inverted: bool) -> Result;
    fn handle_newline(&mut self, _span: Span) -> Result<Self::Res> {
        Ok(Self::Res::default())
    }

    fn handle_macro_newline(&mut self, _span: Span) -> Result<Self::Res> {
        Ok(Self::Res::default())
    }
    fn handle_simple_ident(&mut self, ident: Ident) -> Result;

    fn process_token(&mut self, token: LexicalToken, span: Span) -> Result<Self::Res> {
        let parser_token = match token {
            LexicalToken::MacroDefNewLine => {
                return self.handle_macro_newline(span);
            }

            LexicalToken::Newline => {
                return self.handle_newline(span);
            }

            LexicalToken::MacroCall(followed_by_bracket) => {
                let mut name_span = span.data();
                name_span.lo += 1;
                let name = self.preprocessor().lexer.slice(&name_span);
                let ident = Ident::from_str_and_span(name, span);
                self.handle_macro_call(ident, followed_by_bracket.0)?;
                return Ok(Self::Res::default());
            }

            LexicalToken::Include => {
                self.handle_include(span)?;
                return Ok(Self::Res::default());
            }

            LexicalToken::MacroIf => {
                self.handle_ifdef(false)?;
                return Ok(Self::Res::default());
            }

            LexicalToken::MacroIfn => {
                self.handle_ifdef(true)?;
                return Ok(Self::Res::default());
            }

            LexicalToken::MacroDef => {
                self.handle_macrodef()?;
                return Ok(Self::Res::default());
            }

            LexicalToken::LiteralString => {
                let mut literal_span = span.data();
                literal_span.lo += 1;
                literal_span.hi -= 1;
                ParserToken::LiteralString(StringLiteral::create(
                    &literal_span,
                    &self.preprocessor().source_map,
                ))
            }

            LiteralInteger => ParserToken::IntLiteral(parse_unsigned_int_value(
                self.preprocessor().lexer.slice(&span.data()),
            )),

            LiteralRealNumberWithScaleChar => {
                let mut span = span.data();
                span.hi -= 1;
                let base_str = self.preprocessor().lexer.slice(&span);
                // The lexer already told us exactly where the scale char is (at the end) and since all scale chars are ascii we can use byte indicies/the byte value
                let scale_char = self.preprocessor().lexer.byte_at(span.hi);
                ParserToken::RealLiteral(parse_real_value(base_str, Some(scale_char)))
            }

            LiteralRealNumber => {
                let number_str = self.preprocessor().lexer.slice(&span.data());
                ParserToken::RealLiteral(parse_real_value(number_str, None))
            }

            LexicalToken::SimpleIdentifier(_) => {
                let name = self.preprocessor().lexer.slice(&span.data());
                let ident = Ident::from_str_and_span(name, span);
                self.handle_simple_ident(ident)?;
                return Ok(Self::Res::default());
            }

            LexicalToken::EscapedIdentifier => {
                let mut name_span = span.data();
                name_span.lo += 1;
                name_span.hi -= 1;
                let name = self.preprocessor().lexer.slice(&name_span);
                ParserToken::Ident(Symbol::intern(name))
            }

            LexicalToken::Finish => ParserToken::Finish,
            LexicalToken::Stop => ParserToken::Stop,
            LexicalToken::Info => ParserToken::Info,
            LexicalToken::Warn => ParserToken::Warn,
            LexicalToken::Error => ParserToken::Error,
            LexicalToken::Fatal => ParserToken::Fatal,
            LexicalToken::Temperature => ParserToken::Temperature,
            LexicalToken::Vt => ParserToken::Vt,
            LexicalToken::SimParam => ParserToken::SimParam,
            LexicalToken::SimParamStr => ParserToken::SimParamStr,
            LexicalToken::PortConnected => ParserToken::PortConnected,
            LexicalToken::ParamGiven => ParserToken::ParamGiven,
            LexicalToken::Display => ParserToken::Display,
            LexicalToken::Debug => ParserToken::Debug,
            LexicalToken::Strobe => ParserToken::Strobe,
            LexicalToken::Write => ParserToken::Write,
            LexicalToken::Accessor => ParserToken::Accessor,
            LexicalToken::Semicolon => ParserToken::Semicolon,
            LexicalToken::Colon => ParserToken::Colon,
            LexicalToken::Comma => ParserToken::Comma,
            LexicalToken::ParenOpen => ParserToken::ParenOpen,
            LexicalToken::ParenClose => ParserToken::ParenClose,
            LexicalToken::AttributeStart => ParserToken::AttributeStart,
            LexicalToken::AttributeEnd => ParserToken::AttributeEnd,
            LexicalToken::SquareBracketOpen => ParserToken::SquareBracketOpen,
            LexicalToken::SquareBracketClose => ParserToken::SquareBracketClose,
            LexicalToken::Contribute => ParserToken::Contribute,
            LexicalToken::Assign => ParserToken::Assign,
            LexicalToken::Hash => ParserToken::Hash,
            LexicalToken::OpMul => ParserToken::OpMul,
            LexicalToken::OpDiv => ParserToken::OpDiv,
            LexicalToken::OpModulus => ParserToken::OpModulus,
            LexicalToken::Plus => ParserToken::Plus,
            LexicalToken::Minus => ParserToken::Minus,
            LexicalToken::OpExp => ParserToken::OpExp,
            LexicalToken::OpLogicNot => ParserToken::OpLogicNot,
            LexicalToken::OpBitNot => ParserToken::OpBitNot,
            LexicalToken::OpArithmeticShiftLeft => ParserToken::OpArithmeticShiftLeft,
            LexicalToken::OpArithmeticShiftRight => ParserToken::OpArithmeticShiftRight,
            LexicalToken::OpLess => ParserToken::OpLess,
            LexicalToken::OpLessEqual => ParserToken::OpLessEqual,
            LexicalToken::OpGreater => ParserToken::OpGreater,
            LexicalToken::OpGreaterEqual => ParserToken::OpGreaterEqual,
            LexicalToken::OpEqual => ParserToken::OpEqual,
            LexicalToken::OpNotEqual => ParserToken::OpNotEqual,
            LexicalToken::OpLogicAnd => ParserToken::OpLogicAnd,
            LexicalToken::OpLogicalOr => ParserToken::OpLogicalOr,
            LexicalToken::OpBitAnd => ParserToken::OpBitAnd,
            LexicalToken::OpBitXor => ParserToken::OpBitXor,
            LexicalToken::OpBitNXor => ParserToken::OpBitNXor,
            LexicalToken::OpBitOr => ParserToken::OpBitOr,
            LexicalToken::OpCondition => ParserToken::OpCondition,
            LexicalToken::If => ParserToken::If,
            LexicalToken::Else => ParserToken::Else,
            LexicalToken::Case => ParserToken::Case,
            LexicalToken::EndCase => ParserToken::EndCase,
            LexicalToken::Default => ParserToken::Default,
            LexicalToken::While => ParserToken::While,
            LexicalToken::For => ParserToken::For,
            LexicalToken::Begin => ParserToken::Begin,
            LexicalToken::End => ParserToken::End,
            LexicalToken::Module => ParserToken::Module,
            LexicalToken::EndModule => ParserToken::EndModule,
            LexicalToken::Discipline => ParserToken::Discipline,
            LexicalToken::EndDiscipline => ParserToken::EndDiscipline,
            LexicalToken::Nature => ParserToken::Nature,
            LexicalToken::EndNature => ParserToken::EndNature,
            LexicalToken::Branch => ParserToken::Branch,
            LexicalToken::Parameter => ParserToken::Parameter,
            LexicalToken::DefineParameter => ParserToken::DefineParameter,
            LexicalToken::LocalParameter => ParserToken::LocalParameter,
            LexicalToken::Analog => ParserToken::Analog,
            LexicalToken::Function => ParserToken::Function,
            LexicalToken::EndFunction => ParserToken::EndFunction,
            LexicalToken::AnalogInitial => ParserToken::AnalogInitial,
            LexicalToken::Input => ParserToken::Input,
            LexicalToken::Inout => ParserToken::Inout,
            LexicalToken::Output => ParserToken::Output,
            LexicalToken::Signed => ParserToken::Signed,
            LexicalToken::Vectored => ParserToken::Vectored,
            LexicalToken::Scalared => ParserToken::Scalared,
            LexicalToken::String => ParserToken::String,
            LexicalToken::Time => ParserToken::Time,
            LexicalToken::Realtime => ParserToken::Realtime,
            LexicalToken::Integer => ParserToken::Integer,
            LexicalToken::Real => ParserToken::Real,
            LexicalToken::Reg => ParserToken::Reg,
            LexicalToken::Wreal => ParserToken::Wreal,
            LexicalToken::Supply0 => ParserToken::Supply0,
            LexicalToken::Supply1 => ParserToken::Supply1,
            LexicalToken::Tri => ParserToken::Tri,
            LexicalToken::TriAnd => ParserToken::TriAnd,
            LexicalToken::TriOr => ParserToken::TriOr,
            LexicalToken::Tri0 => ParserToken::Tri0,
            LexicalToken::Tri1 => ParserToken::Tri1,
            LexicalToken::Wire => ParserToken::Wire,
            LexicalToken::Uwire => ParserToken::Uwire,
            LexicalToken::Wand => ParserToken::Wand,
            LexicalToken::Wor => ParserToken::Wor,
            LexicalToken::Ground => ParserToken::Ground,
            LexicalToken::Potential => ParserToken::Potential,
            LexicalToken::Flow => ParserToken::Flow,
            LexicalToken::Domain => ParserToken::Domain,
            LexicalToken::Discrete => ParserToken::Discrete,
            LexicalToken::Continuous => ParserToken::Continuous,
            LexicalToken::TemperatureDerivative => ParserToken::TemperatureDerivative,
            LexicalToken::TimeDerivative => ParserToken::TimeDerivative,
            LexicalToken::PartialDerivative => ParserToken::PartialDerivative,
            LexicalToken::TimeIntegral => ParserToken::TimeIntegral,
            LexicalToken::TimeIntegralMod => ParserToken::TimeIntegralMod,
            LexicalToken::LimExp => ParserToken::LimExp,
            LexicalToken::WhiteNoise => ParserToken::WhiteNoise,
            LexicalToken::FlickerNoise => ParserToken::FlickerNoise,
            LexicalToken::Pow => ParserToken::Pow,
            LexicalToken::Sqrt => ParserToken::Sqrt,
            LexicalToken::Hypot => ParserToken::Hypot,
            LexicalToken::Exp => ParserToken::Exp,
            LexicalToken::Ln => ParserToken::Ln,
            LexicalToken::Log => ParserToken::Log,
            LexicalToken::Min => ParserToken::Min,
            LexicalToken::Max => ParserToken::Max,
            LexicalToken::Abs => ParserToken::Abs,
            LexicalToken::Floor => ParserToken::Floor,
            LexicalToken::Ceil => ParserToken::Ceil,
            LexicalToken::Sin => ParserToken::Sin,
            LexicalToken::Cos => ParserToken::Cos,
            LexicalToken::Tan => ParserToken::Tan,
            LexicalToken::ArcSin => ParserToken::ArcSin,
            LexicalToken::ArcCos => ParserToken::ArcCos,
            LexicalToken::ArcTan => ParserToken::ArcTan,
            LexicalToken::ArcTan2 => ParserToken::ArcTan2,
            LexicalToken::SinH => ParserToken::SinH,
            LexicalToken::CosH => ParserToken::CosH,
            LexicalToken::TanH => ParserToken::TanH,
            LexicalToken::ArcSinH => ParserToken::ArcSinH,
            LexicalToken::ArcCosH => ParserToken::ArcCosH,
            LexicalToken::ArcTanH => ParserToken::ArcTanH,
            LexicalToken::From => ParserToken::From,
            LexicalToken::Exclude => ParserToken::Exclude,
            LexicalToken::Infinity => ParserToken::Infinity,
            LexicalToken::MinusInfinity => ParserToken::MinusInfinity,
            LexicalToken::Abstol => ParserToken::Abstol,
            LexicalToken::Access => ParserToken::Access,
            LexicalToken::TimeDerivativeNature => ParserToken::TimeDerivativeNature,
            LexicalToken::TimeIntegralNature => ParserToken::TimeIntegralNature,
            LexicalToken::Units => ParserToken::Units,

            LexicalToken::MacroElsif
            | LexicalToken::MacroElse
            | LexicalToken::Unexpected
            | LexicalToken::MacroEndIf => return Err(UnexpectedToken(span)),
        };
        self.save_token(parser_token, span);
        Ok(Self::Res::default())
    }
}
