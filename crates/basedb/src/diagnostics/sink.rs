use std::fmt::Display;
use std::sync::Arc;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::Files;
pub use codespan_reporting::term::termcolor::{Ansi, Buffer, ColorChoice, NoColor};
use codespan_reporting::term::termcolor::{StandardStream, WriteColor};
use codespan_reporting::term::{emit, Config};
use vfs::VfsPath;

use crate::diagnostics::{Diagnostic, Report};
use crate::{BaseDB, FileId};

pub trait DiagnosticSink {
    fn add_report(&mut self, report: Report);
    fn add_diagnostic(&mut self, diagnostic: &impl Diagnostic, root_file: FileId, db: &dyn BaseDB) {
        if let Some(report) = diagnostic.to_report(root_file, db) {
            self.add_report(report)
        }
    }
    fn add_diagnostics<'a>(
        &mut self,
        diagnostics: impl IntoIterator<Item = &'a (impl Diagnostic + 'a)>,
        root_file: FileId,
        db: &dyn BaseDB,
    ) {
        diagnostics
            .into_iter()
            .for_each(|diagnostic| self.add_diagnostic(diagnostic, root_file, db))
    }
}

struct FileSrc<'a>(&'a dyn BaseDB);

impl<'a> Files<'_> for FileSrc<'a> {
    type FileId = FileId;

    type Name = VfsPath;

    type Source = Arc<str>;

    fn name(&self, id: FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok(self.0.file_path(id))
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
        match self.0.file_text(id) {
            Ok(src) => Ok(src),
            Err(_) => {
                let vfs = self.0.vfs().read();
                let contents = Arc::from(vfs.file_contents_unchecked(id));
                Ok(contents)
            }
        }
    }

    fn line_index(
        &self,
        file: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        Ok(self.0.line(byte_index.try_into().unwrap(), file).into())
    }

    fn line_range(
        &self,
        file: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        Ok(self.0.line_range(line_index.into(), file).into())
    }
}

pub struct ConsoleSink<'a> {
    warning_cnt: usize,
    error_cnt: usize,
    config: Config,
    db: &'a dyn BaseDB,
    dst: Box<dyn WriteColor + 'a>,
}

impl<'a> ConsoleSink<'a> {
    pub fn new(config: Config, db: &'a dyn BaseDB) -> ConsoleSink<'a> {
        ConsoleSink::new_with(config, db, Box::new(StandardStream::stderr(ColorChoice::Auto)))
    }

    pub fn buffer(config: Config, db: &'a dyn BaseDB, buffer: &'a mut Buffer) -> ConsoleSink<'a> {
        ConsoleSink::new_with(config, db, Box::new(buffer))
    }

    pub fn summary(&mut self, target_name: &impl Display) -> bool {
        if self.error_cnt != 0 {
            let warn = if self.warning_cnt != 0 {
                format!("; {} warning emitted", self.warning_cnt)
            } else {
                String::new()
            };
            let message = format!(
                "could not compile `{}` due to {} previous errors{}",
                target_name, self.error_cnt, warn
            );

            self.print_simple_message(Severity::Error, message);
            return true;
        }

        if self.warning_cnt != 0 {
            let message = format!("`{}` generated {} warning", target_name, self.warning_cnt);
            self.print_simple_message(Severity::Warning, message);
        }

        false
    }

    pub fn print_simple_message(&mut self, severity: Severity, msg: String) {
        emit(
            &mut self.dst,
            &self.config,
            &FileSrc(self.db),
            &Report::new(severity).with_message(msg),
        )
        .expect("Span emitting should never fail");
    }

    pub fn new_with(
        config: Config,
        db: &'a dyn BaseDB,
        dst: Box<dyn WriteColor + 'a>,
    ) -> ConsoleSink<'a> {
        ConsoleSink { warning_cnt: 0, error_cnt: 0, config, db, dst }
    }
}

// impl Drop for ConsoleSink<'_>{
//     fn drop(&self){
//         match self.error_cnt{
//             0 => {
//                 match self.warning_cnt{
//                     0 => eprintln!("")
//                     warnings =>
//                 }
//             }
//         }
//         println!("finished with")
//     }
// }

impl DiagnosticSink for ConsoleSink<'_> {
    fn add_report(&mut self, report: Report) {
        match report.severity {
            Severity::Error => self.error_cnt += 1,
            Severity::Warning => self.warning_cnt += 1,
            _ => (),
        }

        emit(&mut self.dst, &self.config, &FileSrc(self.db), &report)
            .expect("Span emitting should never fail");
    }
}

pub fn print_all<'a>(
    diagnostics: impl IntoIterator<Item = &'a (impl Diagnostic + 'a)>,
    db: &dyn BaseDB,
    root_file: FileId,
    config: Config,
) {
    ConsoleSink::new(config, db).add_diagnostics(diagnostics, root_file, db)
}
