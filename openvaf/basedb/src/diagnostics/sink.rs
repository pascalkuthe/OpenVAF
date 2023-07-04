use std::fmt::Display;
use std::sync::Arc;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::Files;
pub use codespan_reporting::term::termcolor::{Ansi, Buffer, ColorChoice, NoColor};
use codespan_reporting::term::termcolor::{StandardStream, WriteColor};
use codespan_reporting::term::{emit, Chars, Config};
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

struct FileSrc<'a> {
    db: &'a dyn BaseDB,
    anon_paths: bool,
}

impl<'a> Files<'_> for FileSrc<'a> {
    type FileId = FileId;

    type Name = VfsPath;

    type Source = Arc<str>;

    fn name(&self, id: FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        let mut path = self.db.file_path(id);
        if self.anon_paths {
            path = VfsPath::new_virtual_path(format!("/{}", path.name().unwrap()))
        }

        Ok(path)
    }

    fn source(&self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
        match self.db.file_text(id) {
            Ok(src) => Ok(src),
            Err(_) => {
                let vfs = self.db.vfs().read();
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
        Ok(self.db.line(byte_index.try_into().unwrap(), file).into())
    }

    fn line_range(
        &self,
        file: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        Ok(self.db.line_range(line_index.into(), file).into())
    }
}

pub struct ConsoleSink<'a> {
    warning_cnt: usize,
    error_cnt: usize,
    config: Config,
    db: &'a dyn BaseDB,
    dst: Box<dyn WriteColor + 'a>,
    anon_paths: bool,
}

impl<'a> ConsoleSink<'a> {
    pub fn new(db: &'a dyn BaseDB) -> ConsoleSink<'a> {
        ConsoleSink::new_with(db, Box::new(StandardStream::stderr(ColorChoice::Auto)))
    }

    pub fn buffer(db: &'a dyn BaseDB, buffer: &'a mut Buffer) -> ConsoleSink<'a> {
        ConsoleSink::new_with(db, Box::new(buffer))
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
            self.warning_cnt = 0;
        }

        false
    }

    pub fn print_simple_message(&mut self, severity: Severity, msg: String) {
        emit(
            &mut self.dst,
            &self.config,
            &FileSrc { db: self.db, anon_paths: self.anon_paths },
            &Report::new(severity).with_message(msg),
        )
        .expect("Span emitting should never fail");
    }

    pub fn new_with(db: &'a dyn BaseDB, dst: Box<dyn WriteColor + 'a>) -> ConsoleSink<'a> {
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

        ConsoleSink { warning_cnt: 0, error_cnt: 0, config, db, dst, anon_paths: false }
    }

    /// only print the filename instead of the full path, this is useful for UI tests where we do not want to expose the full path
    pub fn annonymize_paths(&mut self) {
        self.anon_paths = true;
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

        emit(
            &mut self.dst,
            &self.config,
            &FileSrc { db: self.db, anon_paths: self.anon_paths },
            &report,
        )
        .expect("Span emitting should never fail");
    }
}

pub fn print_all<'a>(
    diagnostics: impl IntoIterator<Item = &'a (impl Diagnostic + 'a)>,
    db: &dyn BaseDB,
    root_file: FileId,
) {
    ConsoleSink::new(db).add_diagnostics(diagnostics, root_file, db)
}
