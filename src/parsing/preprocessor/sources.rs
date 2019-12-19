use std::fs;
use std::path::Path;

use super::*;

impl<'lt> Preprocessor<'lt> {
    pub fn run(&mut self, source: &str) -> PreprocessorResult {
        let mut preprocessor_parse_result
            = PestParser::parse(Rule::PREPROCESSOR, self.source_container.alloc_str(source))?;//save source in arena to guarantee the reference to it will be valid for the entire preprocessor lifecycle
        self.process_parsed_source(preprocessor_parse_result.next().unwrap().into_inner().next().unwrap())
    }

    pub(super) fn process_include(&mut self, node: ParseTreeNode) -> PreprocessorResult {
        let span = node.as_span();
        let file = node.into_inner().next().unwrap().as_str();
        let path = Self::resolve_name(file);
        match fs::read_to_string(path) {
            Ok(file_contents) => {
                self.run(&file_contents)
            },
            Err(message) => error(&format!("Failed to load file {}:{}", file, message), span),
        }
    }

    fn resolve_name(file: &str) -> &Path {
        let resolved = match file {
            //TODO support multiple versions, make this more dynamic instead of hardcoding names
            "constants.vams" | "constants.va" | "disciplines.vams" | "disciplines.va" | "driver_access.vams" | "driver_access.va"
            => format!("stdlib/2.4/{}", file),
            //TODO proper include dir support
            _ => file.to_string(),
        };
        Path::new(file)
    }
}
