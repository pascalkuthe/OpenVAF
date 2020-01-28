/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use std::path::Path;

use bumpalo::Bump;

use crate::parser::lexer::Token;
use crate::parser::preprocessor::source_map::SourceMapBuilder;
use crate::parser::Error;
use crate::test::setup_logger;
use crate::{Preprocessor, Span};

#[test]
pub fn macros() -> std::result::Result<(), String> {
    let source_map_allocator = Bump::new();
    let preprocessor_allocator = Bump::new();
    setup_logger();
    let mut preprocessor = Preprocessor::new(
        &preprocessor_allocator,
        &source_map_allocator,
        Path::new("tests/macros.va"),
    )
    .expect("IoError");
    let mut start = 0;
    let mut end = 0;
    let mut span = Span::new(0, 0);
    let res = Ok(()).and_then(|_| {
        preprocessor.process_token()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "OK1");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::Comma);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "OK2");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::Comma);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "SMS__");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "OK3");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "OK3L");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::Comma);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "OK4");
        preprocessor.advance()?;
        start = preprocessor.current_start;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Sum1");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::Plus);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Sum2");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Fac1");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::OpMul);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Fac2");
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::Plus);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Fac1");
        span = preprocessor.current_span();
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::OpDiv);
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::SimpleIdentifier);
        assert_eq!(preprocessor.slice(), "Fac2");
        end = preprocessor.current_start + preprocessor.current_len;
        preprocessor.advance()?;
        assert_eq!(preprocessor.current_token(), Token::EOF);
        Ok(())
    });
    let source_map = preprocessor.done();
    res.map_err(|err: Error| {
        err.print(&source_map, true);
        ""
    })?;
    //    let (string, lines) = source_map.resolve_span(Span::new(start, end));
    //    println!("{} at {}", lines, string);
    //    let (string, lines, range) = source_map.resolve_span_within_line(span);
    //    let snippet = Snippet {
    //        title: Some(Annotation {
    //            id: None,
    //            label: Some("This is a test".to_string()),
    //            annotation_type: AnnotationType::Warning,
    //        }),
    //        footer,
    //        slices: vec![Slice {
    //            source: string,
    //            line_start: lines as usize,
    //            origin: Some("macros.va".to_string()),
    //            annotations: vec![SourceAnnotation {
    //                range: (range.start, range.end),
    //                label: "here".to_string(),
    //                annotation_type: AnnotationType::Warning,
    //            }],
    //            fold: false,
    //        }],
    //    };
    //    let dl = DisplayList::from(snippet);
    //    let dlf = DisplayListFormatter::new(true, false);
    //    println!("{}", dlf.format(&dl));
    Ok(())
}

#[test]
pub fn test_source_map() {
    let source_map_allocator = Bump::new();
    let source_map_builder_allocator = Bump::new();
    let (mut builder, mut lexer) = SourceMapBuilder::new(
        &source_map_allocator,
        &source_map_builder_allocator,
        Path::new("tests/source_map.va"),
    )
    .expect("IoError");
    for _ in 0..6 {
        lexer.advance();
    }
    builder.new_line();
    builder.new_line();
    let span: Span = lexer.range().into();
    builder.enter_root_macro(lexer.range().start, Span::new(0, 0), "BAR", 2, "test");
    builder.finish_substitution();
    let source_map = builder.done();
    let span = Span::new_with_length(span.get_start(), 3);
    let (string, lines, _) = source_map.resolve_span(span, true);
    assert_eq!(string, "BAR");
    assert_eq!(lines, 3);
}
