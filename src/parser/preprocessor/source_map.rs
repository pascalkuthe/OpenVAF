/*
 * ******************************************************************************************
 * Copyright (c) 2019 Pascal Kuthe. This file is part of the VARF project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/jamescoding/VARF/blob/master/LICENSE.
 *  No part of VARF, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */
use std::marker::{PhantomData, PhantomPinned};
use std::ops::Range;
use std::path::Path;
use std::ptr::NonNull;

use bumpalo::Bump;
use intrusive_collections::__core::cell::Cell;
use intrusive_collections::rbtree::CursorMut;
use intrusive_collections::{Bound, KeyAdapter, RBTree, RBTreeLink};

use crate::span::{Index, LineNumber};
use crate::{Lexer, Span};

pub type ArgumentIndex = u8;
pub type CallDepth = u8;

//Map Declarations/

intrusive_adapter!(SourceMapAdapter<'source_map> = &'source_map Substitution<'source_map> : Substitution<'source_map> {link:RBTreeLink});
impl<'source_map, 'lt> KeyAdapter<'lt> for SourceMapAdapter<'source_map> {
    type Key = Index;

    fn get_key(&self, value: &'lt Self::Value) -> Self::Key {
        value.start
    }
}

#[derive(Debug)]
pub(crate) struct Substitution<'source_map> {
    name: &'source_map str,
    start: Index,
    end: Cell<Index>,
    stype: SourceType,
    original_span: Span,
    original_first_line: LineNumber,
    original_last_line: LineNumber,
    link: RBTreeLink, //    children: RBTree<LocationMapAdapter>,
}
#[derive(Debug)]
pub(crate) enum SourceType {
    Macro { definition_line: LineNumber },
    File,
}
#[derive(Debug)]
pub struct SourceMap<'source_map> {
    pub main_file_name: &'source_map str,
    pub expanded_source: &'source_map str,
    children: RBTree<SourceMapAdapter<'source_map>>,
    map: RBTree<SourceMapAdapter<'source_map>>,
    _pin_marker: PhantomPinned,
}

impl<'source_map> SourceMap<'source_map> {
    pub fn new(main_file_name: &'source_map str) -> Self {
        Self {
            main_file_name,
            expanded_source: "",
            children: Default::default(),
            map: RBTree::new(SourceMapAdapter::new()),
            _pin_marker: PhantomPinned,
        }
    }

    pub fn resolve_span_within_line(
        &self,
        span: Span,
        translate_lines: bool,
    ) -> (&'source_map str, LineNumber, Option<String>, Range<Index>) {
        let expansion_end = self.expanded_source.len();

        let start = self.expanded_source[..span.get_start() as usize]
            .rfind('\n')
            .map_or(0, |line_pos| line_pos + 1);
        //we don't want to include the newline
        let end = self.expanded_source[span.get_end() as usize..]
            .find('\n')
            .unwrap_or(expansion_end);
        let range = Range {
            start: span.get_start() - start as Index,
            end: span.get_end() - start as Index,
        };
        let end = span.get_end() as usize + end;

        let mut containing_expansion_description = None;
        let cursor = self.map.upper_bound(Bound::Included(&(start as Index)));
        let line_number = if let Some(substitution) = cursor.get() {
            match substitution.end.get() as usize {
                previous_end if translate_lines && (0..=start).contains(&previous_end) => {
                    substitution.original_last_line
                        + bytecount::count(
                            self.expanded_source[previous_end..start].as_bytes(),
                            b'\n',
                        ) as LineNumber
                }
                start_substitution_end
                    if translate_lines && (0..end).contains(&start_substitution_end) =>
                {
                    substitution.original_last_line
                        - bytecount::count(
                            self.expanded_source[span.get_start() as usize..start_substitution_end]
                                .as_bytes(),
                            b'\n',
                        ) as LineNumber
                }
                parent_substitution_end
                    if translate_lines
                        && (end..=expansion_end).contains(&parent_substitution_end) =>
                {
                    let mut column: u16 = 0;
                    for &byte in self.expanded_source
                        [..substitution.original_span.get_start() as usize]
                        .as_bytes()
                        .iter()
                        .rev()
                    {
                        if byte == b'\n' && (byte >> 6) != 0b10 {
                            break;
                        }
                        column += ((byte >> 6) != 0b10) as u16
                    } //This is here so that non ASCII characters don't count as multiple characters. Its done this way and not using iterators for performance reasons (see std reverse count method for explanation of a similar optimization)
                    let (name, line_offset) = match substitution.stype {
                        SourceType::File => (
                            format!(
                                "{}:{}:{} Occurred inside include expansion\n --> {}",
                                self.main_file_name,
                                substitution.original_first_line + 1,
                                column,
                                substitution.name
                            ),
                            0,
                        ),
                        SourceType::Macro { definition_line } => (
                            format!(
                                "{}:{}:{} Occurred inside macro expansion of `{}\n --> {}",
                                self.main_file_name,
                                substitution.original_first_line + 1,
                                column,
                                substitution.name,
                                self.main_file_name
                            ),
                            definition_line,
                        ),
                    };
                    containing_expansion_description = Some(name);
                    if translate_lines {
                        let bytes_to_substitution_start =
                            self.expanded_source[substitution.start as usize..start].as_bytes();
                        line_offset
                            + bytecount::count(bytes_to_substitution_start, b'\n') as LineNumber
                    } else {
                        bytecount::count(self.expanded_source[..start].as_bytes(), b'\n')
                            as LineNumber
                    }
                }
                _ => {
                    debug_assert!(!translate_lines);
                    bytecount::count(self.expanded_source[..start].as_bytes(), b'\n') as LineNumber
                }
            }
        } else {
            let str_to_count = self.expanded_source[..start as usize].as_bytes();
            bytecount::count(str_to_count, b'\n') as LineNumber
        };
        (
            &self.expanded_source[start..end],
            line_number + 1,
            containing_expansion_description,
            range,
        )
    }
    pub fn resolve_span(
        &self,
        span: Span,
        translate_lines: bool,
    ) -> (
        &'source_map str,
        LineNumber,
        Option<(&'source_map str, LineNumber)>,
    ) {
        let expansion_end = self.expanded_source.len();
        let start = span.get_start() as usize;
        let end = span.get_end() as usize;
        let mut containing_expansion_description = None;
        let cursor = self.map.upper_bound(Bound::Included(&(start as Index)));
        let line_number = if let Some(substitution) = cursor.get() {
            match substitution.end.get() as usize {
                previous_end if translate_lines && (0..=start).contains(&previous_end) => {
                    substitution.original_last_line
                        + bytecount::count(
                            self.expanded_source[previous_end..start].as_bytes(),
                            b'\n',
                        ) as LineNumber
                }
                start_substitution_end
                    if translate_lines && (0..end).contains(&start_substitution_end) =>
                {
                    substitution.original_last_line
                        - bytecount::count(
                            self.expanded_source[start..start_substitution_end].as_bytes(),
                            b'\n',
                        ) as LineNumber
                }
                parent_substitution_end
                    if translate_lines
                        && (end..=expansion_end).contains(&parent_substitution_end) =>
                {
                    let (name, line_offset) = match substitution.stype {
                        SourceType::File => (substitution.name, 0),
                        SourceType::Macro { definition_line } => {
                            (substitution.name, definition_line)
                        }
                    };
                    containing_expansion_description =
                        Some((name, substitution.original_first_line));
                    if translate_lines {
                        let bytes_to_substitution_start =
                            self.expanded_source[substitution.start as usize..start].as_bytes();
                        line_offset
                            + bytecount::count(bytes_to_substitution_start, b'\n') as LineNumber
                    } else {
                        bytecount::count(self.expanded_source[..start].as_bytes(), b'\n')
                            as LineNumber
                    }
                }
                _ => {
                    debug_assert!(!translate_lines);
                    bytecount::count(self.expanded_source[..start].as_bytes(), b'\n') as LineNumber
                }
            }
        } else {
            let str_to_count = self.expanded_source[..start as usize].as_bytes();
            bytecount::count(str_to_count, b'\n') as LineNumber
        };
        (
            &self.expanded_source[start..end],
            line_number + 1,
            containing_expansion_description,
        )
    }
}

#[derive(Debug)]
struct SourceMapBuilderState<'lt> {
    source: &'lt str,
    offset: Index,
}

pub(super) struct SourceMapBuilder<'lt, 'source_map> {
    source_map_allocator: &'source_map Bump,
    allocator: &'lt Bump,
    cursor: CursorMut<'source_map, SourceMapAdapter<'source_map>>,
    expansion: String,
    substitution_stack: Vec<SourceMapBuilderState<'lt>>,
    source_map: NonNull<SourceMap<'source_map>>,
    root_line: LineNumber,
    root_file_contents: &'lt str,
    _phantom_data: PhantomData<CursorMut<'source_map, SourceMapAdapter<'source_map>>>,
}

impl<'lt, 'source_map> SourceMapBuilder<'lt, 'source_map> {
    pub(super) fn new(
        source_map_allocator: &'source_map Bump,
        builder_allocator: &'lt Bump,
        main_file: &Path,
    ) -> std::io::Result<(Self, Lexer<'lt>)> {
        let root_file_contents = builder_allocator.alloc_str(&std::fs::read_to_string(main_file)?);
        let name = &*source_map_allocator.alloc_str(main_file.to_str().unwrap());
        let source_map = source_map_allocator.alloc_with(move || SourceMap::new(name));
        let res = Self {
            source_map_allocator,
            allocator: builder_allocator,
            source_map: NonNull::from(&*source_map),
            cursor: source_map.map.cursor_mut(),
            root_line: 0,
            substitution_stack: Vec::new(),
            expansion: "".to_string(),
            root_file_contents,
            _phantom_data: Default::default(),
        };
        Ok((res, Lexer::new(root_file_contents)))
    }

    pub fn done(self) -> &'source_map SourceMap<'source_map> {
        let mut string = bumpalo::collections::String::new_in(self.source_map_allocator);
        string.push_str(self.expansion.as_str());
        string.push_str(&self.root_file_contents[self.get_current_root_offset()..]);
        let res = unsafe { &mut *self.source_map.as_ptr() }; //this is save since we know that the source_map will outlive the builder since its allocated in the arena which is guaranteed to live for 'sorcemap (its only a pointer so we can have a mutable reference to it in the form of cursor while remebering its location in the arena which wont be used past this point)
        res.expanded_source = string.into_bump_str();
        &*res
    }

    fn get_current_root_offset(&self) -> usize {
        self.cursor.get().map_or(0, |substitution| {
            substitution.original_span.get_end() as usize
        })
    }

    pub(super) fn new_line(&mut self) {
        if self.substitution_stack.is_empty() {
            //we only keep track of macro expansion independent line numbers in the mainfile
            self.root_line += 1;
        }
    }

    pub(super) fn current_root_line(&mut self) -> LineNumber {
        self.root_line
    }

    fn enter_root_substitution(
        &mut self,
        start: Index,
        stype: SourceType,
        original_span: Span,
        source: &'lt str,
        name: &str,
    ) {
        let substitution = {
            let name = &*self.source_map_allocator.alloc_str(name);
            let range: Range<usize> = original_span.into();
            let tmp = self.root_file_contents.len() < range.end;
            let l = self.root_file_contents.len();
            let original_source = &self.root_file_contents[range];
            let original_lines = bytecount::count(original_source.as_bytes(), b'\n') as LineNumber;
            let root_line = self.root_line;
            self.source_map_allocator.alloc_with(|| Substitution {
                name,
                start,
                end: Cell::new(0),
                stype,
                original_span,
                original_first_line: root_line,
                original_last_line: root_line + original_lines,
                link: RBTreeLink::new(),
            })
        };
        let preceding_root_slice_start = self.get_current_root_offset();
        let preceding_root_slice_end =
            self.cursor
                .get()
                .map_or(original_span.get_start() as usize, |substitution| {
                    original_span.get_start() as usize - preceding_root_slice_start
                        + substitution.original_span.get_end() as usize
                });
        self.expansion.push_str(
            &self.root_file_contents[preceding_root_slice_start..preceding_root_slice_end],
        );
        self.cursor.insert_after(substitution);
        self.cursor.move_next();
        self.expansion.reserve(source.len()); //Expansions are typically longer than their names (they would be pointless otherwise)
        self.substitution_stack
            .push(SourceMapBuilderState { source, offset: 0 })
    }

    pub(super) fn enter_non_root_substitution(&mut self, original_span: Span, source: &'lt str) {
        debug_assert!(self.substitution_stack.len() != 0);
        let parent_src_state = self.substitution_stack.last_mut().unwrap();
        let old_offset = parent_src_state.offset as usize;
        parent_src_state.offset = original_span.get_end();
        self.expansion
            .push_str(&parent_src_state.source[old_offset..original_span.get_start() as usize]);
        self.substitution_stack
            .push(SourceMapBuilderState { source, offset: 0 });
    }

    /// This function is called when the end of any substitution (macro / file include) is reached
    /// # Returns
    /// The distance between the start and the end position of the substitution.
    /// Note: This is not necessarily the same as the length of the original source text of the substitution
    pub(super) fn finish_substitution(&mut self) -> Index {
        let SourceMapBuilderState { source, offset } = self
            .substitution_stack
            .pop()
            .expect("SourceBuilder: Substitution stack is empty");

        let remaining_str = &source[offset as usize..];

        self.expansion.push_str(remaining_str);

        if self.substitution_stack.is_empty() {
            self.cursor
                .get()
                .unwrap()
                .end
                .set(self.expansion.len() as Index);
        }

        source.len() as Index
    }

    /// This Function is called when a new File is entered (using the `include` directive).
    pub(crate) fn enter_file(
        &mut self,
        path: &Path,
        start: Index,
        original_span: Span,
    ) -> std::io::Result<Lexer<'lt>> {
        let contents = std::fs::read_to_string(path)?;
        let contents = &*self.allocator.alloc_str(&contents);

        if self.substitution_stack.is_empty() {
            self.enter_root_substitution(
                start,
                SourceType::File,
                original_span,
                contents,
                path.to_str().unwrap(),
            );
        } else {
            self.enter_non_root_substitution(original_span, contents)
        }

        Ok(Lexer::new(contents))
    }

    ///  This function is called by the preprocessor to indicate that a Macro reference has been encountered and that tokens will now be consumed from this Macro
    pub(super) fn enter_macro(
        &mut self,
        start: Index,
        original_span: Span,
        definition: &'lt str,
        definition_line: LineNumber,
        name: &str,
    ) {
        if self.substitution_stack.is_empty() {
            self.enter_root_substitution(
                start,
                SourceType::Macro { definition_line },
                original_span,
                definition,
                name,
            )
        } else {
            self.enter_non_root_substitution(original_span, definition)
        }
    }

    pub(super) fn source(&self) -> &'lt str {
        if let Some(state) = self.substitution_stack.last() {
            state.source
        } else {
            self.root_file_contents
        }
    }
}
