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
use std::pin::Pin;
use std::ptr::NonNull;
use std::thread::Builder;

use bumpalo::Bump;
use intrusive_collections::__core::cell::Cell;
use intrusive_collections::rbtree::{Cursor, CursorMut};
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
    main_file_name: &'source_map str,
    expanded_source: &'source_map str,
    children: RBTree<SourceMapAdapter<'source_map>>,
    map: RBTree<SourceMapAdapter<'source_map>>,
    _pin_marker: PhantomPinned,
}

impl<'source_map> SourceMap<'source_map> {
    pub fn main_file_name(&self) -> &'source_map str {
        self.main_file_name
    }
    pub fn main_file_contents(&self) -> &'source_map str {
        self.main_file_contents
    }
    pub fn new(main_file_name: &'source_map str) -> Self {
        Self {
            main_file_name,
            expanded_source: "",
            children: Default::default(),
            map: RBTree::new(SourceMapAdapter::new()),
            _pin_marker: PhantomPinned,
        }
    }

    fn string_to_newline(
        &self,
        reverse: bool,
        cursor: &mut Cursor<SourceMapAdapter<'source_map>>,
        start: Index,
    ) -> (String, Index, bool, bool) {
        let main_file = self.main_file_contents;
        let mut res = String::new();
        let mut last_end = if let Some(first) = cursor.get() {
            //defined here because the loop needs to be initialized
            if first.end.get() >= start {
                //inside substitution append until newline or beginning/end of first
                let start = start - first.start;
                let string = first.contents.get();
                let string = if reverse {
                    &string[..start as usize]
                } else {
                    &string[start as usize..]
                };
                if let Some(index) = Self::append_until_newline_in_str(string, &mut res, reverse) {
                    return (res, index, false, true);
                }
                if reverse {
                    cursor.move_prev();
                    first.original_span.get_start()
                } else {
                    cursor.move_next();
                    first.original_span.get_end()
                }
            } else {
                if !reverse {
                    cursor.move_next()
                }
                start - first.end.get() + first.original_span.get_end()
            }
        } else {
            //the cursor is pointing at the mainfile
            if reverse {
                start
            } else {
                cursor.move_prev();
                let res = if let Some(current) = cursor.get() {
                    let len = start - current.end.get();
                    len + current.original_span.get_end()
                } else {
                    start
                };
                cursor.move_next();
                res
            }
        };

        loop {
            let current = if let Some(current) = cursor.get() {
                //check if newlines is in space between substitutions
                let range = if reverse {
                    Range {
                        start: current.original_span.get_end() as usize,
                        end: last_end as usize,
                    }
                } else {
                    Range {
                        start: last_end as usize,
                        end: current.original_span.get_start() as usize,
                    }
                };
                if let Some(index) =
                    Self::append_until_newline_in_str(&main_file[range], &mut res, reverse)
                {
                    return (res, index, true, false);
                }
                current
            } else {
                //we have passed the last substitution and are now inside the main file
                let end_slice = if reverse {
                    &main_file[0..last_end as usize]
                } else {
                    &main_file[last_end as usize..main_file.len()]
                };
                if let Some(index) = Self::append_until_newline_in_str(end_slice, &mut res, reverse)
                {
                    return (res, index, true, false);
                } else {
                    return (res, start, true, false);
                }
            };

            last_end = if reverse {
                let start = current.original_span.get_start();
                cursor.move_prev();
                start
            } else {
                let end = current.original_span.get_end();
                cursor.move_next();
                end
            };

            if let Some(index) =
                Self::append_until_newline_in_str(current.contents.get(), &mut res, reverse)
            {
                return (res, index, false, false);
            }
        }
    }

    fn append_until_newline_in_str(
        string: &str,
        dest: &mut String,
        reverse: bool,
    ) -> Option<Index> {
        let mut characters = string.as_bytes().iter().enumerate();
        if reverse {
            let res = characters
                .rev()
                .find(|(_, c)| **c == b'\n')
                .map(|(index, _)| index as Index);
            if let Some(index) = res {
                dest.push_str(&string[index as usize + 1..]);
            } else {
                dest.push_str(&string)
            }
            res
        } else {
            characters
                .find(|(_, c)| {
                    if **c == b'\n' {
                        true
                    } else {
                        dest.push(**c as char);
                        false
                    }
                })
                .map(|(index, _)| index as Index)
        }
    }

    pub fn resolve_span_within_line(
        &self,
        span: Span,
    ) -> (String, LineNumber, Option<(String)>, Range<Index>) {
        let mut start_cursor = self.map.upper_bound(Bound::Included(&span.get_start()));
        let mut end_cursor = start_cursor.clone();

        let (mut head_content, head_offset, head_in_root, start_stayed_in_substitution) =
            self.string_to_newline(true, &mut start_cursor, span.get_start());
        let main_content = self.resolve_span_internal(span, &mut end_cursor);
        let (tail_content, _, _, end_stayed_in_substitution) =
            self.string_to_newline(false, &mut end_cursor, span.get_end());
        let range = Range {
            start: head_content.len() as Index,
            end: head_content.len() as Index + main_content.len() as Index,
        };
        head_content.push_str(main_content.as_str());
        head_content.push_str(tail_content.as_str());

        let head_offset = head_offset + 1; //we need to include the last line in the counting as well
        let mut containing_expansion_description = None;
        let line_number = if let Some(start_substitution) = start_cursor.get() {
            if head_in_root {
                let range: Range<usize> = Range {
                    start: start_substitution.original_span.get_end() as usize,
                    end: start_substitution.original_span.get_end() as usize + head_offset as usize,
                };
                let string_to_count = &self.main_file_contents[range];
                start_substitution.original_last_line
                    + bytecount::count(string_to_count.as_bytes(), b'\n') as LineNumber
            } else {
                /* TODO keep track of position within substitution
                let string_to_count =
                    &start_substitution.contents.get())[..head_offset as usize];*/
                if start_stayed_in_substitution && end_stayed_in_substitution {
                    let column = span.get_start()
                        - *&self.main_file_contents
                            [..start_substitution.original_span.get_start() as usize]
                            .rfind("\n")
                            .unwrap() as Index;
                    let (mut name, line_offset) = match start_substitution.stype {
                        SourceType::File => (
                            format!(
                                "{}:{}:{} Occurred inside include expansion\n --> {}",
                                self.main_file_name,
                                start_substitution.original_first_line,
                                column,
                                start_substitution.name
                            ),
                            0,
                        ),
                        SourceType::Macro { definition_line } => (
                            format!(
                                "{}:{}:{} Occurred inside macro expansion of {}\n --> {}",
                                self.main_file_name,
                                start_substitution.original_first_line,
                                column,
                                start_substitution.name,
                                self.main_file_name
                            ),
                            definition_line,
                        ),
                    };
                    containing_expansion_description = Some(name);
                    line_offset
                        + bytecount::count(
                            &start_substitution.contents.get()[..head_offset as usize].as_bytes(),
                            b'\n',
                        ) as LineNumber
                } else {
                    start_substitution.original_first_line
                }
            }
        } else {
            let str_to_count = &self.main_file_contents[..head_offset as usize];
            bytecount::count(str_to_count.as_bytes(), b'\n') as LineNumber
        };
        (
            head_content,
            line_number + 1,
            containing_expansion_description,
            range,
        )
    }
    pub fn resolve_span(&self, span: Span) -> (String, LineNumber) {
        let mut start_cursor = self.map.upper_bound(Bound::Included(&span.get_start()));
        let line_number = if let Some(start_substitution) = start_cursor.get() {
            if start_substitution.end.get() > span.get_start() {
                //we are inside an substitution
                start_substitution.original_first_line
            } else {
                //we are in the main file
                let end = (start_substitution.original_span.get_end()
                    + (span.get_end() - start_substitution.end.get()))
                    as usize; //avoids overflows
                let string_to_count = &self.main_file_contents
                    [start_substitution.original_span.get_end() as usize..end];
                bytecount::count(string_to_count.as_bytes(), b'\n') as LineNumber
                    + start_substitution.original_last_line
            }
        } else {
            //there is no substitution before we will just need to count from the beginning
            bytecount::count(
                &self.main_file_contents[..span.get_start() as usize].as_bytes(),
                b'\n',
            ) as LineNumber
        };
        let res = self.resolve_span_internal(span, &mut start_cursor);
        (res, line_number + 1)
    }

    fn resolve_span_internal(
        &self,
        span: Span,
        cursor: &mut Cursor<SourceMapAdapter<'source_map>>,
    ) -> String {
        let main_file = self.main_file_contents;
        let mut res = String::new();
        let mut current_main_range = if let Some(first_substitution) = cursor.get() {
            let start = (span.get_start() - first_substitution.start) as usize;
            if first_substitution.end.get() >= span.get_end() {
                let range = Range {
                    start,
                    end: (span.get_end() - first_substitution.start) as usize,
                };
                return first_substitution.contents.get()[range].to_string();
            }
            if first_substitution.end.get() < span.get_start() {
                Range {
                    start: (span.get_start() - first_substitution.end.get()
                        + first_substitution.original_span.get_end())
                        as usize,
                    end: 0,
                }
            } else {
                res.push_str(&first_substitution.contents.get()[start..]);
                Range {
                    start: first_substitution.original_span.get_end() as usize,
                    end: 0,
                }
            }
        } else {
            match cursor.peek_next().get() {
                Some(first_substitution) if first_substitution.start < span.get_end() => {
                    //we are before the first substitution
                    Range {
                        start: span.get_start() as usize,
                        end: 0,
                    }
                }
                _ => {
                    //there are no substitutions so the span is just a normal range
                    let range: Range<usize> = span.into();
                    return main_file[range].to_string();
                }
            }
        };
        while let Some(current_substitution) = {
            cursor.move_next();
            cursor.get()
        } {
            if current_substitution.end.get() >= span.get_end() {
                break;
            }
            current_main_range.end = current_substitution.original_span.get_start() as usize;
            res.push_str(&main_file[current_main_range]);
            res.push_str(current_substitution.contents.get());

            current_main_range = Range {
                start: current_substitution.original_span.get_end() as usize,
                end: 0,
            };
        }
        if let Some(last_substitution) = cursor.get() {
            current_main_range.end = last_substitution.original_span.get_start() as usize;
            res.push_str(&main_file[current_main_range]);
            let end = span.get_end() - last_substitution.start;
            res.push_str(&last_substitution.contents.get()[..end as usize]);
        } else {
            //Inside main file
            cursor.move_prev();
            let last_substitution = cursor.get().unwrap();
            let len = span.get_end() - last_substitution.end.get();
            current_main_range.end = (last_substitution.original_span.get_end() + len) as usize;
            res.push_str(&main_file[current_main_range]);
        }
        res //TODO show what substitution an error came from (in case of multiple)
    }
}

#[derive(Debug)]
struct SourceMapBuilderState<'lt> {
    source: &'lt str,
    offset: Index,
}

#[derive(Debug)]
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
        let name = builder_allocator.alloc_str(main_file.to_str().unwrap());
        let source_map = source_map_allocator.alloc_with(|| SourceMap::new(name));
        let mut res = Self {
            source_map_allocator,
            allocator: builder_allocator,
            source_map: NonNull::from(source_map),
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
        unsafe { &*self.source_map.as_ptr() }
    }

    pub(super) fn new_line(&mut self) {
        if self.substitution_stack.is_empty() {
            //we only keep track of macro expansion independent line numbers in the mainfile
            self.root_line += 1;
        }
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
            let name = &*self.allocator.alloc_str(name);
            let range: Range<usize> = original_span.into();
            let original_source = &self.source_map().main_file_contents[range];
            let original_lines = bytecount::count(original_source.as_bytes(), b'\n') as LineNumber;
            let root_line = self.root_line;
            self.allocator.alloc_with(|| Substitution {
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
        self.cursor.insert_after(substitution);
        self.expansion.reserve(source.len()); //Expansions are typically longer than their names (they would be pointless otherwise)
        self.substitution_stack()
            .push(SourceMapBuilderState { source, offset: 0 })
    }

    pub(super) fn enter_non_root_substitution(&mut self, original_span: Span, source: &'lt str) {
        unsafe { self.get_unchecked_mut() }
            .enter_non_root_substitution_internal(original_span, source)
        //this is save since eveything unsed in that function isn't pinned structurally and we dont move self
    }
    fn enter_non_root_substitution_internal(&mut self, original_span: Span, source: &'lt str) {
        let parent_src_state = self.substitution_stack.last_mut().unwrap();
        let old_offset = parent_src_state.offset as usize;
        parent_src_state.offset = original_span.get_end();
        self.expansion
            .push_str(&parent_src_state.source[old_offset..original_span.get_start() as usize]);
        self.substitution_stack
            .push(SourceMapBuilderState { source, offset: 0 });
    }

    const EMPTY_STACK: &'static str = "SourceBuilder: Substitution stack is empty";
    const NO_ROOT_SUBSTITUTION: &'static str = "SourceBuilder: Empty substitution";
    pub(super) fn finish_substitution(&mut self) -> Index {
        let SourceMapBuilderState { source, offset } =
            self.substitution_stack.pop().expect(Self::EMPTY_STACK);
        let remaining_str = &source[offset as usize..];
        self.expansion.push_str(remaining_str);

        if self.substitution_stack.is_empty() {
            self.cursor
                .get()
                .unwrap()
                .end
                .set(substitution.start + contents.len() as Index);
        }
        source.len() as Index
    }

    pub(crate) fn enter_file(
        mut self: Pin<&mut Self>,
        path: &Path,
        start: Index,
        original_span: Span,
    ) -> std::io::Result<Lexer<'lt>> {
        let contents = std::fs::read_to_string(path)?;
        let contents = &*self.allocator.alloc_str(&contents);
        if self.substitution_stack.is_empty() {
            self.as_mut().enter_root_substitution(
                start,
                SourceType::File,
                original_span,
                contents,
                path.to_str().unwrap(),
            );
        } else {
            self.enter_non_root_substitution(original_span, contents)
        }
        Ok(Lexer::new(lexer_str))
    }

    pub(super) fn enter_root_macro(
        mut self: Pin<&mut Self>,
        start: Index,
        original_span: Span,
        definition: &'lt str,
        name: &str,
    ) {
        let definition_line = self.root_line;
        self.enter_root_substitution(
            start,
            SourceType::Macro { definition_line },
            original_span,
            definition,
            name,
        )
    }
    pub(super) fn source(&self) -> &'lt str {
        if let Some(state) = self.substitution_stack.last() {
            state.source
        } else {
            self.root_file_contents
        }
    }
}
