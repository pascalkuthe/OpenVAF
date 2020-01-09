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

use intrusive_collections::rbtree::{Cursor, CursorMut};
use intrusive_collections::{Bound, KeyAdapter, RBTree, RBTreeLink, UnsafeRef};
use logos::Source;
use sr_alloc::{Allocator, NodeId, StrId};

use crate::span::{Index, LineNumber};
use crate::{Lexer, Span};

pub type ArgumentIndex = u8;
pub type CallDepth = u8;

//Map Declarations/

intrusive_adapter!(SourceMapAdapter = UnsafeRef<Substitution> : Substitution {link:RBTreeLink});
impl<'a> KeyAdapter<'a> for SourceMapAdapter {
    type Key = Index;

    fn get_key(&self, value: &'a Self::Value) -> Self::Key {
        value.start
    }
}

#[derive(Debug)]
pub(crate) struct Substitution {
    start: Index,
    end: Index,
    contents: StrId,
    stype: SourceType,
    original_span: Span,
    original_first_line: LineNumber,
    original_last_line: LineNumber,
    link: RBTreeLink, //    children: RBTree<LocationMapAdapter>,
}
#[derive(Debug)]
pub(crate) enum SourceType {
    Macro { definition_span: Span },
    File,
}
#[derive(Debug)]
pub struct SourceMap {
    allocator: Allocator,
    main_file_name: StrId,
    main_file_contents: StrId,
    children: RBTree<SourceMapAdapter>,
    map: RBTree<SourceMapAdapter>,
    _pin_marker: PhantomPinned,
}

impl SourceMap {
    pub fn new(main_file_name: &str, main_file_contents: &str) -> Self {
        let allocator = Allocator::new();
        let main_file_name = allocator.alloc_str_copy(main_file_name);
        let main_file_contents = allocator.alloc_str_copy(main_file_contents);
        Self {
            allocator,
            main_file_name,
            main_file_contents,
            children: Default::default(),
            map: RBTree::new(SourceMapAdapter::new()),
            _pin_marker: PhantomPinned,
        }
    }
    pub fn allocator(&self) -> &Allocator {
        &self.allocator
    }

    fn string_to_newline(
        &self,
        reverse: bool,
        cursor: &mut Cursor<SourceMapAdapter>,
        start: Index,
    ) -> (String, Index, bool) {
        let main_file = self.allocator.get_str(self.main_file_contents);
        let mut res = String::new();
        let mut last_end = if let Some(first) = cursor.get() {
            //defined here because the loop needs to be initialized
            let last_end = if reverse {
                first.original_span.get_start()
            } else {
                first.original_span.get_end()
            };
            if first.end > start {
                //if inside substitution append until newline in there first
                let start = start - first.start;
                let string = self.allocator.get_str(first.contents);
                let string = if reverse {
                    &string[..start as usize]
                } else {
                    &string[start as usize..]
                };
                if let Some(index) = Self::append_until_newline_in_str(string, &mut res, reverse) {
                    return (res, index, false);
                }
            };
            if reverse {
                cursor.move_prev()
            } else {
                cursor.move_next()
            }
            last_end
        } else {
            //the cursor is pointing at the mainfile
            if reverse {
                //get the first /last substitution
                cursor.move_next();
            } else {
                cursor.move_prev();
            };
            let res = match cursor.get() {
                Some(current) if !reverse => {
                    let len = start - current.end;
                    len + current.original_span.get_end()
                }
                _ => start, //there are either no substitutions or we are at the beginning of the main file; there is no need for translation in both cases
            };
            if reverse {
                //move the cursor back
                cursor.move_prev()
            } else {
                cursor.move_next()
            }
            res
        };

        loop {
            let (string, current) = if let Some(current) = cursor.get() {
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
                    return (res, index, true);
                }
                (self.allocator().get_str(current.contents), current)
            } else {
                //we have passed the last substitution and are now inside the main file
                let start = if reverse { 0 } else { main_file.len() as Index };
                if let Some(index) = Self::append_until_newline_in_str(
                    &main_file[start as usize..last_end as usize],
                    &mut res,
                    reverse,
                ) {
                    return (res, index, true);
                } else {
                    return (res, start, true);
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

            if let Some(index) = Self::append_until_newline_in_str(string, &mut res, reverse) {
                return (res, index, false);
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
                .find(|(_, c)| if **c == b'\n' { true } else { false })
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

    pub fn resolve_span_within_line(&self, span: Span) -> (String, LineNumber, Range<usize>) {
        let mut start_cursor = self.map.upper_bound(Bound::Included(&span.get_start()));
        let mut end_cursor = start_cursor.clone();

        let (mut head_content, head_offset, head_in_root) =
            self.string_to_newline(true, &mut start_cursor, span.get_start());
        let main_content = self.resolve_span_internal(span, &mut end_cursor);
        let (tail_content, _, _) = self.string_to_newline(false, &mut end_cursor, span.get_end());
        let range = Range {
            start: head_content.len(),
            end: head_content.len() + main_content.len(),
        };
        head_content.push_str(main_content.as_str());
        head_content.push_str(tail_content.as_str());

        let line_number = if let Some(start_substitution) = start_cursor.get() {
            if head_in_root {
                let range: Range<usize> = Range {
                    start: start_substitution.original_span.get_end() as usize,
                    end: start_substitution.original_span.get_end() as usize + head_offset as usize,
                };
                let string_to_count = &self.allocator.get_str(self.main_file_contents)[range];
                start_substitution.original_last_line
                    + bytecount::count(string_to_count.as_bytes(), b'\n') as LineNumber
            } else {
                let string_to_count =
                    &self.allocator.get_str(start_substitution.contents)[..head_offset as usize];
                start_substitution.original_last_line
                    + bytecount::count(string_to_count.as_bytes(), b'\n') as LineNumber
            }
        } else {
            bytecount::count(
                &self.allocator.get_str(self.main_file_contents)[..head_offset as usize].as_bytes(),
                b'\n',
            ) as LineNumber
        };
        (head_content, line_number, range)
    }
    pub fn resolve_span(&self, span: Span) -> (String, LineNumber) {
        let mut start_cursor = self.map.upper_bound(Bound::Included(&span.get_start()));
        let line_number = if let Some(start_substitution) = start_cursor.get() {
            if start_substitution.end > span.get_start() {
                //we are inside an substitution
                start_substitution.original_first_line
            } else {
                //we are in the main file
                let end = (start_substitution.original_span.get_end()
                    + (span.get_end() - start_substitution.end)) as usize; //avoids overflows
                let string_to_count = &self.allocator.get_str(self.main_file_contents)
                    [start_substitution.original_span.get_end() as usize..end];
                bytecount::count(string_to_count.as_bytes(), b'\n') as LineNumber
                    + start_substitution.original_last_line
            }
        } else {
            //there is no substitution before we will just need to count from the beginning
            bytecount::count(
                &self.allocator.get_str(self.main_file_contents)[..span.get_start() as usize]
                    .as_bytes(),
                b'\n',
            ) as LineNumber
        };
        let res = self.resolve_span_internal(span, &mut start_cursor);
        (res, line_number as LineNumber)
    }

    fn resolve_span_internal(&self, span: Span, cursor: &mut Cursor<SourceMapAdapter>) -> String {
        let main_file = self.allocator.get_str(self.main_file_contents);
        let mut res = String::new();
        let mut current_main_range = if let Some(first_substitution) = cursor.get() {
            let start = (span.get_start() - first_substitution.start) as usize;
            if first_substitution.end >= span.get_end() {
                let range = Range {
                    start,
                    end: (span.get_end() - first_substitution.start) as usize,
                };
                let res_id = first_substitution.contents.range(range);
                return self.allocator.get_str(res_id).to_string();
            }
            res.push_str(&self.allocator.get_str(first_substitution.contents)[start..]);
            Range {
                start: first_substitution.original_span.get_end() as usize,
                end: 0,
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
            if current_substitution.end >= span.get_end() {
                break;
            }
            current_main_range.end = current_substitution.original_span.get_start() as usize;
            res.push_str(&main_file[current_main_range]);
            res.push_str(self.allocator().get_str(current_substitution.contents));

            current_main_range = Range {
                start: current_substitution.original_span.get_end() as usize,
                end: 0,
            };
        }
        if let Some(last_substitution) = cursor.get() {
            current_main_range.end = last_substitution.original_span.get_start() as usize;
            res.push_str(&main_file[current_main_range]);
            let end = span.get_end() - last_substitution.start;
            res.push_str(&self.allocator.get_str(last_substitution.contents)[..end as usize]);
        } else {
            cursor.move_prev();
            let last_substitution = cursor.get().unwrap();
            let len = span.get_end() - last_substitution.end;
            current_main_range.end = (last_substitution.original_span.get_end() + len) as usize;
            res.push_str(&main_file[current_main_range])
        }
        res
    }
}

struct SourceMapBuilderState {
    source: String,
    offset: Index,
}

pub(super) struct SourceMapBuilder {
    cursor: NonNull<CursorMut<'static, SourceMapAdapter>>,
    current_source: Option<(NonNull<Substitution>, bumpalo::collections::String<'static>)>,
    substitution_stack: Vec<SourceMapBuilderState>,
    source_map: NonNull<SourceMap>,
    root_line: LineNumber,
    _phantom_data: PhantomData<CursorMut<'static, SourceMapAdapter>>,
    _pin_marker: PhantomPinned,
}

impl SourceMapBuilder {
    /// #Safety
    /// this returns a lexer with a static lifetime to avoid lifetime complications
    /// it is the callers responsibility to ensure the lexer doesnt outlive self
    pub(super) unsafe fn new(
        main_file: &Path,
    ) -> std::io::Result<(Pin<Box<Self>>, Lexer<'static>)> {
        //By pinning we guarantee that the SourceMap outlives self
        let mut res = Box::pin(Self {
            source_map: NonNull::dangling(),
            cursor: NonNull::dangling(),
            root_line: 1,
            current_source: None,
            substitution_stack: Vec::new(),
            _pin_marker: PhantomPinned,
            _phantom_data: Default::default(),
        });
        let name = main_file.to_str().unwrap();
        let contents = std::fs::read_to_string(main_file)?;
        let source_map_ptr = Box::into_raw(Box::new(SourceMap::new(name, contents.as_str()))); //This is save since the sourcemap doesnt get dropped by the builder its only returned when calling done
        res.as_mut().get_unchecked_mut().source_map = NonNull::new_unchecked(source_map_ptr); //this is save since we just created the ptr and it therefore cant be null
        let cursor = Box::into_raw(Box::new((&mut *source_map_ptr).map.cursor_mut())); //this is save since source_map cant be dropped by self so it will always outlive self and this is a save self refence
        let ptr = NonNull::new_unchecked(cursor); //we just created this pointer from a reference so it wont be null
        res.as_mut().get_unchecked_mut().cursor = ptr; //this is save since changing a single field doesnt move the whole struct
        let lexer_str = std::mem::transmute::<&str, &'static str>(res.source()); //unsafety occurs here
        Ok((res, Lexer::new(lexer_str)))
    }

    pub fn done(self: Pin<Box<Self>>) -> Box<SourceMap> {
        unsafe {
            Box::from_raw(self.source_map.as_ptr()) //This is save since we created SourceMap using a box and it cant be reassigned
        }
    }

    // Internal getters/projections
    fn substitution_stack(self: Pin<&mut Self>) -> &mut Vec<SourceMapBuilderState> {
        unsafe { &mut self.get_unchecked_mut().substitution_stack } //This is save since we don't treat source_stack as structurally pinned
    }
    // Internal getters/projections
    fn current_root_substitution(
        self: Pin<&mut Self>,
    ) -> &mut Option<(NonNull<Substitution>, bumpalo::collections::String<'static>)> {
        unsafe { &mut self.get_unchecked_mut().current_source } //This is save since we don't treat parent_locations as structurally pinned
    }
    pub(super) fn new_line(mut self: Pin<&mut Self>) {
        if self.current_source.is_none() {
            //we only keep track of macro expansion independent line numbers in the mainfile
            unsafe {
                self.as_mut().get_unchecked_mut().root_line += 1; //this is save since lines aren't pinned structurally
            }
        }
    }

    fn cursor(self: Pin<&mut Self>) -> &mut CursorMut<'static, SourceMapAdapter> {
        unsafe { self.get_unchecked_mut().cursor.as_mut() } //This is save since we do not pin cursor structurally and we own the pointer
    }
    fn source_map_mut(self: Pin<&mut Self>) -> Pin<&mut SourceMap> {
        unsafe { self.map_unchecked_mut(|s| s.source_map.as_mut()) } //This is save: just a projection and source_map remains valid as long as this method can becalled
    }

    fn source_map(&self) -> &SourceMap {
        unsafe { self.source_map.as_ref() } //This is save since the source_map outlives self
    }
    pub(super) fn allocator_mut(self: Pin<&mut Self>) -> Pin<&mut Allocator> {
        unsafe {
            self.source_map_mut()
                .map_unchecked_mut(|s| &mut s.allocator)
        }
        //This is just a projection (save)
    }
    pub(super) fn allocator(&self) -> &Allocator {
        self.source_map().allocator()
    }

    fn enter_root_substitution(
        mut self: Pin<&mut Self>,
        start: Index,
        stype: SourceType,
        original_span: Span,
        source: String,
    ) {
        let substitution = {
            let range: Range<usize> = original_span.into();
            let original_source = &self
                .allocator()
                .get_str(self.source_map().main_file_contents)[range];
            let original_lines = bytecount::count(original_source.as_bytes(), b'\n') as LineNumber;
            let root_line = self.root_line;
            self.as_mut()
                .allocator_mut()
                .alloc_node(move || Substitution {
                    start,
                    end: 0,
                    contents: StrId::dangling(),
                    stype,
                    original_span,
                    original_first_line: root_line,
                    original_last_line: root_line + original_lines,
                    link: RBTreeLink::new(),
                })
        };
        let mut string = unsafe {
            std::mem::transmute::<
                bumpalo::collections::String<'_>,
                bumpalo::collections::String<'static>,
            >(self.as_mut().allocator_mut().new_string())
        };
        string.reserve(source.len());
        *self.as_mut().current_root_substitution() = Some((substitution.into(), string));
        self.as_mut()
            .insert_into_root_substitution_map(substitution);
        self.substitution_stack()
            .push(SourceMapBuilderState { source, offset: 0 })
    }

    fn insert_into_root_substitution_map(
        mut self: Pin<&mut Self>,
        substitution: NodeId<Substitution>,
    ) {
        unsafe {
            self.as_mut()
                .cursor()
                .insert_after(UnsafeRef::from_raw(substitution.into())); //This is save since we guarantee that locations cant be deleted (pinning of the sourcemap which contains the allocator)
        }
        self.as_mut().cursor().move_next();
    }

    pub(super) fn enter_non_root_substitution(
        self: Pin<&mut Self>,
        original_span: Span,
        source: String,
    ) {
        unsafe { self.get_unchecked_mut() }
            .enter_non_root_substitution_internal(original_span, source)
        //this is save since eveything unsed in that function isn't pinned structurally and we dont move self
    }
    fn enter_non_root_substitution_internal(&mut self, original_span: Span, source: String) {
        //this is here because borrows cant be split with pins
        let old_offset = {
            let parent_src_state = self.substitution_stack.last_mut().unwrap();
            let old_offset = parent_src_state.offset as usize;
            parent_src_state.offset = original_span.get_end();
            old_offset
        };

        let old_source = &self.substitution_stack.last().unwrap().source;
        self.current_source
            .as_mut()
            .unwrap()
            .1
            .push_str(&old_source[old_offset..original_span.get_start() as usize]);
        let debug = self.current_source.as_ref().unwrap().1.as_str();
        self.substitution_stack
            .push(SourceMapBuilderState { source, offset: 0 });
    }

    const EMPTY_STACK: &'static str = "SourceBuilder: Substitution stack is empty";
    const NO_ROOT_SUBSTITUTION: &'static str = "SourceBuilder: Empty substitution";
    pub(super) fn finish_substitution(mut self: Pin<&mut Self>) -> Index {
        let finished_substitution = self
            .as_mut()
            .substitution_stack()
            .pop()
            .expect(Self::EMPTY_STACK);
        {
            let remaining_str =
                &finished_substitution.source[finished_substitution.offset as usize..];
            let current_substitution = self
                .as_mut()
                .current_root_substitution()
                .as_mut()
                .expect(Self::NO_ROOT_SUBSTITUTION);
            current_substitution.1.push_str(remaining_str);
        }
        if self.substitution_stack.is_empty() {
            let mut finished_root_substitution = None;
            std::mem::swap(
                &mut finished_root_substitution,
                self.as_mut().current_root_substitution(),
            );
            let (mut substitution, contents) =
                finished_root_substitution.expect(Self::NO_ROOT_SUBSTITUTION);
            unsafe {
                let substitution = substitution.as_mut();
                substitution.end = substitution.start + contents.len() as Index;
                let debug = contents.as_str();
                substitution.contents = self.allocator_mut().string_to_id_unchecked(contents);
                //this is save since we only ever insert substitutions never delete them
            }
        }
        finished_substitution.source.len() as Index
    }

    /// # Safety
    /// This returns a lexer that has an arbitrary lifetime it is up to the caller to ensure that it doesnt outlive finish_substitution call on the file
    pub(crate) unsafe fn enter_file<'res>(
        mut self: Pin<&mut Self>,
        path: &Path,
        start: Index,
        original_span: Span,
    ) -> std::io::Result<Lexer<'res>> {
        let contents = std::fs::read_to_string(path)?;
        let lexer_str = std::mem::transmute::<&str, &'res str>(contents.as_str()); //unsafety occures here
        if self.substitution_stack.is_empty() {
            self.as_mut()
                .enter_root_substitution(start, SourceType::File, original_span, contents);
        } else {
            self.enter_non_root_substitution(original_span, contents)
        }
        Ok(Lexer::new(lexer_str))
    }

    pub(super) fn enter_root_macro(
        self: Pin<&mut Self>,
        start: Index,
        original_span: Span,
        definition_span: Span,
        definition: String,
    ) {
        self.enter_root_substitution(
            start,
            SourceType::Macro { definition_span },
            original_span,
            definition,
        )
    }
    pub(super) fn source(&self) -> &str {
        if let Some(state) = self.substitution_stack.last() {
            state.source.as_str()
        } else {
            self.allocator()
                .get_str(self.source_map().main_file_contents)
        }
    }
}
impl Drop for SourceMapBuilder {
    fn drop(&mut self) {
        unsafe {
            //This is save since non_null is non owning so this wont cause a doube free
            Box::from_raw(self.cursor.as_ptr()); //box acts as an owning pointer so it will drop the cursor
        }
    }
}
