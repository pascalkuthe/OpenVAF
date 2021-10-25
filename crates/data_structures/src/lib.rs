/*
 *  ******************************************************************************************
 *  Copyright (c) 2021 Pascal Kuthe. This file is part of the frontend project.
 *  It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 *  *****************************************************************************************
 */

pub mod bit_set;
pub mod sync;
mod work_queue;
pub use arrayvec;

pub use work_queue::WorkQueue;

pub mod index_vec;
pub mod iter;
pub mod paths;
mod scoped_arc_arena;
pub mod pretty;
pub use scoped_arc_arena::{ScopedArcArea,ScopedLrcArea,ScopedRcArea, ScopedArea};


pub use ahash::AHasher as Hasher;
pub use ahash::RandomState;

pub type HashMap<K, V> = ahash::AHashMap<K, V>;
pub type HashSet<T> = ahash::AHashSet<T>;
pub type IndexSet<T> = indexmap::IndexSet<T,RandomState>;
pub type IndexMap<T,V> = indexmap::IndexMap<T,V,RandomState>;

pub use text_size;
// pub use beef::lean::Cow;
pub use smallvec::{self,SmallVec};
pub use smol_str::SmolStr;

pub mod arena;




