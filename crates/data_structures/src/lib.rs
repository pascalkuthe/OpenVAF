/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
*/

mod bit_set;
pub mod sync;
mod work_queue;
pub use bit_set::BitSet;
pub use bit_set::BitSetOperations;
pub use bit_set::HybridBitSet;
pub use bit_set::SparseBitSetMatrix;
pub use index_vec;
pub use work_queue::WorkQueue;

pub type HashMap<K, V> = ahash::AHashMap<K, V>;
pub type HashSet<T> = ahash::AHashSet<T>;
