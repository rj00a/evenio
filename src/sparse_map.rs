use core::fmt;
use std::mem::{self, ManuallyDrop};

use crate::bit_set::BitSet;
use crate::debug_checked::{assume_debug_checked, GetDebugChecked, UnwrapDebugChecked};
use crate::sparse::SparseIndex;

#[derive(Clone, Default)]
pub(crate) struct SparseMap<K, V> {
    // A `SparseMap` where the value type `V` is a ZST is functionally equivalent to a bitset, but
    // a bitset is more space efficient. We can employ this optimization in a zero-cost way by
    // using this union's `zst` field when `V` is a ZST, and falling back to the `non_zst` field
    // otherwise.
    union: Union<K, V>,
}

union Union<K, V> {
    zst: ManuallyDrop<ZstImpl<K, V>>,
    non_zst: ManuallyDrop<NonZstImpl<K, V>>,
}

impl<K, V> Clone for Union<K, V>
where
    K: Clone,
    V: Clone,
{
    fn clone(&self) -> Self {
        if is_zst::<V>() {
            let this = unsafe { &self.zst };

            Self { zst: this.clone() }
        } else {
            let this = unsafe { &self.non_zst };

            Self {
                non_zst: this.clone(),
            }
        }
    }
}

impl<K, V> Default for Union<K, V> {
    fn default() -> Self {
        if is_zst::<V>() {
            Self {
                zst: ManuallyDrop::new(ZstImpl::default()),
            }
        } else {
            Self {
                non_zst: ManuallyDrop::new(NonZstImpl::default()),
            }
        }
    }
}

impl<K, V> Drop for Union<K, V> {
    fn drop(&mut self) {
        if is_zst::<V>() {
            unsafe { ManuallyDrop::drop(&mut self.zst) };
        } else {
            unsafe { ManuallyDrop::drop(&mut self.non_zst) };
        }
    }
}

#[derive(Clone)]
struct ZstImpl<K, V> {
    sparse: BitSet<K>,
    dense: Vec<V>,
}

impl<K, V> Default for ZstImpl<K, V> {
    fn default() -> Self {
        Self {
            sparse: Default::default(),
            dense: Default::default(),
        }
    }
}

#[derive(Clone, Debug)]
struct NonZstImpl<K, V> {
    sparse: Vec<K>,
    dense: Vec<V>,
    // This has the same length as `dense`.
    indices: Vec<K>,
}

impl<K, V> Default for NonZstImpl<K, V> {
    fn default() -> Self {
        Self {
            sparse: Default::default(),
            dense: Default::default(),
            indices: Default::default(),
        }
    }
}

#[derive(Clone, Debug)]
struct Dense<K, V> {
    key: K,
    value: V,
}

impl<K, V> fmt::Debug for SparseMap<K, V>
where
    K: SparseIndex + fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("SparseMap");

        if is_zst::<V>() {
            let this = unsafe { &self.union.zst };

            s.field("sparse", &this.sparse).field("dense", &this.dense)
        } else {
            let this = unsafe { &self.union.non_zst };

            s.field("sparse", &this.sparse)
                .field("dense", &this.dense)
                .field("indices", &this.indices)
        }
        .finish()
    }
}

impl<K: SparseIndex, V> SparseMap<K, V> {
    pub(crate) fn new() -> Self {
        Self {
            union: Union::default(),
        }
    }

    #[inline]
    pub(crate) fn get(&self, key: K) -> Option<&V> {
        if is_zst::<V>() {
            let this = unsafe { &self.union.zst };

            if this.sparse.contains(key) {
                // SAFETY: dense vec must be nonempty since the sparse vec has an element.
                Some(unsafe { this.dense.first().unwrap_debug_checked() })
            } else {
                None
            }
        } else {
            let this = unsafe { &self.union.non_zst };

            let idx = this.sparse.get(key.index())?.index();

            if idx >= K::MAX.index() {
                None
            } else {
                let value = unsafe { this.dense.get_debug_checked(idx) };
                Some(value)
            }
        }
    }

    #[inline]
    pub(crate) fn get_mut(&mut self, key: K) -> Option<&mut V> {
        if is_zst::<V>() {
            let this = unsafe { &mut self.union.zst };

            if this.sparse.contains(key) {
                // SAFETY: dense vec must be nonempty since the sparse vec has an element.
                Some(unsafe { this.dense.first_mut().unwrap_debug_checked() })
            } else {
                None
            }
        } else {
            let this = unsafe { &mut self.union.non_zst };

            let idx = this.sparse.get(key.index())?.index();

            if idx >= K::MAX.index() {
                None
            } else {
                let value = unsafe { this.dense.get_debug_checked_mut(idx) };
                Some(value)
            }
        }
    }

    #[track_caller]
    #[inline]
    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        let sparse_idx = key.index();

        // Technically this assertion isn't necessary in the ZST case, but we'll keep it
        // for consistency.
        assert_ne!(
            sparse_idx,
            K::MAX.index(),
            "cannot insert in sparse map with `K::MAX` as key"
        );

        if is_zst::<V>() {
            let this = unsafe { &mut self.union.zst };

            if this.sparse.insert(key) {
                this.dense.push(value);
                None
            } else {
                Some(value)
            }
        } else {
            let this = unsafe { &mut self.union.non_zst };

            if sparse_idx >= this.sparse.len() {
                this.sparse.resize(sparse_idx + 1, K::MAX);
            }

            // SAFETY: We resized the vec so that `sparse_idx` is in bounds.
            let dense_len = this.dense.len();
            let dense_idx = unsafe { this.sparse.get_debug_checked_mut(sparse_idx) };

            if dense_idx.index() == K::MAX.index() {
                *dense_idx = K::from_index(dense_len);

                this.dense.push(value);
                this.indices.push(key);

                None
            } else {
                let idx = dense_idx.index();
                // SAFETY: Data structure ensures all non-max dense indices are in bounds.
                let v = unsafe { this.dense.get_debug_checked_mut(idx) };

                Some(mem::replace(v, value))
            }
        }
    }

    #[inline]
    pub(crate) fn remove(&mut self, key: K) -> Option<V> {
        if is_zst::<V>() {
            let this = unsafe { &mut self.union.zst };

            if this.sparse.remove(key) {
                // SAFETY: Sparse vec was non-empty, so dense vec must be non-empty too.
                Some(unsafe { this.dense.pop().unwrap_debug_checked() })
            } else {
                None
            }
        } else {
            let this = unsafe { &mut self.union.non_zst };

            let dense_idx = mem::replace(this.sparse.get_mut(key.index())?, K::MAX).index();

            if dense_idx == K::MAX.index() {
                None
            } else {
                unsafe { assume_debug_checked(dense_idx < this.dense.len()) };

                let res = this.dense.swap_remove(dense_idx);

                unsafe { assume_debug_checked(dense_idx < this.indices.len()) };

                this.indices.swap_remove(dense_idx);

                if let Some(&moved_index) = this.indices.get(dense_idx) {
                    *unsafe { this.sparse.get_debug_checked_mut(moved_index.index()) } =
                        K::from_index(dense_idx);
                }

                Some(res)
            }
        }
    }

    #[inline]
    pub(crate) fn values(&self) -> &[V] {
        if is_zst::<V>() {
            let this = unsafe { &self.union.zst };
            &this.dense
        } else {
            let this = unsafe { &self.union.non_zst };
            &this.dense
        }
    }

    #[inline]
    pub(crate) fn values_mut(&mut self) -> &mut [V] {
        if is_zst::<V>() {
            let this = unsafe { &mut self.union.zst };
            &mut this.dense
        } else {
            let this = unsafe { &mut self.union.non_zst };
            &mut this.dense
        }
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        if is_zst::<V>() {
            let this = unsafe { &mut self.union.zst };

            this.sparse.shrink_to_fit();
            this.dense.shrink_to_fit();
        } else {
            let this = unsafe { &mut self.union.non_zst };

            this.dense.shrink_to_fit();

            while let Some(&last) = this.sparse.last() {
                if last.index() != K::MAX.index() {
                    break;
                }

                this.sparse.pop();
            }

            this.sparse.shrink_to_fit();
        }
    }
}

const fn is_zst<T>() -> bool {
    mem::size_of::<T>() == 0
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    #[test]
    fn sparse_map() {
        let mut map = SparseMap::<u32, char>::new();

        assert_eq!(map.insert(12, 'a'), None);
        assert_eq!(map.insert(5, 'b'), None);
        assert_eq!(map.insert(42, 'c'), None);
        assert_eq!(map.insert(5, 'd'), Some('b'));

        assert_eq!(map.get(12), Some(&'a'));
        assert_eq!(map.get(42), Some(&'c'));
        assert_eq!(map.get_mut(42), Some(&mut 'c'));

        assert_eq!(map.remove(42), Some('c'));
        assert_eq!(map.insert(42, 'e'), None);
        assert_eq!(map.insert(43, 'f'), None);

        assert_eq!(
            map.values().iter().copied().collect::<BTreeSet<_>>(),
            BTreeSet::from_iter(['a', 'd', 'e', 'f'])
        );

        assert_eq!(map.remove(43), Some('f'));
        map.shrink_to_fit();

        assert_eq!(
            map.values().iter().copied().collect::<BTreeSet<_>>(),
            BTreeSet::from_iter(['a', 'd', 'e'])
        );
    }

    #[test]
    #[should_panic]
    fn sparse_map_insert_max() {
        let mut map = SparseMap::<u8, char>::new();
        map.insert(u8::MAX, 'f');
    }

    #[test]
    fn sparse_map_zst() {
        let mut map = SparseMap::<u32, ()>::new();

        assert_eq!(map.insert(123, ()), None);
        assert_eq!(map.insert(456, ()), None);
        assert_eq!(map.insert(42, ()), None);

        assert_eq!(map.remove(456), Some(()));
        assert_eq!(map.insert(123, ()), Some(()));

        assert_eq!(map.values().len(), 2);
    }
}
