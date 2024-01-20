use std::mem;

use crate::debug_checked::{assume_debug_checked, GetDebugChecked};
use crate::sparse::SparseIndex;

#[derive(Clone, Default, Debug)]
pub(crate) struct SparseMap<K, V> {
    sparse: Vec<K>,
    dense: Vec<V>,
    // This has the same length as `dense`.
    indices: Vec<K>,
}

#[allow(dead_code)]
impl<K: SparseIndex, V> SparseMap<K, V> {
    pub(crate) const fn new() -> Self {
        Self {
            sparse: vec![],
            dense: vec![],
            indices: vec![],
        }
    }

    #[inline]
    pub(crate) fn get(&self, key: K) -> Option<&V> {
        let idx = self.sparse.get(key.index())?.index();

        if idx >= K::MAX.index() {
            None
        } else {
            let value = unsafe { self.dense.get_debug_checked(idx) };
            Some(value)
        }
    }

    #[inline]
    pub(crate) fn contains_key(&self, key: K) -> bool {
        self.get(key).is_some()
    }

    #[inline]
    pub(crate) fn get_mut(&mut self, key: K) -> Option<&mut V> {
        let idx = self.sparse.get(key.index())?.index();

        if idx >= K::MAX.index() {
            None
        } else {
            let value = unsafe { self.dense.get_debug_checked_mut(idx) };
            Some(value)
        }
    }

    #[track_caller]
    #[inline]
    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        let sparse_idx = key.index();

        assert_ne!(
            sparse_idx,
            K::MAX.index(),
            "cannot insert in sparse map with `K::MAX` as key"
        );

        if sparse_idx >= self.sparse.len() {
            self.sparse.resize(sparse_idx + 1, K::MAX);
        }

        // SAFETY: We resized the vec so that `sparse_idx` is in bounds.
        let dense_len = self.dense.len();
        let dense_idx = unsafe { self.sparse.get_debug_checked_mut(sparse_idx) };

        if dense_idx.index() == K::MAX.index() {
            *dense_idx = K::from_index(dense_len);

            self.dense.push(value);
            self.indices.push(key);

            None
        } else {
            let idx = dense_idx.index();
            // SAFETY: Data structure ensures all non-max dense indices are in bounds.
            let v = unsafe { self.dense.get_debug_checked_mut(idx) };

            Some(mem::replace(v, value))
        }
    }

    #[inline]
    pub(crate) fn remove(&mut self, key: K) -> Option<V> {
        let dense_idx = mem::replace(self.sparse.get_mut(key.index())?, K::MAX).index();

        if dense_idx == K::MAX.index() {
            None
        } else {
            unsafe { assume_debug_checked(dense_idx < self.dense.len()) };

            let res = self.dense.swap_remove(dense_idx);

            unsafe { assume_debug_checked(dense_idx < self.indices.len()) };

            self.indices.swap_remove(dense_idx);

            if let Some(&moved_index) = self.indices.get(dense_idx) {
                *unsafe { self.sparse.get_debug_checked_mut(moved_index.index()) } =
                    K::from_index(dense_idx);
            }

            Some(res)
        }
    }

    pub(crate) fn keys(&self) -> &[K] {
        &self.indices
    }

    pub(crate) fn values(&self) -> &[V] {
        &self.dense
    }

    pub(crate) fn values_mut(&mut self) -> &mut [V] {
        &mut self.dense
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        self.dense.shrink_to_fit();
        self.indices.shrink_to_fit();

        while let Some(&last) = self.sparse.last() {
            if last.index() != K::MAX.index() {
                break;
            }

            self.sparse.pop();
        }

        self.sparse.shrink_to_fit();
    }
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
}
