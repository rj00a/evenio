use std::mem;

use crate::debug_checked::{assume_debug_checked, GetDebugChecked};
use crate::sparse::SparseIndex;

#[derive(Clone, Default, Debug)]
pub(crate) struct SparseMap<K, V> {
    sparse: Vec<K>,
    dense: Vec<Dense<K, V>>,
}

#[derive(Clone, Debug)]
struct Dense<K, V> {
    key: K,
    value: V,
}

impl<K: SparseIndex, V> SparseMap<K, V> {
    pub(crate) const fn new() -> Self {
        Self {
            sparse: vec![],
            dense: vec![],
        }
    }

    #[inline]
    pub(crate) fn get(&self, key: K) -> Option<&V> {
        let idx = self.sparse.get(key.index())?.index();

        if idx >= K::MAX.index() {
            None
        } else {
            let value = unsafe { &self.dense.get_debug_checked(idx).value };
            Some(value)
        }
    }

    #[inline]
    pub(crate) fn get_mut(&mut self, key: K) -> Option<&mut V> {
        let idx = self.sparse.get(key.index())?.index();

        if idx >= K::MAX.index() {
            None
        } else {
            let value = unsafe { &mut self.dense.get_debug_checked_mut(idx).value };
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
        let dense_idx = unsafe { self.sparse.get_debug_checked_mut(sparse_idx) };

        if dense_idx.index() == K::MAX.index() {
            *dense_idx = K::from_index(self.dense.len());

            self.dense.push(Dense { key, value });

            None
        } else {
            // SAFETY: Data structure ensures all non-max dense indices are in bounds.
            let dense = unsafe { self.dense.get_debug_checked_mut(dense_idx.index()) };

            Some(mem::replace(&mut dense.value, value))
        }
    }

    #[inline]
    pub(crate) fn remove(&mut self, key: K) -> Option<V> {
        let dense_idx = mem::replace(self.sparse.get_mut(key.index())?, K::MAX).index();

        if dense_idx == K::MAX.index() {
            None
        } else {
            unsafe { assume_debug_checked(dense_idx < self.dense.len()) };

            let res = self.dense.swap_remove(dense_idx).value;

            if let Some(swapped_dense) = self.dense.get_mut(dense_idx) {
                *unsafe { self.sparse.get_debug_checked_mut(swapped_dense.key.index()) } =
                    K::from_index(dense_idx);
            }

            Some(res)
        }
    }

    #[inline]
    pub(crate) fn values(&self) -> impl Iterator<Item = &V> {
        self.dense.iter().map(|d| &d.value)
    }

    #[inline]
    pub(crate) fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.dense.iter_mut().map(|d| &mut d.value)
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        self.dense.shrink_to_fit();

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
            map.values().copied().collect::<BTreeSet<_>>(),
            BTreeSet::from_iter(['a', 'd', 'e', 'f'])
        );

        assert_eq!(map.remove(43), Some('f'));
        map.shrink_to_fit();

        assert_eq!(
            map.values().copied().collect::<BTreeSet<_>>(),
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
