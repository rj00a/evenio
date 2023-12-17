use core::fmt;
use core::iter::FusedIterator;
use core::marker::PhantomData;

use crate::debug_checked::GetDebugChecked;
use crate::sparse::SparseIndex;

/// A set data structure backed by a vector of bits.
pub struct BitSet<T = usize> {
    blocks: Vec<Block>,
    _marker: PhantomData<T>,
}

impl<T> Clone for BitSet<T> {
    fn clone(&self) -> Self {
        Self {
            blocks: self.blocks.clone(),
            _marker: self._marker.clone(),
        }
    }
}

type Block = usize;
/// Number of bits in a block.
const BITS: usize = Block::BITS as usize;

impl<T> BitSet<T> {
    pub const fn new() -> Self {
        Self {
            blocks: vec![],
            _marker: PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
    }

    #[inline]
    #[track_caller]
    fn grow_to_block(&mut self, block_idx: usize) -> &mut Block {
        if block_idx >= self.blocks.len() {
            // Don't overflow the addition.
            assert_ne!(
                block_idx,
                usize::MAX,
                "reached maximum block count in {}",
                std::any::type_name::<Self>()
            );
            self.blocks.resize(block_idx + 1, 0);
        }

        // SAFETY: Block index is in bounds due to check above.
        unsafe { self.blocks.get_debug_checked_mut(block_idx) }
    }

    #[must_use]
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .all(|(a, b)| a & b == 0)
    }

    pub fn union_assign(&mut self, other: &Self) {
        if self.blocks.len() < other.blocks.len() {
            self.blocks.resize(other.blocks.len(), 0);
        }

        for (a, b) in self.blocks.iter_mut().zip(other.blocks.iter()) {
            *a |= *b;
        }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.blocks
            .iter()
            .map(|block| block.count_ones() as usize)
            .sum()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.blocks.iter().all(|&block| block == 0)
    }
}

impl<T: SparseIndex> BitSet<T> {
    #[track_caller]
    #[inline]
    pub(crate) fn insert(&mut self, value: T) -> bool {
        let idx = value.index();

        let (block, bit) = div_rem(idx, BITS);

        let block = self.grow_to_block(block);

        let newly_inserted = *block & (1 << bit) == 0;

        *block |= 1 << bit;

        newly_inserted
    }

    #[inline]
    pub(crate) fn remove(&mut self, value: T) -> bool {
        let idx = value.index();

        let (block, bit) = div_rem(idx, BITS);

        if let Some(block) = self.blocks.get_mut(block) {
            let removed = *block & (1 << bit) != 0;

            *block &= !(1 << bit);

            removed
        } else {
            false
        }
    }

    #[must_use]
    pub(crate) fn contains(&self, value: T) -> bool {
        let idx = value.index();

        let (block, bit) = div_rem(idx, BITS);

        self.blocks
            .get(block)
            .map_or(false, |&block| (block >> bit) & 1 == 1)
    }

    pub(crate) fn iter(&self) -> Iter<T> {
        Iter {
            bits: self.blocks.get(0).copied().unwrap_or(0),
            block_idx: 0,
            blocks: &self.blocks,
            _marker: PhantomData,
        }
    }

    pub(crate) fn shrink_to_fit(&mut self) {
        while let Some(&last) = self.blocks.last() {
            if last != 0 {
                // There are bits in this block.
                break;
            }

            self.blocks.pop();
        }

        self.blocks.shrink_to_fit();
    }
}

impl<T> Default for BitSet<T> {
    fn default() -> Self {
        Self {
            blocks: vec![],
            _marker: PhantomData,
        }
    }
}

impl<T: SparseIndex> FromIterator<T> for BitSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = Self::new();

        for t in iter {
            set.insert(t);
        }

        set
    }
}

pub(crate) struct Iter<'a, T = usize> {
    bits: Block,
    block_idx: usize,
    blocks: &'a [Block],
    _marker: PhantomData<fn(T)>,
}

impl<'a, T: SparseIndex> Iterator for Iter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.bits == 0 {
            self.block_idx += 1;
            self.bits = *self.blocks.get(self.block_idx)?;
        }

        // Index of least significant bit in the block.
        let zeros = self.bits.trailing_zeros() as usize;

        // Clear the least significant bit.
        self.bits ^= 1 << zeros;

        Some(T::from_index(self.block_idx * BITS + zeros))
    }
}

impl<'a, T: SparseIndex> FusedIterator for Iter<'a, T> {}

#[inline]
fn div_rem(a: usize, b: usize) -> (usize, usize) {
    (a / b, a % b)
}

impl<T> fmt::Debug for BitSet<T>
where
    T: SparseIndex + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut set = f.debug_set();

        for elem in self.iter() {
            set.entry(&elem);
        }

        set.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contains() {
        let indices = [0u32, 1, 4, 15, 64, 100, 1000, 1001, 1002];

        let set = BitSet::from_iter(indices);

        for idx in indices {
            assert!(set.contains(idx));
        }

        assert!(!set.contains(2));
        assert!(!set.contains(3));
        assert!(!set.contains(5));
        assert!(!set.contains(10003));
    }

    #[test]
    fn iter() {
        let mut indices = [0u32, 1, 4, 15, 100, 1000, 1001, 1002, 64];

        let set = BitSet::from_iter(indices);

        let collected = set.iter().collect::<Vec<u32>>();

        indices.sort_unstable();

        assert_eq!(indices.as_slice(), collected);
    }
}
