//! The [`BitSet`], a set backed by a vector of bits.

#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use core::cmp::Ordering;
use core::iter::FusedIterator;
use core::marker::PhantomData;
use core::ops::{BitOr, BitOrAssign};
use core::{any, fmt};

use crate::assert::GetDebugChecked;
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
            _marker: PhantomData,
        }
    }
}

type Block = usize;
/// Number of bits in a block.
const BITS: usize = Block::BITS as usize;

impl<T> BitSet<T> {
    /// Create a new, empty bit set.
    pub const fn new() -> Self {
        Self {
            blocks: vec![],
            _marker: PhantomData,
        }
    }

    /// Clears the set, removing all elements.
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
                any::type_name::<Self>()
            );
            self.blocks.resize(block_idx + 1, 0);
        }

        // SAFETY: Block index is in bounds due to check above.
        unsafe { self.blocks.get_debug_checked_mut(block_idx) }
    }

    /// Returns `true` if `self` has no elements in common with `other`.
    #[must_use]
    pub fn is_disjoint(&self, other: &Self) -> bool {
        self.blocks
            .iter()
            .zip(other.blocks.iter())
            .all(|(a, b)| a & b == 0)
    }

    /// Returns the number of elements in the set.
    #[must_use]
    pub fn len(&self) -> usize {
        self.blocks
            .iter()
            .map(|block| block.count_ones() as usize)
            .sum()
    }

    /// Returns `true` if the set contains no elements.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.blocks.iter().all(|&block| block == 0)
    }
}

impl<T: SparseIndex> BitSet<T> {
    /// Adds a value to the set. Returns whether the value was newly inserted.
    #[track_caller]
    #[inline]
    pub fn insert(&mut self, value: T) -> bool {
        let idx = value.index();

        let (block, bit) = div_rem(idx, BITS);

        let block = self.grow_to_block(block);

        let newly_inserted = *block & (1 << bit) == 0;

        *block |= 1 << bit;

        newly_inserted
    }

    /// Removes a value from the set. Returns whether such an element was
    /// present.
    #[inline]
    pub fn remove(&mut self, value: T) -> bool {
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

    /// Returns `true` if the set contains an element equal to the value.
    #[must_use]
    pub fn contains(&self, value: T) -> bool {
        let idx = value.index();

        let (block, bit) = div_rem(idx, BITS);

        self.blocks
            .get(block)
            .map_or(false, |&block| (block >> bit) & 1 == 1)
    }

    /// Returns an iterator over the element in the set in ascending order.
    pub fn iter(&self) -> Iter<T> {
        Iter {
            bits: self.blocks.first().copied().unwrap_or(0),
            block_idx: 0,
            blocks: &self.blocks,
            _marker: PhantomData,
        }
    }

    /// Shrinks the capacity of the set as much as possible.
    pub fn shrink_to_fit(&mut self) {
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

impl<T> BitOrAssign<&Self> for BitSet<T> {
    fn bitor_assign(&mut self, other: &Self) {
        if self.blocks.len() < other.blocks.len() {
            self.blocks.resize(other.blocks.len(), 0);
        }

        for (a, b) in self.blocks.iter_mut().zip(other.blocks.iter()) {
            *a |= *b;
        }
    }
}

impl<T> BitOr<&Self> for BitSet<T> {
    type Output = Self;

    fn bitor(mut self, other: &Self) -> Self::Output {
        self |= other;
        self
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

impl<'a, T: SparseIndex> IntoIterator for &'a BitSet<T> {
    type Item = T;

    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> fmt::Debug for BitSet<T>
where
    T: SparseIndex + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut set = f.debug_set();

        for elem in self {
            set.entry(&elem);
        }

        set.finish()
    }
}

/// An iterator over the items in a [`BitSet`].
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Iter<'a, T = usize> {
    bits: Block,
    block_idx: usize,
    blocks: &'a [Block],
    _marker: PhantomData<fn() -> T>,
}

impl<'a, T: SparseIndex> Iterator for Iter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        while self.bits == 0 {
            self.bits = *self.blocks.get(self.block_idx + 1)?;
            self.block_idx += 1;
        }

        // Index of least significant bit in the block.
        let zeros = self.bits.trailing_zeros() as usize;

        // Clear the least significant bit.
        self.bits ^= 1 << zeros;

        Some(T::from_index(self.block_idx * BITS + zeros))
    }
}

impl<'a, T: SparseIndex> FusedIterator for Iter<'a, T> {}

impl<T> fmt::Debug for Iter<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Iter")
            .field("bits", &self.bits)
            .field("block_idx", &self.block_idx)
            .field("blocks", &self.blocks)
            .field("_marker", &self._marker)
            .finish()
    }
}

#[inline]
fn div_rem(a: usize, b: usize) -> (usize, usize) {
    (a / b, a % b)
}

impl<T: SparseIndex> Ord for BitSet<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        let mut left = self.blocks.iter();
        let mut right = other.blocks.iter();

        loop {
            match (left.next(), right.next()) {
                (None, None) => break Ordering::Equal,
                (None, Some(&r)) => {
                    if r != 0 {
                        break Ordering::Greater;
                    }
                }
                (Some(&l), None) => {
                    if l != 0 {
                        break Ordering::Less;
                    }
                }
                (Some(l), Some(r)) => match l.cmp(r) {
                    Ordering::Less => break Ordering::Less,
                    Ordering::Equal => {}
                    Ordering::Greater => break Ordering::Greater,
                },
            }
        }
    }
}

impl<T: SparseIndex> PartialOrd for BitSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: SparseIndex> PartialEq for BitSet<T> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl<T: SparseIndex> Eq for BitSet<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contains() {
        let indices = [0_u32, 1, 4, 15, 64, 100, 1000, 1001, 1002];

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
        let mut indices = [0_u32, 1, 4, 15, 100, 1000, 1001, 1002, 64];

        let set = BitSet::from_iter(indices);

        let collected = set.iter().collect::<Vec<u32>>();

        indices.sort_unstable();

        assert_eq!(indices.as_slice(), collected);
    }

    #[test]
    fn ordering() {
        let left = BitSet::from_iter([0_u32, 1, 2, 3, 4, 0]);
        let right = BitSet::from_iter([0_u32, 1, 2, 3, 4, 0]);

        assert_eq!(left, right);

        let mut left = BitSet::from_iter([0_u32, 1, 2]);
        left.insert(500);
        left.remove(500);
        let right = BitSet::from_iter([0_u32, 1, 2]);

        assert_eq!(left, right);
        left.shrink_to_fit();
        assert_eq!(left, right);

        let left = BitSet::from_iter([0_u32, 1, 2]);
        let right = BitSet::from_iter([0_u32, 1, 2, 3]);

        assert_ne!(left < right, left > right);
    }

    #[test]
    fn iter_is_fused() {
        let set = BitSet::<u32>::from_iter([1, 5, 7, 123]);
        let mut iter = set.iter();
        assert_eq!(iter.next(), Some(1));
        assert_eq!(iter.next(), Some(5));
        assert_eq!(iter.next(), Some(7));
        assert_eq!(iter.next(), Some(123));
        assert!(iter.next().is_none());
        assert!(iter.next().is_none());
        assert!(iter.next().is_none());
    }
}
