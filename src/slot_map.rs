#[cfg(not(feature = "std"))]
use alloc::{vec, vec::Vec};
use core::marker::PhantomData;
use core::mem::ManuallyDrop;
use core::num::{NonZeroU32, NonZeroU64};
use core::ops::{Index, IndexMut};
use core::{fmt, mem};

use crate::assert::UnwrapDebugChecked;

#[derive(Clone, Debug)]
pub(crate) struct SlotMap<T> {
    slots: Vec<Slot<T>>,
    next_free: u32,
    len: u32,
}

impl<T> SlotMap<T> {
    pub(crate) fn new() -> Self {
        Self {
            slots: vec![],
            next_free: u32::MAX,
            len: 0,
        }
    }

    pub(crate) fn insert(&mut self, value: T) -> Option<Key> {
        self.insert_with(|_| value)
    }

    pub(crate) fn insert_with<F>(&mut self, f: F) -> Option<Key>
    where
        F: FnOnce(Key) -> T,
    {
        let key;

        if let Some(slot) = self.slots.get_mut(self.next_free as usize) {
            debug_assert!(slot.is_vacant());

            // SAFETY: Generation doesn't overflow because it's even.
            key = unsafe { Key::new(self.next_free, slot.generation + 1).unwrap_debug_checked() };

            // Get value before modifying the slot in case `f` unwinds.
            let value = f(key);

            slot.generation += 1;

            self.next_free = unsafe { slot.union.next_free };

            slot.union.value = ManuallyDrop::new(value);
        } else {
            let index = self.slots.len() as u32;

            if index == u32::MAX {
                return None;
            }

            key = Key::new(index, 1).unwrap();

            let value = f(key);

            self.slots.push(Slot {
                union: SlotUnion {
                    value: ManuallyDrop::new(value),
                },
                generation: 1,
            });
        };

        self.len += 1;

        Some(key)
    }

    pub(crate) fn remove(&mut self, key: Key) -> Option<T> {
        let slot = self.slots.get_mut(key.index() as usize)?;

        if slot.generation != key.generation().get() {
            return None;
        }

        slot.generation = slot.generation.wrapping_add(1);

        let res = unsafe { ManuallyDrop::take(&mut slot.union.value) };

        // If the generation didn't overflow then add the slot to the free list.
        // Otherwise, the slot is considered retired and won't be used again.
        if slot.generation != 0 {
            slot.union.next_free = self.next_free;
            self.next_free = key.index();
        }

        self.len -= 1;

        Some(res)
    }

    pub(crate) fn get(&self, key: Key) -> Option<&T> {
        let slot = self.slots.get(key.index() as usize)?;

        if slot.generation != key.generation().get() {
            return None;
        }

        Some(unsafe { &slot.union.value })
    }

    pub(crate) fn get_mut(&mut self, key: Key) -> Option<&mut T> {
        let slot = self.slots.get_mut(key.index() as usize)?;

        if slot.generation != key.generation().get() {
            return None;
        }

        Some(unsafe { &mut slot.union.value })
    }

    pub(crate) fn get_by_index(&self, index: u32) -> Option<(Key, &T)> {
        let slot = self.slots.get(index as usize)?;

        if slot.is_vacant() {
            return None;
        }

        let key = unsafe { Key::new_unchecked(index, slot.generation) };

        let value = unsafe { &slot.union.value };

        Some((key, value))
    }

    pub(crate) fn get_by_index_mut(&mut self, index: u32) -> Option<(Key, &mut T)> {
        let slot = self.slots.get_mut(index as usize)?;

        if slot.is_vacant() {
            return None;
        }

        let key = unsafe { Key::new_unchecked(index, slot.generation) };

        let value = unsafe { &mut slot.union.value };

        Some((key, value))
    }

    pub(crate) fn next_key_iter(&self) -> NextKeyIter<T> {
        NextKeyIter {
            index: if self.next_free == u32::MAX {
                self.slots.len() as u32
            } else {
                self.next_free
            },
            _marker: PhantomData,
        }
    }

    /// Returns the number of occupied slots.
    pub(crate) const fn len(&self) -> u32 {
        self.len
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (Key, &T)> {
        self.slots
            .iter()
            .enumerate()
            .filter(|(_, s)| !s.is_vacant())
            .map(|(idx, slot)| {
                let key = unsafe { Key::new_unchecked(idx as u32, slot.generation) };
                let value = unsafe { &*slot.union.value };

                (key, value)
            })
    }

    #[allow(dead_code)]
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = (Key, &mut T)> {
        self.slots.iter_mut().enumerate().filter_map(|(idx, slot)| {
            (!slot.is_vacant()).then(|| {
                let key = unsafe { Key::new_unchecked(idx as u32, slot.generation) };
                let value = unsafe { &mut *slot.union.value };

                (key, value)
            })
        })
    }
}

impl<T> Default for SlotMap<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Index<Key> for SlotMap<T> {
    type Output = T;

    fn index(&self, k: Key) -> &Self::Output {
        match self.get(k) {
            Some(v) => v,
            None => panic!("invalid slot map key of {k:?}"),
        }
    }
}

impl<T> IndexMut<Key> for SlotMap<T> {
    fn index_mut(&mut self, k: Key) -> &mut Self::Output {
        match self.get_mut(k) {
            Some(v) => v,
            None => panic!("invalid slot map key of {k:?}"),
        }
    }
}

struct Slot<T> {
    union: SlotUnion<T>,
    // Odd when occupied, even when vacant.
    generation: u32,
}

impl<T> Slot<T> {
    const fn is_vacant(&self) -> bool {
        self.generation % 2 == 0
    }
}

impl<T> Drop for Slot<T> {
    fn drop(&mut self) {
        if mem::needs_drop::<T>() && !self.is_vacant() {
            unsafe { ManuallyDrop::drop(&mut self.union.value) }
        }
    }
}

impl<T: Clone> Clone for Slot<T> {
    fn clone(&self) -> Self {
        Self {
            union: if self.is_vacant() {
                unsafe {
                    SlotUnion {
                        next_free: self.union.next_free,
                    }
                }
            } else {
                unsafe {
                    SlotUnion {
                        value: self.union.value.clone(),
                    }
                }
            },
            generation: self.generation,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for Slot<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut s = f.debug_struct("Slot");

        if self.is_vacant() {
            s.field("next_free", unsafe { &self.union.next_free });
        } else {
            s.field("value", unsafe { &*self.union.value });
        }

        s.field("generation", &self.generation).finish()
    }
}

union SlotUnion<T> {
    value: ManuallyDrop<T>,
    next_free: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Key {
    /// Least significant half is index, most significant half is generation.
    n: NonZeroU64,
}

impl Key {
    pub(crate) const NULL: Self = Self { n: NonZeroU64::MAX };

    #[inline]
    pub(crate) const fn new(index: u32, generation: u32) -> Option<Self> {
        // LSB of generation must be 1 for safety.
        if generation % 2 == 1 {
            Some(unsafe { Self::new_unchecked(index, generation) })
        } else {
            None
        }
    }

    /// SAFETY: Generation must be odd.
    #[inline]
    const unsafe fn new_unchecked(index: u32, generation: u32) -> Self {
        let n = (generation as u64) << 32 | index as u64;
        Self {
            n: unsafe { NonZeroU64::new_unchecked(n) },
        }
    }

    #[inline]
    pub(crate) const fn index(self) -> u32 {
        // Truncate away MSBs.
        self.n.get() as u32
    }

    #[inline]
    pub(crate) const fn generation(self) -> NonZeroU32 {
        // SAFETY: Generation is always non-zero, so resulting `NonZeroU32` must be
        // non-zero.
        unsafe { NonZeroU32::new_unchecked((self.n.get() >> 32) as u32) }
    }
}

impl Default for Key {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.index(), self.generation())
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct NextKeyIter<T> {
    index: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> NextKeyIter<T> {
    pub(crate) fn new() -> Self {
        Self {
            index: 0,
            _marker: PhantomData,
        }
    }

    pub(crate) fn next(&mut self, sm: &SlotMap<T>) -> Option<Key> {
        let key;

        if let Some(slot) = sm.slots.get(self.index as usize) {
            if slot.is_vacant() {
                // SAFETY: slot.generation is even because slot is vacant.
                key = Some(unsafe { Key::new_unchecked(self.index, slot.generation + 1) });

                // SAFETY: slot is vacant.
                let next_free = unsafe { slot.union.next_free };

                // Reached end of free list?
                if next_free == u32::MAX {
                    self.index = sm.slots.len() as u32;
                } else {
                    self.index = next_free;
                }
            } else {
                panic!("incorrect state for next key iter");
            }
        } else if self.index < u32::MAX {
            key = Some(Key::new(self.index, 1).unwrap());

            self.index += 1;
        } else {
            key = None;
        }

        key
    }
}

#[cfg(test)]
mod tests {
    use alloc::collections::BTreeSet;
    use alloc::rc::Rc;
    use core::cell::Cell;

    use super::*;

    #[test]
    fn insert_remove() {
        let mut sm = SlotMap::new();

        let k = sm.insert(123).unwrap();
        assert_eq!(sm.remove(Key::NULL), None);
        assert_eq!(sm.remove(k), Some(123));
        let k1 = sm.insert(123).unwrap();
        let k2 = sm.insert(456).unwrap();
        let k3 = sm.insert(789).unwrap();
        assert_eq!(BTreeSet::from_iter([k, k1, k2, k3]).len(), 4);
        assert_eq!(sm.remove(Key::NULL), None);
        assert_eq!(sm.remove(k2), Some(456));
        assert_eq!(sm.remove(k2), None);
        assert_eq!(sm.len(), 2);
        assert_eq!(sm.remove(k1), Some(123));
        assert_eq!(sm.remove(k3), Some(789));
        assert_eq!(sm.len(), 0);
    }

    #[test]
    fn get() {
        let mut sm = SlotMap::new();

        let k1 = sm.insert(123).unwrap();
        let k2 = sm.insert(456).unwrap();

        assert_eq!(sm.get(k1), Some(&123));
        assert_eq!(sm.get_mut(k2), Some(&mut 456));
        assert_eq!(sm.get(Key::NULL), None);

        assert_eq!(sm.remove(k2), Some(456));

        assert_eq!(sm.get(k1), Some(&123));
        assert_eq!(sm.get_mut(k1), Some(&mut 123));
    }

    #[test]
    fn retires_slot() {
        let mut sm = SlotMap::new();

        sm.insert(123);
        sm.slots[0].generation = u32::MAX;

        let k = Key::new(0, u32::MAX).unwrap();

        assert_eq!(sm.remove(k), Some(123));
        let k2 = sm.insert(456).unwrap();

        assert_ne!(k2.index(), 0);
    }

    #[test]
    fn drops_items() {
        struct Foo(Rc<Cell<usize>>);

        impl Drop for Foo {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }

        let mut sm = SlotMap::new();
        let count = Rc::new(Cell::new(0));

        sm.insert(Foo(count.clone()));
        let k = sm.insert(Foo(count.clone())).unwrap();
        sm.insert(Foo(count.clone()));

        sm.remove(k);

        drop(sm);

        assert_eq!(count.get(), 3);
    }

    #[test]
    fn iter() {
        let mut sm = SlotMap::new();

        let k0 = sm.insert(123).unwrap();
        let k1 = sm.insert(456).unwrap();
        let k2 = sm.insert(789).unwrap();

        let mut it = sm.iter();
        assert_eq!(it.next(), Some((k0, &123)));
        assert_eq!(it.next(), Some((k1, &456)));
        assert_eq!(it.next(), Some((k2, &789)));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn next_key_iter() {
        let mut sm = SlotMap::new();

        sm.insert(123).unwrap();
        let id = sm.insert(456).unwrap();
        sm.insert(789).unwrap();

        sm.remove(id);

        let mut iter = sm.next_key_iter();

        assert_eq!(iter.next(&sm), sm.insert(0));
        assert_eq!(iter.next(&sm), sm.insert(0));
        assert_eq!(iter.next(&sm), sm.insert(0));
    }

    #[test]
    fn next_key_iter_null_next_free() {
        let mut sm = SlotMap::new();

        sm.insert(123).unwrap();
        sm.insert(456).unwrap();

        let mut iter = sm.next_key_iter();

        assert_eq!(iter.next(&sm), sm.insert(0));
        assert_eq!(iter.next(&sm), sm.insert(0));
    }
}
