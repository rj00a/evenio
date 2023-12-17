use core::fmt;
use core::mem::ManuallyDrop;
use core::num::NonZeroU32;

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

    pub(crate) fn insert(&mut self, value: T) -> Key {
        let key;

        if let Some(slot) = self.slots.get_mut(self.next_free as usize) {
            debug_assert!(slot.is_vacant());

            slot.generation += 1;

            key = Key {
                index: self.next_free,
                generation: unsafe { NonZeroU32::new_unchecked(slot.generation) },
            };

            self.next_free = unsafe { slot.union.next_free };

            slot.union.value = ManuallyDrop::new(value);
        } else {
            let index: u32 = self.slots.len() as u32;

            if index == u32::MAX {
                panic!("reached maximum number of slotmap elements");
            }

            key = Key {
                index,
                generation: ONE,
            };

            self.slots.push(Slot {
                union: SlotUnion {
                    value: ManuallyDrop::new(value),
                },
                generation: 1,
            });
        };

        self.len += 1;

        key
    }

    pub(crate) fn remove(&mut self, key: Key) -> Option<T> {
        let slot = self.slots.get_mut(key.index as usize)?;

        if slot.generation != key.generation.get() {
            return None;
        }

        slot.generation = slot.generation.wrapping_add(1);

        let res = unsafe { ManuallyDrop::take(&mut slot.union.value) };

        // If the generation overflowed then we consider the slot retired and won't try
        // to use it again.
        if slot.generation != 0 {
            slot.union.next_free = self.next_free;
            self.next_free = key.index;
        }

        self.len -= 1;

        Some(res)
    }

    pub(crate) fn get(&self, key: Key) -> Option<&T> {
        let slot = self.slots.get(key.index as usize)?;

        if slot.generation != key.generation.get() {
            return None;
        }

        Some(unsafe { &slot.union.value })
    }

    pub(crate) fn get_mut(&mut self, key: Key) -> Option<&mut T> {
        let slot = self.slots.get_mut(key.index as usize)?;

        if slot.generation != key.generation.get() {
            return None;
        }

        Some(unsafe { &mut slot.union.value })
    }

    pub(crate) fn key_at_index(&self, idx: u32) -> Option<Key> {
        let slot = self.slots.get(idx as usize)?;

        if slot.is_vacant() {
            return None;
        }

        Some(unsafe { Key::new_unchecked(idx, slot.generation) })
    }

    /// Returns the number of occupied slots.
    pub(crate) const fn len(&self) -> u32 {
        self.len
    }

    pub(crate) const fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl<T> Default for SlotMap<T> {
    fn default() -> Self {
        Self::new()
    }
}

struct Slot<T> {
    union: SlotUnion<T>,
    generation: u32,
}

impl<T> Slot<T> {
    const fn is_vacant(&self) -> bool {
        self.generation % 2 == 0
    }
}

impl<T> Drop for Slot<T> {
    fn drop(&mut self) {
        if !self.is_vacant() {
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
            s.field("value", unsafe { &self.union.value });
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
    index: u32,
    generation: NonZeroU32,
}

impl Key {
    pub(crate) const NULL: Self = Self {
        index: u32::MAX,
        generation: NonZeroU32::MAX,
    };

    pub(crate) const fn new(index: u32, generation: NonZeroU32) -> Option<Self> {
        // LSB of generation must be 1 for safety.
        if generation.get() % 2 == 0 {
            None
        } else {
            Some(Self { index, generation })
        }
    }

    const unsafe fn new_unchecked(index: u32, generation: u32) -> Self {
        Self {
            index,
            generation: NonZeroU32::new_unchecked(generation),
        }
    }

    pub(crate) const fn index(self) -> u32 {
        self.index
    }

    pub(crate) const fn generation(self) -> NonZeroU32 {
        self.generation
    }
}

impl Default for Key {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.index, self.generation)
    }
}

const ONE: NonZeroU32 = match NonZeroU32::new(1) {
    Some(n) => n,
    None => unreachable!(),
};

#[cfg(test)]
mod tests {
    use std::cell::Cell;
    use std::collections::BTreeSet;
    use std::rc::Rc;

    use super::*;

    #[test]
    fn slotmap_insert_remove() {
        let mut sm = SlotMap::new();

        let k = sm.insert(123);
        assert_eq!(sm.remove(Key::NULL), None);
        assert_eq!(sm.remove(k), Some(123));
        let k1 = sm.insert(123);
        let k2 = sm.insert(456);
        let k3 = sm.insert(789);
        assert_eq!(BTreeSet::from_iter([k, k1, k2, k3]).len(), 4);
        assert_eq!(sm.remove(Key::NULL), None);
        assert_eq!(sm.remove(k2), Some(456));
        assert_eq!(sm.remove(k2), None);
        assert_eq!(sm.len(), 2);
        assert_eq!(sm.remove(k1), Some(123));
        assert_eq!(sm.remove(k3), Some(789));
        assert!(sm.is_empty());
    }

    #[test]
    fn slotmap_get() {
        let mut sm = SlotMap::new();

        let k1 = sm.insert(123);
        let k2 = sm.insert(456);

        assert_eq!(sm.get(k1), Some(&123));
        assert_eq!(sm.get_mut(k2), Some(&mut 456));
        assert_eq!(sm.get(Key::NULL), None);

        assert_eq!(sm.remove(k2), Some(456));

        assert_eq!(sm.get(k1), Some(&123));
        assert_eq!(sm.get_mut(k1), Some(&mut 123));
    }

    #[test]
    fn slotmap_retires_slot() {
        let mut sm = SlotMap::new();

        sm.insert(123);
        sm.slots[0].generation = u32::MAX;

        let k = Key {
            index: 0,
            generation: NonZeroU32::MAX,
        };

        assert_eq!(sm.remove(k), Some(123));
        let k2 = sm.insert(456);

        assert_ne!(k2.index(), 0);
    }

    #[test]
    fn slotmap_drops_items() {
        struct Foo(Rc<Cell<usize>>);

        impl Drop for Foo {
            fn drop(&mut self) {
                self.0.set(self.0.get() + 1);
            }
        }

        let mut sm = SlotMap::new();
        let count = Rc::new(Cell::new(0));

        sm.insert(Foo(count.clone()));
        let k = sm.insert(Foo(count.clone()));
        sm.insert(Foo(count.clone()));

        sm.remove(k);

        drop(sm);

        assert_eq!(count.get(), 3);
    }
}
