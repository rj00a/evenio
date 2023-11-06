use std::fmt;
use std::num::NonZeroU32;

use crate::util::{GetDebugChecked, UnwrapDebugChecked};

#[derive(Debug)]
pub(crate) struct Storage<T> {
    entries: Vec<Entry<T>>,
    pending: Vec<u32>,
    reserved_cursor: u32,
}

#[derive(Debug)]
pub(crate) struct Entry<T> {
    generation: NonZeroU32,
    value: Option<T>,
}

impl<T> Storage<T> {
    pub(crate) fn new() -> Self {
        Self {
            entries: vec![],
            pending: vec![],
            reserved_cursor: 0,
        }
    }

    pub(crate) fn get(&self, id: Ident) -> Option<&T> {
        let entry = self.entries.get(id.index as usize)?;

        if entry.generation == id.generation {
            entry.value.as_ref()
        } else {
            None
        }
    }

    pub(crate) unsafe fn get_unchecked(&self, id: Ident) -> &T {
        let entry = &self.entries.get_debug_checked(id.index as usize);

        debug_assert_eq!(entry.generation, id.generation);

        &entry.value.as_ref().unwrap_debug_checked()
    }

    pub(crate) fn get_mut(&mut self, id: Ident) -> Option<&mut T> {
        let entry = self.entries.get_mut(id.index as usize)?;

        if entry.generation == id.generation {
            entry.value.as_mut()
        } else {
            None
        }
    }

    pub(crate) unsafe fn get_unchecked_mut(&mut self, id: Ident) -> &T {
        let entry = self.entries.get_debug_checked_mut(id.index as usize);

        debug_assert_eq!(entry.generation, id.generation);

        entry.value.as_ref().unwrap_debug_checked()
    }

    pub(crate) fn add(&mut self, value: T) -> Ident {
        const ONE: NonZeroU32 = match NonZeroU32::new(1) {
            Some(n) => n,
            None => unreachable!(),
        };

        if let Some(idx) = self.pending.pop() {
            // SAFETY: freelist contains only valid indices.
            let entry = unsafe { self.entries.get_unchecked_mut(idx as usize) };

            let Some(next_gen) = NonZeroU32::new(entry.generation.get().wrapping_add(1)) else {
                // Retire this index permanently.
                return self.add(value);
            };

            entry.generation = next_gen;
            entry.value = Some(value);

            Ident {
                index: idx,
                generation: next_gen,
            }
        } else {
            // Make sure Ident::NULL is always vacant.
            if self.entries.len() >= u32::MAX as usize {
                panic!("too many entries");
            }

            let idx = self.entries.len() as u32;

            self.entries.push(Entry {
                generation: ONE,
                value: Some(value),
            });

            Ident {
                index: idx,
                generation: ONE,
            }
        }
    }

    pub(crate) fn remove(&mut self, id: Ident) -> Option<T> {
        let entry = self.entries.get_mut(id.index as usize)?;

        if entry.generation != id.generation {
            return None;
        }

        if entry.value.is_some() {
            self.pending.push(id.index);
        }

        entry.value.take()
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len() - self.pending.len()
    }
}

impl<T> Default for Storage<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Ident {
    pub(crate) index: u32,
    pub(crate) generation: NonZeroU32,
}

impl Ident {
    pub(crate) const NULL: Self = Self {
        index: u32::MAX,
        generation: NonZeroU32::MAX,
    };

    pub(crate) const fn to_bits(self) -> u64 {
        (self.index as u64) << 32 | self.generation.get() as u64
    }

    pub(crate) const fn from_bits(bits: u64) -> Option<Self> {
        match NonZeroU32::new(bits as u32) {
            Some(gen) => Some(Self {
                index: (bits >> 32) as u32,
                generation: gen,
            }),
            None => None,
        }
    }
}

impl Default for Ident {
    fn default() -> Self {
        Self::NULL
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.index, self.generation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_storage_add_remove() {
        let mut storage = Storage::new();

        let id1 = storage.add("foo");

        let val1 = *storage.get(id1).unwrap();

        assert_eq!(val1, "foo");

        let id2 = storage.add("bar");

        assert!(storage.remove(id1).is_some());

        assert!(storage.get(id1).is_none());
        assert!(storage.remove(id1).is_none());

        let id3 = storage.add("baz");

        assert_ne!(id1, id3);
        assert_ne!(id1, id2);

        assert!(storage.remove(id2).is_some());
    }
}
