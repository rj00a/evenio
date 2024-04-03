use crate::{slot_map::Key, sparse::SparseIndex};

/// Lightweight identifier for a handler.
///
/// Handler identifiers are implemented using an [index] and a generation count.
/// The generation count ensures that IDs from removed handlers are not reused
/// by new handlers.
///
/// A handler identifier is only meaningful in the [`World`] it was created
/// from. Attempting to use a handler ID in a different world will have
/// unexpected results.
///
/// [index]: HandlerIdx
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct HandlerId(pub(super) Key);

impl HandlerId {
    /// The handler ID which never identifies a live handler. This is the
    /// default value for `HandlerId`.
    pub const NULL: Self = Self(Key::NULL);

    /// Returns the index of this ID.
    pub const fn index(self) -> HandlerIdx {
        HandlerIdx(self.0.index())
    }

    /// Returns the generation count of this ID.
    pub const fn generation(self) -> u32 {
        self.0.generation().get()
    }
}

/// A [`HandlerId`] with the generation count stripped out.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct HandlerIdx(pub u32);

unsafe impl SparseIndex for HandlerIdx {
    const MAX: Self = Self(u32::MAX);

    fn index(self) -> usize {
        self.0.index()
    }

    fn from_index(idx: usize) -> Self {
        Self(u32::from_index(idx))
    }
}
