//! Reimplementation of the standard `Exclusive`/`SyncCell` type.
//!
//! Once `Exclusive` is stabilized, this module should be removed.
//! See <https://github.com/rust-lang/rust/issues/98407>

use core::fmt;

#[derive(Default)]
#[repr(transparent)]
pub struct Exclusive<T> {
    inner: T,
}

// SAFETY: Only `&mut` access to `T` is provided when `T: !Sync`, so this is
// safe.
unsafe impl<T> Sync for Exclusive<T> {}

impl<T> Exclusive<T> {
    pub fn new(t: T) -> Self {
        Self { inner: t }
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T: Sync> Exclusive<T> {
    pub fn get(&self) -> &T {
        // This is safe because `T: Sync`.
        &self.inner
    }
}

impl<T> fmt::Debug for Exclusive<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Exclusive").finish_non_exhaustive()
    }
}
