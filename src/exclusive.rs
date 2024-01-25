//! Reimplementation of the standard `Exclusive`/`SyncCell` type.
//!
//! Once `Exclusive` is stabilized, this module should be removed.
//! See <https://github.com/rust-lang/rust/issues/98407>

use core::fmt;

/// A wrapper which implements [`Sync`] irrespective of the wrapped type `T`.
///
/// This is safe because only mutable access to the inner value is provided if
/// `T` is `!Sync`.
#[derive(Default)]
#[repr(transparent)]
pub struct Exclusive<T> {
    inner: T,
}

// SAFETY: Only `&mut` access to `T` is provided when `T: !Sync`, so this is
// safe.
unsafe impl<T> Sync for Exclusive<T> {}

impl<T> Exclusive<T> {
    /// Create a new `Exclusive` wrapper.
    pub fn new(t: T) -> Self {
        Self { inner: t }
    }

    /// For types that implement [`Sync`], get an immutable reference to the
    /// underlying value.
    pub fn get(&self) -> &T
    where
        T: Sync,
    {
        &self.inner
    }

    /// Get a mutable reference to the underlying value.
    pub fn get_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    /// Unwrap the contained value.
    pub fn into_inner(self) -> T {
        self.inner
    }
}

impl<T> fmt::Debug for Exclusive<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Exclusive").finish_non_exhaustive()
    }
}
