use alloc::boxed::Box;
use core::borrow::Borrow;
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::ops::{Deref, DerefMut};
use core::ptr::NonNull;
use core::{fmt, mem};

/// Like [`Box`], but permits raw pointer-like aliasing. `AliasedBox` owns its
/// `T` and is responsible for dropping the inner value and deallocating the
/// backing store.
///
/// The current aliasing rules for the standard `Box` are too strict for our
/// `unsafe` trickery. To get around this, we create `AliasedBox` -- A simple
/// wrapper around a [`NonNull`].
///
/// [`std::boxed`] has this to say:
///
/// > **Warning: This section is not normative and is subject to change,
/// > possibly being relaxed in the future! It is a simplified summary of the
/// > rules currently implemented in the compiler.**
/// >
/// > The aliasing rules for `Box<T>` are the same as for `&mut T`. `Box<T>`
/// > asserts uniqueness over its content. Using raw pointers derived from a
/// > box after that box has been mutated through, moved or borrowed as
/// > `&mut T` is not allowed. For more guidance on working with box from
/// > unsafe code, see [rust-lang/unsafe-code-guidelines#326][issue].
///
/// [issue]: https://github.com/rust-lang/unsafe-code-guidelines/issues/326
#[repr(transparent)]
pub(crate) struct AliasedBox<T: ?Sized>(NonNull<T>);

impl<T> AliasedBox<T> {
    #[must_use]
    pub(crate) fn new(x: T) -> Self {
        Box::new(x).into()
    }

    #[must_use]
    #[allow(dead_code)]
    pub(crate) fn into_inner(this: Self) -> T {
        let ptr = this.0.as_ptr();
        mem::forget(this);
        *unsafe { Box::from_raw(ptr) }
    }
}

impl<T: ?Sized> AliasedBox<T> {
    /// Returns a (non-null) pointer to the inner value without forming an
    /// intermediate reference.
    #[must_use]
    pub(crate) fn as_ptr(this: &Self) -> *const T {
        this.0.as_ptr().cast_const()
    }

    /// Returns a (non-null) mutable pointer to the inner value without forming
    /// an intermediate reference.
    #[must_use]
    pub(crate) fn as_mut_ptr(this: &mut Self) -> *mut T {
        this.0.as_ptr()
    }

    /// Returns a (non-null) pointer to the inner value without forming an
    /// intermediate reference.
    pub(crate) fn as_non_null(this: &Self) -> NonNull<T> {
        this.0
    }
}

impl<T: ?Sized> Deref for AliasedBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<T: ?Sized> DerefMut for AliasedBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl<T: ?Sized> AsRef<T> for AliasedBox<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: ?Sized> AsMut<T> for AliasedBox<T> {
    fn as_mut(&mut self) -> &mut T {
        self
    }
}

impl<T: ?Sized> Borrow<T> for AliasedBox<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: Default> Default for AliasedBox<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T> Default for AliasedBox<[T]> {
    fn default() -> Self {
        Self(NonNull::from(<&[T]>::default()))
    }
}

impl<T: ?Sized> From<Box<T>> for AliasedBox<T> {
    fn from(value: Box<T>) -> Self {
        // SAFETY: `Box::into_raw` guarantees non-null.
        Self(unsafe { NonNull::new_unchecked(Box::into_raw(value)) })
    }
}

impl<T: Clone> Clone for AliasedBox<T> {
    fn clone(&self) -> Self {
        AliasedBox::new(self.as_ref().clone())
    }
}

impl<T: ?Sized + PartialEq> PartialEq for AliasedBox<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl<T: ?Sized + Eq> Eq for AliasedBox<T> {}

impl<T: ?Sized + PartialOrd> PartialOrd for AliasedBox<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T: ?Sized + Ord> Ord for AliasedBox<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: ?Sized + Hash> Hash for AliasedBox<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for AliasedBox<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("AliasedBox").field(&self.as_ref()).finish()
    }
}

impl<T: ?Sized> Drop for AliasedBox<T> {
    fn drop(&mut self) {
        let _ = unsafe { Box::from_raw(self.0.as_ptr()) };
    }
}

unsafe impl<T: ?Sized + Send> Send for AliasedBox<T> {}

unsafe impl<T: ?Sized + Sync> Sync for AliasedBox<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aliased_box_create_destroy() {
        let _ = AliasedBox::new("abc123".to_owned());
    }

    #[test]
    fn aliased_box_into_inner() {
        let _ = AliasedBox::into_inner(AliasedBox::new("abc123".to_owned()));
    }
}
