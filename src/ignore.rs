use core::panic::{RefUnwindSafe, UnwindSafe};

#[derive(Copy, Clone)]
enum Void {}

/// A generic type which
/// - is zero-sized
/// - is impossible to construct (like `!`).
/// - implements the auto traits irrespective of `T`.
/// - is covariant in `T`.
///
/// Useful for creating [unit structs with type parameters](https://github.com/dtolnay/case-studies/blob/master/unit-type-parameters/README.md).
///
/// This type is necessarily public but should not be directly accessible by
/// users.
#[allow(missing_debug_implementations)]
#[repr(packed)]
pub struct Ignore<T: ?Sized>([*const T; 0], Void);

impl<T: ?Sized> Copy for Ignore<T> {}

impl<T: ?Sized> Clone for Ignore<T> {
    fn clone(&self) -> Self {
        *self
    }
}

// SAFETY: `Ignore` does not actually contain a `T`.
unsafe impl<T: ?Sized> Send for Ignore<T> {}

// SAFETY: `Ignore` does not actually contain a `T`.
unsafe impl<T: ?Sized> Sync for Ignore<T> {}

impl<T: ?Sized> UnwindSafe for Ignore<T> {}

impl<T: ?Sized> RefUnwindSafe for Ignore<T> {}

#[test]
fn enum_with_ignored_variant_is_zero_sized() {
    enum Test {
        _A,
        _B(Ignore<u64>),
    }

    assert_eq!(core::mem::size_of::<Test>(), 0);
}
