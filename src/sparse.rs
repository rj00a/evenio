//! The [`SparseIndex`] trait.

/// Something that can be used as an integer key in a sparse data structure,
/// such as a bit set.
///
/// Ideally the values are as small as possible.
///
/// # Laws
///
/// 1. `from_index(value.index()) == value` for all `value`.
/// 2. `MAX.index() >= value.index()` for all `value`.
///
/// # Safety
///
/// This trait is `unsafe` because unsafe code may depend on implementations'
/// correctness.
pub(crate) unsafe trait SparseIndex: Copy + Clone {
    /// The maximum supported value.
    const MAX: Self;

    /// Converts `self` to a `usize` index.
    fn index(self) -> usize;

    /// Convert from a `usize` to `Self`.
    ///
    /// The input must be no larger than [`Self::MAX`]. Otherwise the result is
    /// unspecified.
    fn from_index(idx: usize) -> Self;
}

macro_rules! impl_sparse_index {
    ($t:ty) => {
        #[allow(trivial_numeric_casts)]
        unsafe impl SparseIndex for $t {
            const MAX: Self = Self::MAX;

            #[inline]
            fn index(self) -> usize {
                self as usize
            }

            #[inline]
            fn from_index(idx: usize) -> Self {
                debug_assert!(TryInto::<Self>::try_into(idx).is_ok(), "{idx} out of range");

                idx as Self
            }
        }
    };
}

impl_sparse_index!(u8);
impl_sparse_index!(u16);
impl_sparse_index!(u32);
impl_sparse_index!(usize);
