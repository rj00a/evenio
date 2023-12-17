pub unsafe trait SparseIndex: Copy + Clone {
    const MAX: Self;

    fn index(self) -> usize;
    fn from_index(idx: usize) -> Self;
}

macro_rules! impl_sparse_index {
    ($t:ty) => {
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
