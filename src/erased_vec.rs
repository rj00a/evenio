use std::alloc;
use std::alloc::Layout;
use std::ptr::NonNull;

use crate::util::{capacity_overflow, UnwrapDebugChecked};

/// Like `Vec<T>`, but `T` is erased.
#[derive(Debug)]
pub(crate) struct ErasedVec {
    /// Layout of a single element.
    elem_layout: Layout,
    /// Number of elements.
    len: usize,
    cap: usize,
    /// Pointer to the element array.
    data: NonNull<u8>,
    /// The erased element type's drop function, if any.
    drop: Option<unsafe fn(NonNull<u8>)>,
}

impl ErasedVec {
    /// # Safety
    /// - `layout`'s size must be evenly divisble by its alignment.
    /// - If `Some`, `drop` must be safe to call with an aligned pointer to an
    ///   ErasedVec's element.
    pub(crate) unsafe fn new(layout: Layout, drop: Option<unsafe fn(NonNull<u8>)>) -> Self {
        debug_assert_eq!(padding_needed_for(&layout, layout.align()), 0);

        Self {
            elem_layout: layout,
            len: 0,
            cap: if layout.size() == 0 { usize::MAX } else { 0 },
            data: NonNull::dangling(),
            drop,
        }
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        let available = self.cap - self.len;

        if additional > available {
            let Some(required_cap) = self.len.checked_add(additional) else {
                // ZSTs will always reach this because `cap` is `usize::MAX`.
                capacity_overflow()
            };

            debug_assert_ne!(self.elem_layout.size(), 0);

            // This doubling cannot overflow because `self.cap <= isize::MAX` and the type
            // of `cap` is `usize`.
            let new_cap = (self.cap * 2).max(required_cap);

            // Get the new layout of the new allocation and check that it doesn't exceed
            // `isize::MAX`.
            let Some((new_cap_layout, _)) = repeat_layout(&self.elem_layout, new_cap) else {
                capacity_overflow()
            };

            // The current layout of the capacity.
            let old_cap_layout = self.capacity_layout();

            debug_assert!(old_cap_layout.size() <= isize::MAX as usize);
            debug_assert!((1..isize::MAX as usize).contains(&new_cap_layout.size()));

            let ptr = if old_cap_layout.size() == 0 {
                // SAFETY: `new_cap_layout` was checked for validity by `array_layout`.
                unsafe { alloc::alloc(new_cap_layout) }
            } else {
                // SAFETY:
                // - `old_cap_layout` size is nonzero, so `data` must be currently allocated via
                //   the global allocator.
                // - `old_cap_layout` is the previous layout of the data.
                // - `new_cap_layout` size does not exceed `isize::MAX` by call to
                //   `array_layout`, and is nonzero due to nonzero `additional` and ZST check.
                unsafe { alloc::realloc(self.data.as_ptr(), old_cap_layout, new_cap_layout.size()) }
            };

            // Check for memory allocation failure before setting new capacity
            // because `handle_alloc_error` could potentially unwind.
            match NonNull::new(ptr) {
                Some(data) => self.data = data,
                None => alloc::handle_alloc_error(new_cap_layout),
            }

            self.cap = new_cap;
        }
    }

    pub(crate) fn clear(&mut self) {
        // Set length to zero first in case `drop` unwinds.
        self.len = 0;

        if let Some(drop) = self.drop {
            let elem_size = self.elem_layout.size();

            for i in 0..self.len {
                let elem = unsafe { self.data.as_ptr().add(i * elem_size) };
                // SAFETY:
                // - `elem` points to a valid element.
                // - `elem` is nonnull.
                unsafe { drop(NonNull::new_unchecked(elem)) }
            }
        }
    }

    /// Returns the layout of the entire allocated buffer owned by this
    /// ErasedVec.
    pub(crate) fn capacity_layout(&self) -> Layout {
        // SAFETY: Capacity layout was validated when it was last changed.
        unsafe {
            repeat_layout(&self.elem_layout, self.cap)
                .expect_debug_checked("current capacity layout should be valid")
                .0
        }
    }
}

impl Drop for ErasedVec {
    fn drop(&mut self) {
        self.clear();

        let cap_layout = self.capacity_layout();

        if cap_layout.size() > 0 {
            // SAFETY: Ptr is currently allocated because size is nonzero, and `cap_layout`
            // was the layout used for the allocation.
            unsafe {
                alloc::dealloc(self.data.as_ptr(), cap_layout);
            }
        }
    }
}

// TODO: replace with `Layout::repeat` if/when it stabilizes.
/// Creates a layout describing the record for `n` instances of
/// `self`, with a suitable amount of padding between each to
/// ensure that each instance is given its requested size and
/// alignment. On success, returns `(k, offs)` where `k` is the
/// layout of the array and `offs` is the distance between the start
/// of each element in the array.
///
/// On arithmetic overflow, returns `LayoutError`.
#[inline]
#[must_use]
fn repeat_layout(layout: &Layout, n: usize) -> Option<(Layout, usize)> {
    // This cannot overflow. Quoting from the invariant of Layout:
    // > `size`, when rounded up to the nearest multiple of `align`,
    // > must not overflow (i.e., the rounded value must be less than
    // > `usize::MAX`)
    let padded_size = layout.size() + padding_needed_for(layout, layout.align());
    let alloc_size = padded_size.checked_mul(n)?;

    // SAFETY: self.align is already known to be valid and alloc_size has been
    // padded already.
    unsafe {
        Some((
            Layout::from_size_align_unchecked(alloc_size, layout.align()),
            padded_size,
        ))
    }
}

// TODO: replace with `Layout::padding_needed_for` if/when it stabilizes.
/// Returns the amount of padding we must insert after `self`
/// to ensure that the following address will satisfy `align`
/// (measured in bytes).
///
/// e.g., if `self.size()` is 9, then `self.padding_needed_for(4)`
/// returns 3, because that is the minimum number of bytes of
/// padding required to get a 4-aligned address (assuming that the
/// corresponding memory block starts at a 4-aligned address).
///
/// The return value of this function has no meaning if `align` is
/// not a power-of-two.
///
/// Note that the utility of the returned value requires `align`
/// to be less than or equal to the alignment of the starting
/// address for the whole allocated block of memory. One way to
/// satisfy this constraint is to ensure `align <= self.align()`.
#[inline]
#[must_use]
const fn padding_needed_for(layout: &Layout, align: usize) -> usize {
    let len = layout.size();

    // Rounded up value is:
    //   len_rounded_up = (len + align - 1) & !(align - 1);
    // and then we return the padding difference: `len_rounded_up - len`.
    //
    // We use modular arithmetic throughout:
    //
    // 1. align is guaranteed to be > 0, so align - 1 is always valid.
    //
    // 2. `len + align - 1` can overflow by at most `align - 1`, so the &-mask with
    //    `!(align - 1)` will ensure that in the case of overflow, `len_rounded_up`
    //    will itself be 0. Thus the returned padding, when added to `len`, yields
    //    0, which trivially satisfies the alignment `align`.
    //
    // (Of course, attempts to allocate blocks of memory whose
    // size and padding overflow in the above manner should cause
    // the allocator to yield an error anyway.)

    let len_rounded_up = len.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
    len_rounded_up.wrapping_sub(len)
}

// TODO: replace with `Layout::pad_to_align` if/when it stabilizes.
/// Creates a layout by rounding the size of this layout up to a multiple
/// of the layout's alignment.
///
/// This is equivalent to adding the result of `padding_needed_for`
/// to the layout's current size.
#[inline]
#[must_use]
const fn pad_to_align(layout: &Layout) -> Layout {
    let pad = padding_needed_for(layout, layout.align());
    // This cannot overflow. Quoting from the invariant of Layout:
    // > `size`, when rounded up to the nearest multiple of `align`,
    // > must not overflow isize (i.e., the rounded value must be
    // > less than or equal to `isize::MAX`)
    let new_size = layout.size() + pad;

    // SAFETY: padded size is guaranteed to not exceed `isize::MAX`.
    unsafe { Layout::from_size_align_unchecked(new_size, layout.align()) }
}
