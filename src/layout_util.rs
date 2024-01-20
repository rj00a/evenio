//! Contains reimplementations of some unstable [`Layout`] functions taken from
//! the standard library. This module should be removed if/when
//! [`alloc_layout_extra`] is stabilized.
//!
//! [`alloc_layout_extra`]: https://github.com/rust-lang/rust/issues/55724

use core::alloc::Layout;

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
pub(crate) const fn padding_needed_for(layout: &Layout, align: usize) -> usize {
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

/// Creates a layout by rounding the size of this layout up to a multiple
/// of the layout's alignment.
///
/// This is equivalent to adding the result of `padding_needed_for`
/// to the layout's current size.
#[inline]
#[must_use]
pub(crate) const fn pad_to_align(layout: &Layout) -> Layout {
    let pad = padding_needed_for(layout, layout.align());
    // This cannot overflow. Quoting from the invariant of Layout:
    // > `size`, when rounded up to the nearest multiple of `align`,
    // > must not overflow isize (i.e., the rounded value must be
    // > less than or equal to `isize::MAX`)
    let new_size = layout.size() + pad;

    // SAFETY: padded size is guaranteed to not exceed `isize::MAX`.
    unsafe { Layout::from_size_align_unchecked(new_size, layout.align()) }
}
