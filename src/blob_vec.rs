use alloc::alloc;
use core::alloc::Layout;
use core::ptr;
use core::ptr::NonNull;

use crate::drop::DropFn;
use crate::layout_util::pad_to_align;

/// Like `Vec<T>`, but `T` is erased.
#[derive(Debug)]
pub(crate) struct BlobVec {
    /// Layout of a single element.
    elem_layout: Layout,
    /// Number of elements.
    len: usize,
    /// Capacity of allocated buffer.
    cap: usize,
    /// Pointer to beginning of allocated buffer.
    data: NonNull<u8>,
    /// The erased element type's drop function, if any.
    drop: DropFn,
}

impl BlobVec {
    /// # Safety
    /// - `drop` must be safe to call with elements of this `BlobVec` as
    ///   described by [`DropFn`]'s documentation.
    pub(crate) unsafe fn new(layout: Layout, drop: DropFn) -> Self {
        Self {
            elem_layout: pad_to_align(&layout),
            len: 0,
            cap: if layout.size() == 0 { usize::MAX } else { 0 },
            data: NonNull::dangling(),
            drop,
        }
    }

    pub(crate) unsafe fn push(&mut self) -> NonNull<u8> {
        self.reserve(1);

        let slot = self.data.as_ptr().add(self.elem_layout.size() * self.len);

        self.len += 1;

        NonNull::new_unchecked(slot)
    }

    unsafe fn swap_remove_no_drop(&mut self, idx: usize) {
        debug_assert!(idx < self.len, "index out of bounds");

        let src = self
            .data
            .as_ptr()
            .add(self.elem_layout.size() * (self.len - 1));

        let dst = self.data.as_ptr().add(self.elem_layout.size() * idx);

        self.len -= 1;

        if src != dst {
            ptr::copy_nonoverlapping(src, dst, self.elem_layout.size());
        }
    }

    pub(crate) unsafe fn swap_remove(&mut self, idx: usize) {
        debug_assert!(idx < self.len, "index out of bounds");

        let src = self
            .data
            .as_ptr()
            .add(self.elem_layout.size() * (self.len - 1));

        let dst = self.data.as_ptr().add(self.elem_layout.size() * idx);

        if let Some(drop) = self.drop {
            drop(NonNull::new_unchecked(dst));
        }

        self.len -= 1;

        if src != dst {
            ptr::copy_nonoverlapping(src, dst, self.elem_layout.size());
        }
    }

    pub(crate) unsafe fn assign(&mut self, idx: usize, elem: *const u8) {
        debug_assert!(idx < self.len, "index out of bounds");

        let ptr = self.data.as_ptr().add(idx * self.elem_layout.size());

        if let Some(drop) = self.drop {
            drop(NonNull::new_unchecked(ptr));
        }

        ptr::copy_nonoverlapping(elem, ptr, self.elem_layout.size());
    }

    #[cfg(test)]
    fn get_mut(&mut self, idx: usize) -> Option<NonNull<u8>> {
        if idx >= self.len {
            return None;
        }

        Some(unsafe {
            NonNull::new(self.data.as_ptr().add(idx * self.elem_layout.size())).unwrap_unchecked()
        })
    }

    /// Move an element from `self` to `other`. The element at `src_idx` is
    /// swap removed from `self` and pushed onto the end of `other`.
    ///
    /// # Safety
    /// - `src_idx` must be in bounds within `self`.
    /// - Underlying types of `self` and `other` must be interchangeable.
    pub(crate) unsafe fn transfer_elem(&mut self, other: &mut Self, src_idx: usize) {
        debug_assert_eq!(
            self.elem_layout, other.elem_layout,
            "elem layouts must be the same"
        );
        debug_assert!(src_idx < self.len, "index out of bounds");

        let src = self.data.as_ptr().add(src_idx * self.elem_layout.size());
        let dst = other.push().as_ptr();

        ptr::copy_nonoverlapping(src, dst, self.elem_layout.size());
        self.swap_remove_no_drop(src_idx);
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

            let Some(new_cap_in_bytes) = new_cap.checked_mul(self.elem_layout.size()) else {
                capacity_overflow()
            };

            if new_cap_in_bytes > isize::MAX as usize {
                capacity_overflow()
            }

            // SAFETY:
            // - `new_cap_in_bytes` is <= `isize::MAX` from above check (size is multiple of
            //   align).
            // - Alignment is from layout so must be valid.
            let new_cap_layout = unsafe {
                Layout::from_size_align_unchecked(new_cap_in_bytes, self.elem_layout.align())
            };

            // The current layout of the capacity.
            let old_cap_layout = self.capacity_layout();

            let ptr = if old_cap_layout.size() == 0 {
                // SAFETY: `new_cap_layout` is nonzero due to previous ZST check.
                unsafe { alloc::alloc(new_cap_layout) }
            } else {
                // SAFETY:
                // - `old_cap_layout` size is nonzero, so `data` must be currently allocated via
                //   the global allocator.
                // - `old_cap_layout` is the previous layout of the data.
                // - `new_cap_layout` size does not exceed `isize::MAX` because of `Layout`
                //   invariant, and is nonzero due to previous ZST check.
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
        // Set length to zero first in case `drop` unwinds. Otherwise, we could end up
        // calling the destructor more than once.
        let len = self.len;
        self.len = 0;

        if let Some(drop) = self.drop {
            let elem_size = self.elem_layout.size();

            for i in 0..len {
                let elem = unsafe { self.data.as_ptr().add(i * elem_size) };
                // SAFETY:
                // - `elem` points to a valid element.
                // - `elem` is nonnull.
                unsafe { drop(NonNull::new_unchecked(elem)) }
            }
        }
    }

    /// Returns the layout of the entire allocated buffer owned by this
    /// `BlobVec`.
    pub(crate) fn capacity_layout(&self) -> Layout {
        unsafe {
            Layout::from_size_align(self.elem_layout.size() * self.cap, self.elem_layout.align())
                .unwrap_unchecked()
        }
    }

    /// Returns the layout of a single element.
    ///
    /// Note that the size has been rounded up to a multiple of the alignment,
    /// which may mean the [`Layout`] returned by this method is different than
    /// the one given to `BlobVec::new`.
    pub(crate) fn elem_layout(&self) -> Layout {
        self.elem_layout
    }

    pub(crate) fn as_ptr(&self) -> NonNull<u8> {
        self.data
    }

    pub(crate) fn len(&self) -> usize {
        self.len
    }

    pub(crate) fn capacity(&self) -> usize {
        self.cap
    }
}

impl Drop for BlobVec {
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

#[cold]
fn capacity_overflow() -> ! {
    panic!("capacity overflow")
}

#[cfg(test)]
mod tests {
    use ::alloc::rc::Rc;

    use super::*;
    use crate::drop::drop_fn_of;

    fn new_blob_vec<T>() -> BlobVec {
        unsafe { BlobVec::new(Layout::new::<T>(), drop_fn_of::<T>()) }
    }

    #[test]
    fn calls_drop_on_elements() {
        type T = Rc<()>;

        let elem = T::new(());

        let mut vec = new_blob_vec::<T>();

        for _ in 0..5 {
            unsafe {
                let ptr = vec.push().as_ptr().cast::<T>();
                ptr.write(elem.clone());
            }
        }

        drop(vec);

        assert_eq!(Rc::strong_count(&elem), 1);
    }

    #[test]
    fn swap_remove() {
        let mut vec = new_blob_vec::<String>();

        let strings = ["aaa", "bbb", "ccc", "ddd"];

        for s in strings {
            unsafe {
                vec.push().as_ptr().cast::<String>().write(s.into());
            }
        }

        let ptr = vec.as_ptr().cast::<String>().as_ptr();

        unsafe {
            vec.swap_remove(1);

            assert_eq!(*ptr.add(1), "ddd");

            vec.swap_remove(2);

            assert_eq!(*ptr.add(1), "ddd");

            vec.swap_remove(0);

            assert_eq!(*ptr, "ddd");

            vec.swap_remove(0);
            assert_eq!(vec.len, 0);
        }
    }

    #[test]
    fn unusual_alignment() {
        unsafe {
            let mut vec = BlobVec::new(Layout::from_size_align(5, 128).unwrap(), None);

            #[track_caller]
            fn check(ptr: NonNull<u8>) {
                assert!(ptr.as_ptr() as usize % 128 == 0);
            }

            check(vec.push());
            check(vec.push());
            check(vec.push());

            vec.swap_remove(1);
            check(vec.get_mut(1).unwrap());
        }
    }
}
