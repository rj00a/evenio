use core::fmt;

use slab::Slab;

pub(crate) trait GetDebugChecked {
    type Output: ?Sized;

    #[track_caller]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output;
    #[track_caller]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output;
}

impl<T> GetDebugChecked for [T] {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        #[cfg(debug_assertions)]
        return &self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked(idx);
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        #[cfg(debug_assertions)]
        return &mut self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked_mut(idx);
    }
}

impl<T> GetDebugChecked for Vec<T> {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        self.as_slice().get_debug_checked(idx)
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.as_mut_slice().get_debug_checked_mut(idx)
    }
}

// Don't use `Slab::get_unchecked` because there's a panicking branch. https://github.com/tokio-rs/slab/pull/74
impl<T> GetDebugChecked for Slab<T> {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        self.get(idx).expect_debug_checked("invalid slab key")
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.get_mut(idx).expect_debug_checked("invalid slab key")
    }
}

pub(crate) trait UnwrapDebugChecked {
    type Output;

    #[track_caller]
    unsafe fn unwrap_debug_checked(self) -> Self::Output;
    #[track_caller]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output;
}

impl<T> UnwrapDebugChecked for Option<T> {
    type Output = T;

    #[inline]
    unsafe fn unwrap_debug_checked(self) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.unwrap();

        #[cfg(not(debug_assertions))]
        return self.unwrap_unchecked();
    }

    #[inline]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.expect(msg);

        #[cfg(not(debug_assertions))]
        {
            let _ = msg;
            return self.unwrap_unchecked();
        }
    }
}

impl<T, E> UnwrapDebugChecked for Result<T, E>
where
    E: fmt::Debug,
{
    type Output = T;

    #[inline]
    unsafe fn unwrap_debug_checked(self) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.unwrap();

        #[cfg(not(debug_assertions))]
        return self.unwrap_unchecked();
    }

    #[inline]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.expect(msg);

        #[cfg(not(debug_assertions))]
        {
            let _ = msg;
            return self.unwrap_unchecked();
        }
    }
}

#[inline]
#[track_caller]
pub(crate) unsafe fn unreachable_debug_checked() -> ! {
    #[cfg(debug_assertions)]
    unreachable!();

    #[cfg(not(debug_assertions))]
    std::hint::unreachable_unchecked();
}

#[inline]
#[track_caller]
pub(crate) unsafe fn assume_debug_checked(cond: bool) {
    if !cond {
        unreachable_debug_checked()
    }
}