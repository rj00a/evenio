//! Utilities for runtime and compile-time assertions.

use core::marker::PhantomData;
use core::slice::SliceIndex;
use core::{fmt, mem};

use slab::Slab;

use crate::component::Component;
use crate::event::Event;

const _: () = assert!(
    mem::size_of::<usize>() >= mem::size_of::<u32>(),
    "unsupported target"
);

/// Extension trait for checked array indexing with checks removed in release
/// mode.
pub(crate) trait GetDebugChecked<Idx> {
    type Output: ?Sized;

    /// Gets a reference to the element at the given index.
    ///
    /// If `idx` is not in bounds, a panic occurs in debug mode and Undefined
    /// Behavior occurs in release mode.
    ///
    /// # Safety
    ///
    /// - `idx` must be in bounds.
    #[track_caller]
    unsafe fn get_debug_checked(&self, idx: Idx) -> &Self::Output;
    /// Gets a mutable reference to the element at the given index.
    ///
    /// If `idx` is not in bounds, a panic occurs in debug mode and Undefined
    /// Behavior occurs in release mode.
    ///
    /// # Safety
    ///
    /// - `idx` must be in bounds.
    #[track_caller]
    unsafe fn get_debug_checked_mut(&mut self, idx: Idx) -> &mut Self::Output;
}

impl<T, I> GetDebugChecked<I> for [T]
where
    I: SliceIndex<[T]>,
{
    type Output = I::Output;

    unsafe fn get_debug_checked(&self, idx: I) -> &Self::Output {
        #[cfg(debug_assertions)]
        return &self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked(idx);
    }

    unsafe fn get_debug_checked_mut(&mut self, idx: I) -> &mut Self::Output {
        #[cfg(debug_assertions)]
        return &mut self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked_mut(idx);
    }
}

// Don't use `Slab::get_unchecked` because there's a panicking branch. https://github.com/tokio-rs/slab/pull/74
impl<T> GetDebugChecked<usize> for Slab<T> {
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

/// Extension trait for checked unwrapping with checks removed in release
/// mode.
pub(crate) trait UnwrapDebugChecked {
    type Output;

    /// Consumes `self` and returns the inner value.
    ///
    /// # Safety
    ///
    /// - Must successfully unwrap (`Some` in `Option`, `Ok` in `Result`, etc.)
    #[track_caller]
    unsafe fn unwrap_debug_checked(self) -> Self::Output;
    /// Like [`Self::unwrap_debug_checked`] but panics with the given error
    /// message. The message is ignored in release mode.
    ///
    /// # Safety
    ///
    /// - Must successfully unwrap (`Some` in `Option`, `Ok` in `Result`, etc.)
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
    core::hint::unreachable_unchecked();
}

#[inline]
#[track_caller]
pub(crate) unsafe fn assume_debug_checked(cond: bool) {
    if !cond {
        unreachable_debug_checked()
    }
}

pub(crate) struct AssertMutable<T>(PhantomData<T>);

impl<C: Component> AssertMutable<C> {
    pub(crate) const COMPONENT: () = assert!(
        !C::IS_IMMUTABLE,
        "component does not permit mutation through mutable references (see \
         `Component::IS_IMMUTABLE`)."
    );
}

impl<E: Event> AssertMutable<E> {
    pub(crate) const EVENT: () = assert!(
        !E::IS_IMMUTABLE,
        "event does not permit mutation through mutable references (see `Event::IS_IMMUTABLE`)."
    );
}

pub(crate) struct AssertUntargetedEvent<E>(PhantomData<E>);

impl<E: Event> AssertUntargetedEvent<E> {
    pub(crate) const ASSERTION: () = assert!(
        !E::IS_TARGETED,
        "event is targeted (See `Event::IS_TARGETED`)"
    );
}

pub(crate) struct AssertTargetedEvent<E>(PhantomData<E>);

impl<E: Event> AssertTargetedEvent<E> {
    pub(crate) const ASSERTION: () = assert!(
        E::IS_TARGETED,
        "event is untargeted (See `Event::IS_TARGETED`)"
    );
}
