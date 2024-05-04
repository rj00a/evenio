//! Utilities for runtime and compile-time assertions.

use core::marker::PhantomData;
use core::mem;

use crate::component::Component;
use crate::event::Event;

const _: () = assert!(
    mem::size_of::<usize>() >= mem::size_of::<u32>(),
    "unsupported target"
);

#[inline]
#[track_caller]
pub(crate) unsafe fn assume_unchecked(cond: bool) {
    if !cond {
        core::hint::unreachable_unchecked()
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

