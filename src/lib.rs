#![doc = include_str!("../README.md")]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
#![warn(clippy::std_instead_of_alloc, clippy::std_instead_of_core)]
#![allow(clippy::len_without_is_empty, clippy::let_unit_value)]
// Run locally with `RUSTDOCFLAGS="--cfg docsrs" cargo +nightly doc --all-features --open`
#![cfg_attr(docsrs, feature(doc_cfg))]

extern crate alloc;

// Lets us use our own derive macros internally.
extern crate self as evenio;

pub mod access;
mod aliased_box;
pub mod archetype;
mod assert;
mod bit_set;
mod blob_vec;
pub mod component;
pub mod drop;
pub mod entity;
pub mod event;
pub mod fetch;
pub mod handler;
mod layout_util;
mod map;
pub mod query;
mod slot_map;
mod sparse;
mod sparse_map;
#[cfg(doc)]
pub mod tutorial;
pub mod world;

#[cfg(feature = "rayon")]
#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
pub use rayon;

/// Re-exports of the most commonly used items in the library.
///
/// This is intended to be glob-imported like so:
///
/// ```
/// use evenio::prelude::*;
/// ```
pub mod prelude {
    pub use crate::component::{Component, ComponentId};
    pub use crate::entity::EntityId;
    pub use crate::event::{
        Despawn, EventMut, GlobalEvent, GlobalEventId, Insert, Receiver, ReceiverMut, Remove,
        Sender, Spawn, TargetedEvent, TargetedEventId,
    };
    pub use crate::fetch::{Fetcher, GetError, Single, SingleError, TrySingle};
    pub use crate::handler::{Handler, HandlerId, HandlerParam, IntoHandler};
    pub use crate::query::{Has, Not, Or, Query, ReadOnlyQuery, With, Xor};
    pub use crate::world::World;
}

mod private {
    use core::panic::{RefUnwindSafe, UnwindSafe};

    #[derive(Copy, Clone)]
    enum Void {}

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
}
