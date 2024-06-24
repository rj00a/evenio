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
mod bit_set;
pub mod component;
pub mod drop;
pub mod entity;
pub mod event;
pub mod fetch;
pub mod handler;
mod ignore;
mod map;
pub mod mutability;
pub mod query;
mod slot_map;
mod sparse;
mod sparse_map;
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

const _: () = assert!(
    core::mem::size_of::<usize>() >= core::mem::size_of::<u32>(),
    "unsupported target"
);

#[inline]
#[track_caller]
unsafe fn assume_unchecked(cond: bool) {
    if !cond {
        core::hint::unreachable_unchecked()
    }
}
