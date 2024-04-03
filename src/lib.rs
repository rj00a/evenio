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
pub mod exclusive;
pub mod fetch;
pub mod handler;
mod layout_util;
mod map;
pub mod query;
mod slot_map;
pub mod sparse;
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
        Despawn, Event, EventId, EventMut, Insert, Receiver, ReceiverMut, Remove, Sender, Spawn,
    };
    pub use crate::fetch::{Fetcher, FetchError, Single, SingleError, TrySingle};
    pub use crate::handler::{Handler, HandlerId, HandlerParam, IntoHandler};
    pub use crate::query::{Has, Not, Or, Query, ReadOnlyQuery, With, Xor};
    pub use crate::world::World;
}
