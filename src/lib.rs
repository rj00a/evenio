#![doc = include_str!("../README.md")]
#![cfg_attr(not(any(feature = "std", test)), no_std)]
#![warn(clippy::std_instead_of_alloc, clippy::std_instead_of_core)]
#![allow(clippy::len_without_is_empty, clippy::let_unit_value)]

extern crate alloc;

// Lets us use our own derive macros internally.
extern crate self as evenio;

pub mod access;
pub mod archetype;
mod assert;
pub mod bit_set;
mod blob_vec;
pub mod bool_expr;
pub mod component;
pub mod drop;
pub mod entity;
pub mod event;
#[doc(hidden)]
pub mod exclusive;
pub mod fetch;
mod layout_util;
pub mod query;
mod slot_map;
pub mod sparse;
mod sparse_map;
pub mod system;
#[cfg(doc)]
pub mod tutorial;
pub mod world;

/// For macros only.
#[doc(hidden)]
pub mod __private {
    pub use memoffset::offset_of;
}

pub mod prelude {
    pub use crate::component::{Component, ComponentId};
    pub use crate::entity::EntityId;
    pub use crate::event::{
        Despawn, Event, EventId, EventMut, Insert, Receiver, ReceiverMut, Remove, Sender, Spawn,
    };
    pub use crate::fetch::{Fetcher, GetError, Single, SingleError, TrySingle};
    pub use crate::query::{Has, Not, Or, Query, ReadOnlyQuery, With, Xor};
    pub use crate::system::{IntoSystem, SystemId};
    pub use crate::world::World;
}
