#![doc = include_str!("../README.md")]

extern crate alloc;

// Lets us use our own derive macros internally.
extern crate self as evenio;

pub mod access;
pub mod archetype;
pub mod bit_set;
mod blob_vec;
pub mod component;
mod debug_checked;
pub mod entity;
pub mod event;
#[doc(hidden)]
pub mod exclusive;
pub mod fetch;
mod layout_util;
pub mod query;
mod slot_map;
pub mod system;
pub mod world;

pub mod prelude {
    pub use crate::component::{Component, ComponentId};
    pub use crate::entity::EntityId;
    pub use crate::event::{Event, EventMut, EventId, Receiver, Sender};
    pub use crate::fetch::{FetchError, Fetcher};
    pub use crate::query::{Has, Not, Or, Query, ReadOnlyQuery, With, Xor};
    pub use crate::system::{IntoSystem, SystemId};
    pub use crate::world::World;
}

const _: () = if std::mem::size_of::<usize>() < std::mem::size_of::<u32>() {
    panic!("unsupported target")
};
