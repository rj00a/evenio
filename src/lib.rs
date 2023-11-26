#![doc = include_str!("../README.md")]

const _: () = if std::mem::size_of::<usize>() <= 2 {
    panic!("unsupported target")
};

// Lets us use our own derive macros internally.
extern crate self as evenio;

mod access;
pub mod archetype;
mod bit_set;
pub mod command;
pub mod component;
mod debug_checked;
pub mod entity;
mod erased_vec;
pub mod event;
#[doc(hidden)]
pub mod exclusive;
pub mod label;
pub mod query;
pub mod system;
#[cfg(test)]
mod tests;
mod type_id_hash;
pub mod world;

pub mod prelude {
    pub use crate::component::Component;
    pub use crate::entity::EntityId;
    pub use crate::event::{Despawn, Event, EventSet, Insert, SendTo, Sender, Spawn, Take};
    pub use crate::query::{Has, Not, Or, ReadOnlyWorldQuery, With, WorldQuery, Xor};
    pub use crate::world::World;
}
