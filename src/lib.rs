//! TODO
#![deny(
    rustdoc::broken_intra_doc_links,
    rustdoc::private_intra_doc_links,
    rustdoc::missing_crate_level_docs,
    rustdoc::invalid_codeblock_attributes,
    rustdoc::invalid_rust_codeblocks,
    rustdoc::bare_urls,
    rustdoc::invalid_html_tags
)]
#![warn(
    missing_debug_implementations,
    // missing_docs, // TODO
    unreachable_pub,
    // trivial_numeric_casts,
    unused_lifetimes,
    unused_import_braces,
    rust_2018_idioms,
    nonstandard_style,
    future_incompatible,
    clippy::dbg_macro,
    clippy::undocumented_unsafe_blocks,
    clippy::mod_module_files,
)]
// Warned by `future_incompatible`.
#![allow(elided_lifetimes_in_paths)]

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
mod util;
pub mod world;

pub mod prelude {
    pub use crate::component::{Component, ComponentSet};
    pub use crate::entity::EntityId;
    pub use crate::event::{Despawn, Event, EventSet, Insert, Sender, Spawn, Take};
    pub use crate::world::World;
}
