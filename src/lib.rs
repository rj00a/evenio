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
    trivial_casts,
    trivial_numeric_casts,
    unused_lifetimes,
    unused_import_braces,
    rust_2018_idioms,
    nonstandard_style,
    future_incompatible,
    clippy::dbg_macro,
    clippy::undocumented_unsafe_blocks
)]
// Warned by `future_incompatible`.
#![allow(elided_lifetimes_in_paths)]

// Lets us use our own proc macros internally.
#[cfg(test)]
extern crate self as evenio;

pub mod archetype;
pub mod component;
pub mod entity;
mod erased_vec;
pub mod event;
#[doc(hidden)]
pub mod exclusive;
pub mod query;
pub mod system;
#[cfg(test)]
mod tests;
mod util;
pub mod world;

pub mod prelude {
    pub use crate::component::Component;
    pub use crate::entity::EntityId;
    pub use crate::event::{Event, Sender, Take};
    pub use crate::world::World;
}
