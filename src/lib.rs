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
    missing_docs,
    unreachable_pub,
    trivial_casts,
    trivial_numeric_casts,
    unused_lifetimes,
    unused_import_braces,
    clippy::dbg_macro,
    clippy::undocumented_unsafe_blocks
)]

mod archetype;
mod component;
mod entity;
mod erased_vec;
mod event;
mod query;
mod system;
mod util;
mod world;

pub use component::*;
pub use entity::*;
pub use event::*;
pub use system::*;
pub use world::*;
