//! Library guide book
//!
//! This is a short "getting started" guide to explain the main concepts of the
//! library. Each chapter of the book is in a separate module. A
//! basic understanding of Rust is assumed. To begin, start with [chapter
//! 1](ch01_systems_and_events).
//!
//! _Note: this module is intended for documentation purposes only. The contents
//! of this module should be considered unstable and may change in minor version
//! updates._

#![cfg_attr(
    not(doc),
    deprecated = "This module is intended for documentation purposes only."
)]

#[doc = include_str!("../guide/ch01_systems_and_events.md")]
pub mod ch01_systems_and_events {}

#[doc = include_str!("../guide/ch02_event_mutation.md")]
pub mod ch02_event_mutation {}

#[doc = include_str!("../guide/ch03_sending_events_from_systems.md")]
pub mod ch03_sending_events_from_systems {}

#[doc = include_str!("../guide/ch04_entities_and_components.md")]
pub mod ch04_entities_and_components {}
