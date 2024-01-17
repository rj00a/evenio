//! Library tutorial book
//!
//! This is a short "getting started" tutorial which explains the main concepts
//! of the library. A basic understanding of Rust is assumed.
//!
//! Each chapter of the book is in a separate module. To begin, start with
//! [chapter 1](ch01_systems_and_events).
//!
//! _Note: this module is intended for documentation purposes only. The contents
//! of this module should be considered unstable and may change in minor version
//! updates._

#[doc = include_str!("../tutorial/ch01_systems_and_events.md")]
pub mod ch01_systems_and_events {}

#[doc = include_str!("../tutorial/ch02_event_mutation.md")]
pub mod ch02_event_mutation {}

#[doc = include_str!("../tutorial/ch03_sending_events_from_systems.md")]
pub mod ch03_sending_events_from_systems {}

#[doc = include_str!("../tutorial/ch04_entities_and_components.md")]
pub mod ch04_entities_and_components {}

#[doc = include_str!("../tutorial/ch05_fetching.md")]
pub mod ch05_fetching {}

#[doc = include_str!("../tutorial/ch06_targeted_events.md")]
pub mod ch06_targeted_events {}

#[doc = include_str!("../tutorial/ch07_enforcing_invariants.md")]
pub mod ch07_enforcing_invariants {}
