//! Library tutorial book
//!
//! This is a short "getting started" tutorial which explains the main concepts
//! of the library. A basic understanding of Rust is assumed.
//!
//! Each chapter of the book is in a separate module. To begin, start with
//! [chapter 1](ch01_handlers_and_events).
//!
//! _Note: This module's contents are not stable_

#[doc = include_str!("../tutorial/ch01_handlers_and_events.md")]
pub mod ch01_handlers_and_events {}

#[doc = include_str!("../tutorial/ch02_event_mutation.md")]
pub mod ch02_event_mutation {}

#[doc = include_str!("../tutorial/ch03_sending_events_from_handlers.md")]
pub mod ch03_sending_events_from_handlers {}

#[doc = include_str!("../tutorial/ch04_entities_and_components.md")]
pub mod ch04_entities_and_components {}

#[doc = include_str!("../tutorial/ch05_fetching.md")]
pub mod ch05_fetching {}

#[doc = include_str!("../tutorial/ch06_targeted_events.md")]
pub mod ch06_targeted_events {}

#[doc = include_str!("../tutorial/ch07_enforcing_invariants.md")]
pub mod ch07_enforcing_invariants {}
