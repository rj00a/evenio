#![doc = include_str!("../README.md")]

extern crate alloc;

// Lets us use our own derive macros internally.
extern crate self as evenio;

pub mod access;
pub mod archetype;
pub mod bit_set;
mod blob_vec;
pub mod bool_expr;
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
pub mod sparse;
mod sparse_map;
pub mod system;
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
        AddComponent, AddEvent, AddSystem, Call, Despawn, Event, EventId, EventMut, Insert,
        Receiver, Remove, Sender, Spawn,
    };
    pub use crate::fetch::{Fetcher, GetError, Single, SingleError, TrySingle};
    pub use crate::query::{Has, Not, Or, Query, ReadOnlyQuery, With, Xor};
    pub use crate::system::{IntoSystem, SystemId};
    pub use crate::world::World;
}

const _: () = assert!(
    core::mem::size_of::<usize>() >= core::mem::size_of::<u32>(),
    "unsupported target"
);

/// Drop function for some erased (and sized) type.
///
/// In order to be safe to call, the input pointer must be correctly aligned and
/// point to an initialized value of the erased type. The pointed-to memory
/// should be considered uninitialized after the call.
///
/// If the function pointer is `None`, then the erased type is considered
/// trivially droppable and no destructor needs to run.
type DropFn = Option<unsafe fn(core::ptr::NonNull<u8>)>;

const fn drop_fn_of<T>() -> DropFn {
    use core::mem::needs_drop;
    use core::ptr::drop_in_place;

    if needs_drop::<T>() {
        Some(|ptr| unsafe { drop_in_place(ptr.cast::<T>().as_ptr()) })
    } else {
        None
    }
}
