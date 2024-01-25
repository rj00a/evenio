//! [`Drop`] utilities.

use core::mem::needs_drop;
use core::ptr::{drop_in_place, NonNull};

/// Drop function for some data. The data may not necessarily have a type in
/// Rust's type system.
///
/// The function pointer takes a pointer to some data as input and performs an
/// in-place drop on the data. In order to be safe to call, the input pointer
/// must be correctly aligned and must point to an initialized value of the
/// correct type. The memory should be considered uninitialized after the call.
///
/// If the function pointer is `None`, then the data is considered trivially
/// droppable and no destructor needs to run.
pub type DropFn = Option<unsafe fn(NonNull<u8>)>;

/// Returns the appropriate [`DropFn`] for some [`Sized`] Rust type `T`.
pub const fn drop_fn_of<T>() -> DropFn {
    if needs_drop::<T>() {
        Some(|ptr| unsafe { drop_in_place(ptr.cast::<T>().as_ptr()) })
    } else {
        None
    }
}
