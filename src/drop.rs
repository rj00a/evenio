//! [`Drop`] utilities.

use core::mem::needs_drop;
use core::ptr::{drop_in_place, NonNull};

/// Drop function for some erased (and sized) type.
///
/// In order to be safe to call, the input pointer must be correctly aligned and
/// must point to an initialized value of the correct type. The pointed-to
/// memory should be considered uninitialized after the call.
///
/// If the function pointer is `None`, then the value is considered trivially
/// droppable and no destructor needs to run.
pub type DropFn = Option<unsafe fn(NonNull<u8>)>;

/// Returns the appropriate [`DropFn`] for some known type `T`.
pub const fn drop_fn_of<T>() -> DropFn {
    if needs_drop::<T>() {
        Some(|ptr| unsafe { drop_in_place(ptr.cast::<T>().as_ptr()) })
    } else {
        None
    }
}
