use std::any::{Any, TypeId};
use std::borrow::ToOwned;
use std::collections::HashMap;
use std::fmt;
use std::hash::{BuildHasherDefault, Hash, Hasher};

use ahash::AHasher;
use slab::Slab;

/// Map type with [`TypeId`] keys and `V` values.
pub(crate) type TypeIdMap<V> = HashMap<TypeId, V, BuildHasherDefault<TypeIdHasher>>;

/// A hasher optimized for hashing a single [`TypeId`].
///
/// TypeId is already thoroughly hashed, so there's no reason to hash it again.
/// Just leave the bits unchanged.
#[derive(Default)]
pub(crate) struct TypeIdHasher {
    hash: u64,
}

impl Hasher for TypeIdHasher {
    fn write_u64(&mut self, n: u64) {
        debug_assert_eq!(self.hash, 0, "value was already hashed");

        self.hash = n;
    }

    fn write_u128(&mut self, n: u128) {
        debug_assert_eq!(self.hash, 0, "value was already hashed");

        // Tolerate TypeId being either u64 or u128.
        self.hash = n as u64;
    }

    fn write(&mut self, bytes: &[u8]) {
        debug_assert_eq!(self.hash, 0, "value was already hashed");

        // This will only be called if TypeId is neither u64 nor u128, which is not
        // anticipated. In that case we'll just fall back to using a different
        // hash implementation.
        let mut hasher = AHasher::default();

        hasher.write(bytes);
        self.hash = hasher.finish();
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}

pub(crate) trait GetDebugChecked {
    type Output: ?Sized;

    #[track_caller]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output;
    #[track_caller]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output;
}

impl<T> GetDebugChecked for [T] {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        #[cfg(debug_assertions)]
        return &self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked(idx);
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        #[cfg(debug_assertions)]
        return &mut self[idx];

        #[cfg(not(debug_assertions))]
        return self.get_unchecked_mut(idx);
    }
}

impl<T> GetDebugChecked for Vec<T> {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        self.as_slice().get_debug_checked(idx)
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.as_mut_slice().get_debug_checked_mut(idx)
    }
}

// Don't use `Slab::get_unchecked` because there is a branch for
// panic handling. https://github.com/tokio-rs/slab/pull/74
impl<T> GetDebugChecked for Slab<T> {
    type Output = T;

    #[inline]
    unsafe fn get_debug_checked(&self, idx: usize) -> &Self::Output {
        self.get(idx).expect_debug_checked("invalid slab key")
    }

    #[inline]
    unsafe fn get_debug_checked_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.get_mut(idx).expect_debug_checked("invalid slab key")
    }
}

pub(crate) trait UnwrapDebugChecked {
    type Output;

    #[track_caller]
    unsafe fn unwrap_debug_checked(self) -> Self::Output;
    #[track_caller]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output;
}

impl<T> UnwrapDebugChecked for Option<T> {
    type Output = T;

    #[inline]
    unsafe fn unwrap_debug_checked(self) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.unwrap();

        #[cfg(not(debug_assertions))]
        return self.unwrap_unchecked();
    }

    #[inline]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.expect(msg);

        #[cfg(not(debug_assertions))]
        {
            let _ = msg;
            return self.unwrap_unchecked();
        }
    }
}

impl<T, E> UnwrapDebugChecked for Result<T, E>
where
    E: fmt::Debug,
{
    type Output = T;

    #[inline]
    unsafe fn unwrap_debug_checked(self) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.unwrap();

        #[cfg(not(debug_assertions))]
        return self.unwrap_unchecked();
    }

    #[inline]
    unsafe fn expect_debug_checked(self, msg: &str) -> Self::Output {
        #[cfg(debug_assertions)]
        return self.expect(msg);

        #[cfg(not(debug_assertions))]
        {
            let _ = msg;
            return self.unwrap_unchecked();
        }
    }
}

#[cold]
pub(crate) fn capacity_overflow() -> ! {
    panic!("capacity overflow")
}

pub trait Label: Send + Sync + Any + fmt::Debug + private::Sealed {
    fn dyn_clone(&self) -> Box<dyn Label>;
    fn dyn_eq(&self, other: &dyn Label) -> bool;
    fn dyn_hash(&self, hasher: &mut dyn Hasher);
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl PartialEq for dyn Label {
    fn eq(&self, other: &Self) -> bool {
        self.dyn_eq(other)
    }
}

impl Eq for dyn Label {}

impl Hash for dyn Label {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state)
    }
}

impl Clone for Box<dyn Label> {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

impl ToOwned for dyn Label {
    type Owned = Box<dyn Label>;

    fn to_owned(&self) -> Self::Owned {
        self.dyn_clone()
    }
}

impl<T> Label for T
where
    T: Clone + Eq + Hash + Send + Sync + Any + fmt::Debug,
{
    fn dyn_clone(&self) -> Box<dyn Label> {
        Box::new(self.clone())
    }

    fn dyn_eq(&self, other: &dyn Label) -> bool {
        other
            .as_any()
            .downcast_ref::<T>()
            .map_or(false, |other| self == other)
    }

    fn dyn_hash(&self, mut hasher: &mut dyn Hasher) {
        Hash::hash(&TypeId::of::<Self>(), &mut hasher);
        Hash::hash(self, &mut hasher)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

mod private {
    use super::*;

    pub trait Sealed {}
    impl<T> Sealed for T where T: Clone + Eq + Hash + Send + Sync + Any + fmt::Debug {}
}
