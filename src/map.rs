//! Contains the general-purpose map type [`Map`] and specialized map for
//! [`TypeId`] keys [`TypeIdMap`].
//!
//! These maps are hash maps when the `std` feature is enabled and fall back to
//! `BTreeMap` otherwise.

use core::any::TypeId;

#[cfg(feature = "std")]
pub(crate) type Map<K, V> = ahash::AHashMap<K, V>;

#[cfg(not(feature = "std"))]
pub(crate) type Map<K, V> = alloc::collections::BTreeMap<K, V>;

#[cfg(feature = "std")]
pub(crate) type Entry<'a, K, V> = std::collections::hash_map::Entry<'a, K, V>;

#[cfg(not(feature = "std"))]
pub(crate) type Entry<'a, K, V> = alloc::collections::btree_map::Entry<'a, K, V>;

#[cfg(feature = "std")]
pub(crate) type TypeIdMap<V> =
    std::collections::HashMap<TypeId, V, core::hash::BuildHasherDefault<TypeIdHasher>>;

#[cfg(not(feature = "std"))]
pub(crate) type TypeIdMap<V> = alloc::collections::BTreeMap<TypeId, V>;

/// A hasher optimized for hashing a single [`TypeId`].
///
/// `TypeId` is already thoroughly hashed, so there's no reason to hash it
/// again. Just leave the bits unchanged.
#[cfg(feature = "std")]
#[derive(Default)]
pub(crate) struct TypeIdHasher {
    hash: u64,
}

#[cfg(feature = "std")]
impl core::hash::Hasher for TypeIdHasher {
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
        let mut hasher = ahash::AHasher::default();

        hasher.write(bytes);
        self.hash = hasher.finish();
    }

    fn finish(&self) -> u64 {
        self.hash
    }
}
