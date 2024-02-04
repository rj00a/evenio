use core::any::TypeId;
use core::hash::{BuildHasherDefault, Hasher};

use ahash::AHasher;

#[cfg(feature = "std")]
pub(crate) type HashMap<K, V, S = ahash::RandomState> = std::collections::HashMap<K, V, S>;

#[cfg(not(feature = "std"))]
pub(crate) type HashMap<K, V, S = ahash::RandomState> = hashbrown::HashMap<K, V, S>;

#[cfg(feature = "std")]
pub(crate) type Entry<'a, K, V> = std::collections::hash_map::Entry<'a, K, V>;

#[cfg(not(feature = "std"))]
pub(crate) type Entry<'a, K, V, S = ahash::RandomState> = hashbrown::hash_map::Entry<'a, K, V, S>;

/// Map type optimized for [`TypeId`] keys.
pub(crate) type TypeIdMap<V> = HashMap<TypeId, V, BuildHasherDefault<TypeIdHasher>>;

/// A hasher optimized for hashing a single [`TypeId`].
///
/// `TypeId` is already thoroughly hashed, so there's no reason to hash it
/// again. Just leave the bits unchanged.
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
