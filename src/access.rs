//! Data access checking.

mod archetype_filter;
mod component_access;

pub use archetype_filter::*;
pub use component_access::*;

/// Describes how a particular piece of data is accessed. Used to prevent
/// aliased mutabliity according to Rust's rules.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub enum Access {
    /// Cannot read or write to the data.
    #[default]
    None,
    /// Can read, but not write to the data (shared access).
    Read,
    /// Can both read and write to the data (exclusive access).
    ReadWrite,
}

impl Access {
    /// Produces the access which is the superset of `self` and `other`. If the
    /// two accesses are incompatible with each other, then `None` is returned
    /// instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::access::Access::*;
    ///
    /// assert_eq!(Read.join(Read), Some(Read));
    /// assert_eq!(Read.join(ReadWrite), None);
    /// assert_eq!(ReadWrite.join(None), Some(ReadWrite));
    /// ```
    #[must_use]
    pub const fn join(self, other: Self) -> Option<Access> {
        match (self, other) {
            (Access::None, Access::None) => Some(Access::None),
            (Access::Read | Access::ReadWrite, Access::None) => Some(self),
            (Access::None, Access::Read | Access::ReadWrite) => Some(other),
            (Access::Read, Access::Read) => Some(Access::Read),
            (Access::Read, Access::ReadWrite)
            | (Access::ReadWrite, Access::Read)
            | (Access::ReadWrite, Access::ReadWrite) => None,
        }
    }

    /// Shorthand for `self.join(other).is_some()`. See [`join`] for more
    /// information.
    ///
    /// [`join`]: Self::join
    ///
    /// # Examples
    ///
    /// ```
    /// use evenio::access::Access::*;
    ///
    /// assert!(Read.is_compatible(Read));
    /// assert!(!Read.is_compatible(ReadWrite));
    /// assert!(ReadWrite.is_compatible(None));
    /// ```
    #[must_use]
    pub const fn is_compatible(self, other: Self) -> bool {
        self.join(other).is_some()
    }
}
