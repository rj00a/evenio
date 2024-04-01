//! Data access checking.

mod archetype_filter;
mod component_access;

pub use archetype_filter::*;
pub use component_access::*;

/// Describes how a particular piece of data is accessed. Used to prevent
/// aliased mutabliity.
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
    /// Can `self` and `other` be active at the same time without causing
    /// aliased mutability?
    ///
    /// This operation is symmetric.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use evenio::access::Access::*;
    ///
    /// assert!(Read.is_compatible(Read));
    /// assert!(!Read.is_compatible(ReadWrite));
    /// assert!(ReadWrite.is_compatible(None));
    /// ```
    #[must_use]
    pub const fn is_compatible(self, other: Self) -> bool {
        matches!(
            (self, other),
            (Access::None, _)
                | (Access::Read, Access::None | Access::Read)
                | (Access::ReadWrite, Access::None)
        )
    }
}
