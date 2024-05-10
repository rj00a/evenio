use core::any::TypeId;

/// Marker type indicating mutability at the type level.
#[derive(Debug)]
pub enum Mutable {}

impl MutabilityMarker for Mutable {}

/// Marker type indicating immutability at the type level.
#[derive(Debug)]
pub enum Immutable {}

impl MutabilityMarker for Immutable {}

/// Sealed marker trait implemented only for [`Mutable`] and [`Immutable`].
pub trait MutabilityMarker: Send + Sync + 'static + mutability::Sealed {}

mod mutability {
    pub trait Sealed {}

    impl Sealed for super::Mutable {}
    impl Sealed for super::Immutable {}
}

/// Enum indicating mutability or immutability at the value level.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Mutability {
    /// The event is mutable.
    Mutable,
    /// The event is immutable.
    Immutable,
}

impl Mutability {
    pub fn of<M: MutabilityMarker>() -> Self {
        if TypeId::of::<M>() == TypeId::of::<Mutable>() {
            Mutability::Mutable
        } else if TypeId::of::<M>() == TypeId::of::<Immutable>() {
            Mutability::Immutable
        } else {
            unreachable!()
        }
    }

    pub const fn is_mutable(self) -> bool {
        matches!(self, Self::Mutable)
    }

    pub const fn is_immutable(self) -> bool {
        matches!(self, Self::Immutable)
    }
}
