use core::any::{Any, TypeId};
use core::cmp::Ordering;
use core::fmt;

/// Abstract identifiers.
///
/// Labels are useful for identification purposes (keys in maps) while
/// abstracting away the particular underlying type. Labels exist primarily as
/// trait objects (`dyn Label`).
///
/// This trait is automatically implemented for all types which implement the
/// necessary traits, and cannot be implemented manually. For instance,
/// [`TypeId`], [`String`], and [`u64`] are all valid labels.
pub trait Label: Send + Sync + Any + fmt::Debug + private::Sealed {
    /// Clone the underlying type into a box and return the result.
    fn dyn_clone(&self) -> Box<dyn Label>;
    /// Compare two labels, possibly with different underlying types.
    ///
    /// If the underlying types are the same, then the type's [`Ord`]
    /// implementation is used. Otherwise, the underlying types' [`TypeId`] is
    /// compared.
    ///
    /// This is used in the [`Ord`] implementation of `dyn Label`.
    fn dyn_cmp(&self, other: &dyn Label) -> Ordering;
    /// Returns the underlying type as an [`Any`].
    fn as_any(&self) -> &dyn Any;
    /// Returns the underlying type as a mutable [`Any`].
    fn as_any_mut(&mut self) -> &mut dyn Any;
    /// Returns the underlying type as an [`Any`].
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

impl Clone for Box<dyn Label> {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

impl PartialEq for dyn Label {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for dyn Label {}

impl PartialOrd for dyn Label {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for dyn Label {
    fn cmp(&self, other: &Self) -> Ordering {
        self.dyn_cmp(other)
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
    T: Clone + Ord + Send + Sync + Any + fmt::Debug,
{
    fn dyn_clone(&self) -> Box<dyn Label> {
        Box::new(self.clone())
    }

    fn dyn_cmp(&self, other: &dyn Label) -> Ordering {
        let other_any = other.as_any();

        match other_any.downcast_ref::<T>() {
            Some(other) => self.cmp(other),
            None => other_any.type_id().cmp(&TypeId::of::<Self>()),
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

mod private {
    use super::*;

    pub trait Sealed {}
    impl<T> Sealed for T where T: Clone + Ord + Send + Sync + Any + fmt::Debug {}
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    #[test]
    fn label_container() {
        let mut labels = BTreeSet::<Box<dyn Label>>::new();

        labels.insert(Box::new("foobar"));
        labels.insert(Box::new(String::from("foobar")));
        labels.insert(Box::new("foobar"));

        assert_eq!(labels.len(), 2);

        let key: &dyn Label = &"foobar";
        assert!(labels.contains(key));
    }
}
