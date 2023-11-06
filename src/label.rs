use std::any::{Any, TypeId};
use std::fmt;
use std::hash::{Hash, Hasher};

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

#[cfg(test)]
mod tests {
    use ahash::HashSet;

    use super::*;

    #[test]
    fn label_container() {
        let mut labels = HashSet::<Box<dyn Label>>::default();

        assert!(labels.insert(Box::new("foobar")));
        assert!(labels.insert(Box::new(String::from("foobar"))));

        let key: &dyn Label = &"foobar";

        assert!(labels.contains(key));
    }
}
