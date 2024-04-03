//! Accessing components on entities.

mod iter;
mod single;
mod fetcher;
#[cfg(feature = "rayon")]
mod par_iter;

pub use iter::*;
pub use single::*;
pub use fetcher::*;
#[cfg(feature = "rayon")]
#[cfg_attr(docsrs, doc(cfg(feature = "rayon")))]
pub use par_iter::*;

#[cfg(test)]
mod tests {
    use alloc::collections::BTreeSet;

    use crate::prelude::*;

    #[derive(Event)]
    struct E1;

    #[derive(Event)]
    struct E2;

    #[derive(Event)]
    struct E3;

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C1(u32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C2(u32);

    #[derive(Component, PartialEq, Eq, Debug)]
    struct C3(u32);

    #[test]
    fn random_access() {
        let mut world = World::new();

        let e = world.spawn();
        let e2 = world.spawn();
        let e3 = world.spawn();
        world.spawn();

        world.insert(e, C1(123));
        world.insert(e2, C1(456));
        world.insert(e3, C2(789));

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            assert_eq!(f.get(e), Ok(&C1(123)));
        });

        world.add_handler(move |_: Receiver<E2>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e3), Ok(&C2(789)))
        });

        world.send(E1);

        world.add_handler(|_: Receiver<E2>, f: Fetcher<&C1>| {
            assert_eq!(f.get(EntityId::NULL), Err(FetchError::NoSuchEntity));
        });

        world.send(E2);

        world.add_handler(move |_: Receiver<E3>, f: Fetcher<&C2>| {
            assert_eq!(f.get(e), Err(FetchError::QueryDoesNotMatch))
        });

        world.send(E3);
    }

    #[test]
    fn iter() {
        let mut world = World::new();

        let mut set = BTreeSet::new();

        for i in 0..20_u32 {
            let e = world.spawn();

            world.insert(e, C1(i.pow(2)));

            if i % 2 == 0 {
                world.insert(e, C2(i.pow(2)));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i.pow(2)));
            }

            set.insert(i.pow(2));
        }

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for c in f {
                assert!(set.remove(&c.0));
            }

            assert!(set.is_empty());
        });

        world.send(E1);
    }

    #[cfg(feature = "rayon")]
    #[test]
    fn par_iter() {
        use rayon::prelude::*;

        let mut world = World::new();

        const N: u32 = 20;

        for i in 0..N {
            let e = world.spawn();

            world.insert(e, C1(i));

            if i % 2 == 0 {
                world.insert(e, C2(i));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i));
            }
        }

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            let sum = f.par_iter().map(|c| c.0).sum::<u32>();
            assert_eq!(sum, N * (N - 1) / 2);
        });

        world.send(E1);
    }

    #[test]
    fn iter_empty() {
        let mut world = World::new();

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<&C1>| {
            for c in f {
                println!("{c:?}");
            }
        });

        world.send(E1);
    }

    #[test]
    fn iter_previously_nonempty() {
        let mut world = World::new();

        world.add_handler(move |_: Receiver<E1>, f: Fetcher<EntityId>| {
            for id in f {
                println!("{id:?}");
            }
        });

        let e = world.spawn();
        world.send(E1);
        world.despawn(e);
        world.send(E1);
    }

    #[test]
    fn iter_len() {
        let mut world = World::new();

        let count = 20;

        for i in 1..=count {
            let e = world.spawn();
            world.insert(e, C1(i));

            if i % 2 == 0 {
                world.insert(e, C2(i));
            }

            if i % 3 == 0 {
                world.insert(e, C3(i));
            }
        }

        world.add_handler(
            move |_: Receiver<E1>, f1: Fetcher<&C1>, f2: Fetcher<&C2>, f3: Fetcher<&C3>| {
                assert_eq!(f1.iter().len(), count as usize);
                assert_eq!(f2.iter().len(), count as usize / 2);
                assert_eq!(f3.iter().len(), count as usize / 3);
            },
        );

        world.send(E1);
    }

    #[test]
    fn single_param() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C1(123));
        }

        world.add_handler(|_: Receiver<E1>, Single(&C1(n)): Single<&C1>| {
            assert_eq!(n, 123);
        });

        world.send(E1);
    }

    #[test]
    #[should_panic]
    fn single_param_panics_on_zero() {
        let mut world = World::new();

        world.add_handler(|_: Receiver<E1>, _: Single<&C1>| {});

        world.send(E1);
    }

    #[test]
    #[should_panic]
    fn single_param_panics_on_many() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C1(123));
            let e = world.spawn();
            world.insert(e, C1(456));
        }

        world.add_handler(|_: Receiver<E1>, _: Single<&C1>| {});

        world.send(E1);
    }

    #[test]
    fn try_single_param() {
        let mut world = World::new();

        {
            let e = world.spawn();
            world.insert(e, C2(123));

            let e = world.spawn();
            world.insert(e, C3(123));
            let e = world.spawn();
            world.insert(e, C3(456));
        }

        world.add_handler(
            |_: Receiver<E1>, s1: TrySingle<&C1>, s2: TrySingle<&C2>, s3: TrySingle<&C3>| {
                assert_eq!(s1.0, Err(SingleError::QueryDoesNotMatch));
                assert_eq!(s2.0, Ok(&C2(123)));
                assert_eq!(s3.0, Err(SingleError::MoreThanOneMatch));
            },
        );

        world.send(E1);
    }

    fn _assert_auto_trait_impls()
    where
        for<'a> Fetcher<'a, ()>: Send + Sync,
        for<'a, 'b> Fetcher<'a, &'b C1>: Send + Sync,
        for<'a, 'b, 'c> Fetcher<'a, (&'b C1, &'c mut C2)>: Send + Sync,
    {
    }
}
