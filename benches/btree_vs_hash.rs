//! Compares performance of hashsets, btreesets, and other search methods in
//! relevant scenarios.

use std::collections::BTreeSet;
use std::hash::Hash;

use ahash::AHashSet;
use divan::{black_box, Bencher};

fn main() {
    divan::main()
}

const LENS: [u32; 6] = [1, 5, 10, 50, 100, 1000];

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn insert_hashset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Hash + Eq,
{
    let mut set = AHashSet::<T>::new();
    set.reserve(len as usize);

    bencher.bench_local(|| {
        for n in 0..len {
            set.insert(black_box(T::from(n * 1000)));
        }

        set.clear();
    });
}

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn insert_btreeset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Ord,
{
    let mut set = BTreeSet::<T>::new();

    bencher.bench_local(|| {
        for n in 0..len {
            set.insert(black_box(T::from(n * 1000)));
        }

        set.clear();
    });
}

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn lookup_hashset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Hash + Eq + Sync,
{
    let set: AHashSet<T> = (0..len).map(|n| T::from(n * 1000)).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(T::from(123456789))));
    });
}

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn lookup_btreeset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Ord + Sync,
{
    let set: BTreeSet<T> = (0..len).map(|n| T::from(n * 1000)).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(T::from(123456789))));
    });
}

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn lookup_linear_search<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Eq + Sync,
{
    let set: Vec<T> = (0..len).map(|n| T::from(n * 1000)).collect();

    bencher.bench(|| {
        let n = black_box(T::from(123456789));
        black_box(set.iter().find(|&v| *v == n));
    });
}

#[divan::bench(args = LENS, types = [u32, u64, u128])]
fn lookup_binary_search<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Ord + Sync,
{
    let set: Vec<T> = (0..len).map(|n| T::from(n * 1000)).collect();

    bencher.bench(|| {
        let n = black_box(T::from(123456789));
        let _ = black_box(set.binary_search(&n));
    });
}
