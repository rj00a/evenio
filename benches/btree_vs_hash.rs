//! Compares performance of hashsets, btreesets, and other search methods in
//! relevant scenarios.

use std::collections::BTreeSet;
use std::hash::Hash;

use ahash::AHashSet;
use divan::{black_box, Bencher};
use indexmap::IndexSet;

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
fn insert_indexset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Hash + Eq,
{
    let mut set = IndexSet::with_hasher(ahash::RandomState::new());
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
fn lookup_indexset<T>(bencher: Bencher, len: u32)
where
    T: From<u32> + Hash + Eq + Sync,
{
    let set: IndexSet<T, ahash::RandomState> = (0..len).map(|n| T::from(n * 1000)).collect();

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

const ARRAY_LENS: [usize; 10] = [1, 2, 3, 4, 5, 10, 20, 30, 40, 50];
const ARRAY_COUNT: usize = 100;

#[divan::bench(args = ARRAY_LENS)]
fn lookup_u32_array_hashset(bencher: Bencher, array_len: usize) {
    let set: AHashSet<Box<[u32]>> = (0..ARRAY_COUNT)
        .map(|n| vec![n as u32; n % array_len + 1].into_boxed_slice())
        .chain([[].into()])
        .collect();

    let val = vec![12345; array_len].into_boxed_slice();

    bencher.bench(|| {
        black_box(set.get(black_box(&*val)));
    });
}

#[divan::bench(args = ARRAY_LENS)]
fn lookup_u32_array_indexset(bencher: Bencher, array_len: usize) {
    let set: IndexSet<Box<[u32]>, ahash::RandomState> = (0..ARRAY_COUNT)
        .map(|n| vec![n as u32; n % array_len + 1].into_boxed_slice())
        .chain([[].into()])
        .collect();

    let val = vec![12345; array_len].into_boxed_slice();

    bencher.bench(|| {
        black_box(set.get(black_box(&*val)));
    });
}

#[divan::bench(args = ARRAY_LENS)]
fn lookup_u32_array_btreeset(bencher: Bencher, array_len: usize) {
    let set: BTreeSet<Box<[u32]>> = (0..ARRAY_COUNT)
        .map(|n| vec![n as u32; n % array_len + 1].into_boxed_slice())
        .chain([[].into()])
        .collect();

    let val = vec![12345; array_len].into_boxed_slice();

    bencher.bench(|| {
        black_box(set.get(black_box(&*val)));
    });
}
