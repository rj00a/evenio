//! Compares performance of hashsets and btreesets in relevant scenarios.

use std::collections::BTreeSet;

use ahash::AHashSet;
use divan::{black_box, Bencher};

fn main() {
    divan::main()
}

const LENS: [u32; 6] = [1, 5, 10, 50, 100, 1000];

#[divan::bench(args = LENS)]
fn insert_u32_hashset(bencher: Bencher, len: u32) {
    let mut set = AHashSet::<u32>::new();
    set.reserve(len as usize);

    bencher.bench_local(|| {
        for n in 0..len {
            set.insert(black_box(n * 1000));
        }

        set.clear();
    });
}

#[divan::bench(args = LENS)]
fn insert_u32_btreeset(bencher: Bencher, len: u32) {
    let mut set = BTreeSet::<u32>::new();

    bencher.bench_local(|| {
        for n in 0..len {
            set.insert(black_box(n * 1000));
        }

        set.clear();
    });
}

#[divan::bench(args = LENS)]
fn lookup_u128_hashset(bencher: Bencher, len: u32) {
    let set: AHashSet<u128> = (0..len).map(|n| n as u128 * 10000).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(123456789)));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u128_btreeset(bencher: Bencher, len: u32) {
    let set: BTreeSet<u128> = (0..len).map(|n| n as u128 * 10000).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(123456789)));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u128_linear_search(bencher: Bencher, len: u32) {
    let set: Vec<u128> = (0..len).map(|n| n as u128 * 10000).collect();

    bencher.bench(|| {
        let n = black_box(123456789);
        black_box(set.iter().find(|&&v| v == n));
    });
}

#[divan::bench(args = LENS)]
fn lookup_128_binary_search(bencher: Bencher, len: u32) {
    let set: Vec<u128> = (0..len).map(|n| n as u128 * 10000).collect();

    bencher.bench(|| {
        let n = black_box(123456789);
        let _ = black_box(set.binary_search(&n));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u32_hashset(bencher: Bencher, len: u32) {
    let set: AHashSet<u32> = (0..len).map(|n| n * 10000).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(123456789)));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u32_btreeset(bencher: Bencher, len: u32) {
    let set: BTreeSet<u32> = (0..len).map(|n| n * 10000).collect();

    bencher.bench(|| {
        black_box(set.get(&black_box(123456789)));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u32_linear_search(bencher: Bencher, len: u32) {
    let set: Vec<u32> = (0..len).map(|n| n * 10000).collect();

    bencher.bench(|| {
        let n = black_box(123456789);
        black_box(set.iter().find(|&&v| v == n));
    });
}

#[divan::bench(args = LENS)]
fn lookup_u32_binary_search(bencher: Bencher, len: u32) {
    let set: Vec<u32> = (0..len).map(|n| n * 10000).collect();

    bencher.bench(|| {
        let n = black_box(123456789);
        let _ = black_box(set.binary_search(&n));
    });
}
