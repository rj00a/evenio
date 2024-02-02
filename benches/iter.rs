//! Measures performance of Fetcher/Query iteration.

use divan::Bencher;

fn main() {
    divan::main()
}

const DATA: f64 = 123456789.0;
const ARGS: [usize; 7] = [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000];

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C1(f64);

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C2(#[allow(dead_code)] f64);

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C3(#[allow(dead_code)] f64);

#[derive(evenio::event::Event)]
struct E;

#[divan::bench(args = ARGS)]
fn iter_evenio(bencher: Bencher, len: usize) {
    use evenio::prelude::*;

    let mut world = World::new();

    for i in 0..len {
        let e = world.spawn();

        world.insert(e, C1(42.0));

        if i % 2 == 0 {
            world.insert(e, C2(42.0));
        }

        if i % 3 == 0 {
            world.insert(e, C3(42.0));
        }
    }

    world.add_system(move |_: Receiver<E>, f: Fetcher<&mut C1>| {
        for c in f {
            c.0 = c.0.sqrt();
        }
    });

    bencher.bench_local(|| world.send(E));
}

#[divan::bench(args = ARGS)]
fn iter_bevy(bencher: Bencher, len: usize) {
    use bevy_ecs::prelude::*;

    let mut world = World::new();

    for i in 0..len {
        let mut e = world.spawn(C1(DATA));

        if i % 2 == 0 {
            e.insert(C2(DATA));
        }

        if i % 3 == 0 {
            e.insert(C3(DATA));
        }
    }

    let mut state = world.query::<&mut C1>();

    bencher.bench_local(|| {
        for mut c in state.iter_mut(&mut world) {
            c.bypass_change_detection().0 = c.0.sqrt();
        }
    });
}

// TODO: iteration with high fragmentation.

#[cfg(feature = "rayon")]
#[divan::bench(args = ARGS)]
fn par_iter_evenio(bencher: Bencher, len: usize) {
    use evenio::prelude::*;
    use rayon::prelude::*;

    #[derive(Event)]
    struct E;

    let mut world = World::new();

    for i in 0..len {
        let e = world.spawn();

        world.insert(e, C1(42.0));

        if i % 2 == 0 {
            world.insert(e, C2(42.0));
        }

        if i % 3 == 0 {
            world.insert(e, C3(42.0));
        }
    }

    world.add_system(move |_: Receiver<E>, f: Fetcher<&mut C1>| {
        f.into_par_iter().for_each(|c| {
            c.0 = c.0.sqrt();
        });
    });

    bencher.bench_local(|| world.send(E));
}

#[divan::bench(args = ARGS)]
fn par_iter_bevy(bencher: Bencher, len: usize) {
    use bevy_ecs::prelude::*;
    use bevy_tasks::{ComputeTaskPool, TaskPool};

    ComputeTaskPool::get_or_init(TaskPool::new);

    let mut world = World::new();

    for i in 0..len {
        let mut e = world.spawn(C1(DATA));

        if i % 2 == 0 {
            e.insert(C2(DATA));
        }

        if i % 3 == 0 {
            e.insert(C3(DATA));
        }
    }

    let mut state = world.query::<&mut C1>();

    bencher.bench_local(|| {
        state.par_iter_mut(&mut world).for_each(|mut c| {
            c.bypass_change_detection().0 = c.0.sqrt();
        });
    });
}

// TODO: parallel iteration with varying amounts of work per iteration.
