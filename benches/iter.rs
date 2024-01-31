//! Measures performance of Fetcher/Query iteration.

use divan::Bencher;

fn main() {
    divan::main()
}

const ITERS: usize = 1_000_000;

const DATA: f64 = 123456789.0;

#[derive(Copy, Clone, evenio::component::Component, bevy_ecs::component::Component)]
struct Data(f64);

#[divan::bench]
fn iter_evenio(bencher: Bencher) {
    use evenio::prelude::*;

    let mut world = World::new();

    for _ in 0..ITERS {
        let e = world.spawn();
        world.insert(e, Data(DATA));
    }

    #[derive(Event)]
    struct E;

    world.add_system(|_: Receiver<E>, f: Fetcher<&mut Data>| {
        for data in f {
            data.0 = data.0.sqrt();
        }
    });

    bencher.bench_local(|| world.send(E));
}

#[divan::bench]
fn iter_bevy(bencher: Bencher) {
    use bevy_ecs::prelude::*;
    use bevy_ecs::schedule::ScheduleLabel;

    let mut world = World::new();

    world.spawn_batch((0..ITERS).map(|_| Data(DATA)));

    #[derive(ScheduleLabel, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy, Clone)]
    struct Whatever;

    let mut sched = Schedule::new(Whatever);

    sched.add_systems(|mut q: Query<&mut Data>| {
        for mut data in &mut q {
            data.bypass_change_detection().0 = data.0.sqrt();
        }
    });

    bencher.bench_local(|| sched.run(&mut world));
}

// TODO: iteration with many fragmented archetypes.
