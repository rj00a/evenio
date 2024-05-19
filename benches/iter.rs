//! Measures performance of Fetcher/Query iteration.

use divan::Bencher;

fn main() {
    divan::main()
}

const DATA: f64 = 123456789.0;
const LENS: [usize; 7] = [1, 10, 100, 1_000, 10_000, 100_000, 1_000_000];

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C1(f64);

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C2(#[allow(dead_code)] f64);

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct C3(#[allow(dead_code)] f64);

#[derive(evenio::event::GlobalEvent)]
struct E;

#[cfg(feature = "rayon")]
#[divan::bench(args = LENS, consts = [false, true])]
fn iter_simple_evenio<const PARALLEL: bool>(bencher: Bencher, len: usize) {
    use evenio::prelude::*;
    use evenio::rayon::prelude::*;

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

    world.add_handler(move |_: Receiver<E>, f: Fetcher<&mut C1>| {
        if PARALLEL {
            f.into_par_iter().for_each(|c| {
                c.0 = c.0.sqrt();
            });
        } else {
            f.into_iter().for_each(|c| {
                c.0 = c.0.sqrt();
            });
        }
    });

    bencher.bench_local(|| world.send(E));
}

#[divan::bench(args = LENS, consts = [false, true])]
fn iter_simple_bevy<const PARALLEL: bool>(bencher: Bencher, len: usize) {
    use bevy_ecs::prelude::*;
    use bevy_tasks::{ComputeTaskPool, TaskPool};

    if PARALLEL {
        ComputeTaskPool::get_or_init(TaskPool::new);
    }

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
        if PARALLEL {
            state.par_iter_mut(&mut world).for_each(|mut c| {
                c.bypass_change_detection().0 = c.0.sqrt();
            });
        } else {
            state.iter_mut(&mut world).for_each(|mut c| {
                c.bypass_change_detection().0 = c.0.sqrt();
            });
        }
    });
}

const ITERS_PER_PIXEL: [usize; 11] = [1, 2, 3, 8, 16, 32, 64, 128, 256, 512, 1024];
const IMAGE_SIZE: u16 = 100;

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct Pixel(u16, u16);

#[derive(evenio::component::Component, bevy_ecs::component::Component)]
struct Output(bool);

#[cfg(feature = "rayon")]
#[divan::bench(args = ITERS_PER_PIXEL, consts = [true])]
fn iter_mandelbrot_evenio<const PARALLEL: bool>(bencher: Bencher, iters: usize) {
    use evenio::prelude::*;
    use evenio::rayon::prelude::*;

    let mut world = World::new();

    for y in 0..IMAGE_SIZE {
        for x in 0..IMAGE_SIZE {
            let e = world.spawn();
            world.insert(e, Pixel(x, y));
            world.insert(e, Output(false));
        }
    }

    world.add_handler(move |_: Receiver<E>, f: Fetcher<(&Pixel, &mut Output)>| {
        if PARALLEL {
            f.into_par_iter().for_each(|(&Pixel(x, y), out)| {
                out.0 = mandelbrot(x, y, iters);
            });
        } else {
            f.into_iter().for_each(|(&Pixel(x, y), out)| {
                out.0 = mandelbrot(x, y, iters);
            });
        }
    });

    bencher.bench_local(|| {
        world.send(E);
    });
}

#[divan::bench(args = ITERS_PER_PIXEL, consts = [true])]
fn iter_mandelbrot_bevy<const PARALLEL: bool>(bencher: Bencher, iters: usize) {
    use bevy_ecs::prelude::*;
    use bevy_tasks::{ComputeTaskPool, TaskPool};

    if PARALLEL {
        ComputeTaskPool::get_or_init(TaskPool::new);
    }

    let mut world = World::new();

    for y in 0..IMAGE_SIZE {
        for x in 0..IMAGE_SIZE {
            world.spawn((Pixel(x, y), Output(false)));
        }
    }

    let mut state = world.query::<(&Pixel, &mut Output)>();

    bencher.bench_local(|| {
        if PARALLEL {
            state
                .par_iter_mut(&mut world)
                .for_each(move |(&Pixel(x, y), mut out)| {
                    out.bypass_change_detection().0 = mandelbrot(x, y, iters);
                });
        } else {
            state
                .iter_mut(&mut world)
                .for_each(|(&Pixel(x, y), mut out)| {
                    out.bypass_change_detection().0 = mandelbrot(x, y, iters);
                });
        }
    });
}

#[inline]
fn mandelbrot(x: u16, y: u16, iters: usize) -> bool {
    let scale = 2.;

    let cr = f64::from(x) / f64::from(IMAGE_SIZE - 1) * scale - scale / 2.;
    let ci = f64::from(y) / f64::from(IMAGE_SIZE - 1) * scale - scale / 2.;

    let mut zr = cr;
    let mut zi = ci;

    for _ in 0..iters {
        if zr * zr + zi * zi >= 4. {
            return false;
        }

        (zr, zi) = (zr * zr - zi * zi + cr, 2. * zr * zi + ci);
    }

    true
}

#[allow(unused)]
fn make_mandel_pgm(iters: usize) -> std::io::Result<()> {
    let mut pgm = format!("P2 {IMAGE_SIZE} {IMAGE_SIZE} 1\n");

    for y in 0..IMAGE_SIZE {
        for x in 0..IMAGE_SIZE {
            pgm += if mandelbrot(x, y, iters) { " 1" } else { " 0" };
        }
        pgm += "\n";
    }

    std::fs::write("mandel.pgm", pgm)
}
