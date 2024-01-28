//! Performance of spawning an entity, adding many components to it, and
//! despawning.

use divan::{black_box, Bencher};

fn main() {
    divan::main()
}

const COUNTS: [u64; 10] = [1, 2, 3, 5, 10, 15, 20, 25, 30, 40];

#[divan::bench(args = COUNTS, sample_size = 10)]
fn spawn_ent_many_comps_n_evenio(bencher: Bencher, count: u64) {
    use evenio::prelude::*;

    macro_rules! add_components {
        ($world:ident, $e:ident, $n:ident, $($index:literal)*) => {
            $(
                {
                    #[derive(Component)]
                    struct C(#[allow(dead_code)] u64);

                    if $n <= $index {
                        return;
                    }

                    $world.insert($e, black_box(C(0)));
                }
            )*
        }
    }

    let mut world = World::new();

    bencher.bench_local(|| {
        let e = world.spawn();

        add_components!(world, e, count,
            0 1 2 3 4 5 6 7 8 9 10
            11 12 13 14 15 16 17 18 19 20
            21 22 23 24 25 26 27 28 29 30
            31 32 33 34 35 36 37 38 39 40
        );

        world.despawn(e);

        black_box(&mut world);
    });
}

#[divan::bench(sample_size = 10)]
fn spawn_ent_many_comps_40_bevy(bencher: Bencher) {
    use bevy_ecs::prelude::*;

    macro_rules! def_bundle {
        ($($name:ident)*) => {
            $(
                #[derive(Component, Default)]
                struct $name(#[allow(dead_code)] u64);
            )*

            #[derive(Bundle, Default)]
            #[allow(non_snake_case)]
            struct ALotOfComponents {
                $(
                    $name: $name,
                )*
            }
        }
    }

    def_bundle!(
        C0 C1 C2 C3 C4 C5 C6 C7 C8 C9
        C10 C11 C12 C13 C14 C15 C16 C17 C18 C19
        C20 C21 C22 C23 C24 C25 C26 C27 C28 C29
        C30 C31 C32 C33 C34 C35 C36 C37 C38 C39
    );

    let mut world = World::new();

    bencher.bench_local(|| {
        let e = world.spawn(black_box(ALotOfComponents::default()));

        e.despawn();

        black_box(&mut world);
    });
}
