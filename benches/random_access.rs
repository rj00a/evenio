//! Measures performance of random access component lookups, i.e.
//! `Fetcher::get`.

use divan::{black_box, Bencher};

fn main() {
    divan::main()
}

const TOTAL_ENTITIES: usize = 10_000;
const FETCHED_COUNT: usize = 1000;

#[divan::bench]
fn random_access_evenio(bencher: Bencher) {
    use evenio::prelude::*;

    let mut entities = vec![];
    let mut world = World::new();

    macro_rules! add_components {
        ($world:ident; $entities:ident; $($name:ident)*) => {
            $(
                #[derive(Component, Default)]
                struct $name(#[allow(dead_code)] u64);
            )*

            #[derive(Query)]
            #[allow(non_snake_case, dead_code)]
            struct ComponentQuery<'a> {
                $(
                    $name: &'a $name,
                )*
            }

            for i in 0..TOTAL_ENTITIES {
                let e = $world.spawn();

                if i % (TOTAL_ENTITIES / FETCHED_COUNT) == 0 {
                    $entities.push(e);
                }

                $(
                    $world.insert(e, $name(i as u64));
                )*
            }
        }
    }

    add_components!(world; entities;
        C0 C1 C2 C3 C4 C5 C6 C7 /*C8 C9
        C10 C11 C12 C13 C14 C15 C16 C17 C18 C19
        C20 C21 C22 C23 C24 C25 C26 C27 C28 C29
        C30 C31 C32 C33 C34 C35 C36 C37 C38 C39*/
    );

    #[derive(Event)]
    struct E;

    world.add_system(move |_: Receiver<E>, f: Fetcher<ComponentQuery>| {
        for &e in &entities {
            let _ = black_box(f.get(e));
        }
    });

    bencher.bench_local(|| world.send(E));
}

#[divan::bench]
fn random_access_bevy(bencher: Bencher) {
    #![allow(non_snake_case)] // Can't add attributes to the generated `ComponentQueryItem`.

    use bevy_ecs::prelude::*;
    use bevy_ecs::query::QueryData;
    use bevy_ecs::schedule::ScheduleLabel;

    let mut entities = vec![];
    let mut world = World::new();

    macro_rules! add_components {
        ($world:ident; $entities:ident; $($name:ident)*) => {
            $(
                #[derive(Component, Default)]
                struct $name(#[allow(dead_code)] u64);
            )*

            #[derive(QueryData)]
            #[allow(dead_code)]
            struct ComponentQuery<'a> {
                $(
                    $name: &'a $name,
                )*
            }

            for i in 0..TOTAL_ENTITIES {
                let e = $world.spawn((
                    $(
                        $name(i as u64)
                    ),*
                ));

                if i % (TOTAL_ENTITIES / FETCHED_COUNT) == 0 {
                    $entities.push(e.id());
                }
            }
        }
    }

    add_components!(world; entities;
        C0 C1 C2 C3 C4 C5 C6 C7 /*C8 C9
        C10 C11 C12 C13 C14 C15 C16 C17 C18 C19
        C20 C21 C22 C23 C24 C25 C26 C27 C28 C29
        C30 C31 C32 C33 C34 C35 C36 C37 C38 C39*/
    );

    #[derive(ScheduleLabel, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
    struct Whatever;

    let mut sched = Schedule::new(Whatever);

    sched.add_systems(move |q: Query<ComponentQuery>| {
        for &e in &entities {
            let _ = black_box(q.get(e));
        }
    });

    bencher.bench_local(|| sched.run(&mut world));
}
