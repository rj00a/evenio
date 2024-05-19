//! Measures performance of sending evenio events.

use divan::{black_box, Bencher};
use evenio::prelude::*;

fn main() {
    divan::main()
}

#[divan::bench(sample_count = 1000)]
fn send_many_global_events(bencher: Bencher) {
    let mut world = World::new();

    #[derive(GlobalEvent)]
    struct A(u64);

    #[derive(GlobalEvent)]
    struct B(u64);

    #[derive(GlobalEvent)]
    struct C(u64);

    #[derive(GlobalEvent)]
    struct D(#[allow(dead_code)] u64);

    world.add_handler(get_a_send_b);
    world.add_handler(get_b_send_c);
    world.add_handler(get_c_send_d);

    fn get_a_send_b(r: Receiver<A>, s: Sender<B>) {
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
    }

    fn get_b_send_c(r: Receiver<B>, s: Sender<C>) {
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
    }

    fn get_c_send_d(r: Receiver<C>, s: Sender<D>) {
        s.send(D(r.event.0));
        s.send(D(r.event.0));
        s.send(D(r.event.0));
        s.send(D(r.event.0));
        s.send(D(r.event.0));
    }

    bencher.bench_local(|| {
        world.send(A(123));
        black_box(&mut world);
    });
}

#[divan::bench]
fn send_many_targeted_events(bencher: Bencher) {
    let mut world = World::new();

    #[derive(TargetedEvent, Component)]
    struct A(u64);

    #[derive(TargetedEvent, Component)]
    struct B(u64);

    #[derive(TargetedEvent, Component)]
    struct C(u64);

    #[derive(TargetedEvent, Component)]
    struct D(#[allow(dead_code)] u64);

    let e = world.spawn();
    world.insert(e, A(123));
    world.insert(e, B(456));
    world.insert(e, C(789));
    world.insert(e, D(101112));

    world.add_handler(get_a_send_b);
    world.add_handler(get_b_send_c);
    world.add_handler(get_c_send_d);

    fn get_a_send_b(r: Receiver<A, (EntityId, &A)>, s: Sender<B>) {
        s.send_to(r.query.0, B(r.query.1 .0));
        s.send_to(r.query.0, B(r.query.1 .0));
        s.send_to(r.query.0, B(r.query.1 .0));
        s.send_to(r.query.0, B(r.query.1 .0));
        s.send_to(r.query.0, B(r.query.1 .0));
    }

    fn get_b_send_c(r: Receiver<B, (EntityId, &B)>, s: Sender<C>) {
        s.send_to(r.query.0, C(r.query.1 .0));
        s.send_to(r.query.0, C(r.query.1 .0));
        s.send_to(r.query.0, C(r.query.1 .0));
        s.send_to(r.query.0, C(r.query.1 .0));
        s.send_to(r.query.0, C(r.query.1 .0));
    }

    fn get_c_send_d(r: Receiver<C, (EntityId, &C)>, s: Sender<D>) {
        s.send_to(r.query.0, D(r.query.1 .0));
        s.send_to(r.query.0, D(r.query.1 .0));
        s.send_to(r.query.0, D(r.query.1 .0));
        s.send_to(r.query.0, D(r.query.1 .0));
        s.send_to(r.query.0, D(r.query.1 .0));
    }

    bencher.bench_local(|| {
        world.send_to(e, A(123));
        black_box(&mut world);
    });
}
