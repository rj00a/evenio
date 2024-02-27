//! Measures performance of sending evenio events.

use divan::{black_box, Bencher};
use evenio::prelude::*;

fn main() {
    divan::main()
}

#[divan::bench(sample_count = 1000)]
fn send_many_events(bencher: Bencher) {
    let mut world = World::new();

    #[derive(Event)]
    struct A(u64);

    #[derive(Event)]
    struct B(u64);

    #[derive(Event)]
    struct C(u64);

    #[derive(Event)]
    struct D(#[allow(dead_code)] u64);

    world.add_handler(get_a_send_b);
    world.add_handler(get_b_send_c);
    world.add_handler(get_c_send_d);

    fn get_a_send_b(r: Receiver<A>, mut s: Sender<B>) {
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
        s.send(B(r.event.0));
    }

    fn get_b_send_c(r: Receiver<B>, mut s: Sender<C>) {
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
        s.send(C(r.event.0));
    }

    fn get_c_send_d(r: Receiver<C>, mut s: Sender<D>) {
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
