use std::panic;
use std::sync::Arc;

use crate::prelude::*;

#[test]
fn world_drops_events_on_panic() {
    #[derive(Event)]
    struct A(Arc<()>);

    #[derive(Event)]
    struct B(Arc<()>);

    #[derive(Event)]
    struct C(Arc<()>);

    let mut world = World::new();

    world
        .add_system(|a: &A, mut sender: Sender<B>| {
            sender.send(B(a.0.clone()));
            sender.send(B(a.0.clone()));
        })
        .unwrap();

    world
        .add_system(|b: &B, mut sender: Sender<C>| {
            sender.send(C(b.0.clone()));
            sender.send(C(b.0.clone()));
        })
        .unwrap();

    world.add_system(|_: &C| panic!("oops!")).unwrap();

    let arc = Arc::new(());
    let arc_cloned = arc.clone();

    let res = panic::catch_unwind(move || world.send_event(A(arc_cloned)));

    assert_eq!(*res.unwrap_err().downcast::<&str>().unwrap(), "oops!");

    assert_eq!(Arc::strong_count(&arc), 1);
}

#[test]
fn conflicting_sender_params() {
    let mut world = World::new();

    #[derive(Event)]
    struct A(i32);

    fn system(_: &A, _: Sender<A>, _: Sender<A>) {}

    // assert!(world.add_system(system).is_err());

    world.send_event(A(123));
}
