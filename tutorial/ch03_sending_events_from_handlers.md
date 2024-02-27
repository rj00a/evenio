# Sending Events From Handlers

Previously, we've seen how to send events using the [`World::send`] method.
But to send events from _within_ a handler, we'll need to use the [`Sender`] handler parameter:

```rust
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(Event)]
struct A;
#[derive(Event)]
struct B;
#[derive(Event)]
struct C;

world.add_handler(|_: Receiver<A>, mut sender: Sender<(B, C)>| {
    sender.send(B);
    sender.send(C);
    println!("sent B and C!");
});

world.add_handler(|_: Receiver<B>| println!("got B!"));
world.add_handler(|_: Receiver<C>| println!("got C!"));

world.send(A);
```

Output:

```txt
sent B and C!
got B!
got C!
```

In the parameter `Sender<(B, C)>`, the `(B, C)` is the set of events the sender is allowed to send.
Attempting to send an event that is not in this set will fail.

Note that [`Sender::send`] does not immediately send events, but rather adds them to the front of the **event queue** in reverse order.
The next event in the queue begins broadcasting once all handlers for the current event have finished.

```rust
use evenio::prelude::*;

#[derive(Event)]
struct A;
#[derive(Event, Debug)]
struct B(i32);
#[derive(Event, Debug)]
struct C(i32);

fn get_a_send_b(_: Receiver<A>, mut sender: Sender<B>) {
    sender.send(B(0));
    sender.send(B(3));
    println!("got A, sending B twice!");
}

fn get_b_send_c(r: Receiver<B>, mut sender: Sender<C>) {
    sender.send(C(r.event.0 + 1));
    sender.send(C(r.event.0 + 2));
    println!("got {:?}, sending C twice!", r.event);
}

fn get_c(r: Receiver<C>) {
    println!("got {:?}!", r.event);
}

let mut world = World::new();

world.add_handler(get_a_send_b);
world.add_handler(get_b_send_c);
world.add_handler(get_c);

println!("sending A!");
world.send(A);
```

Output:
```txt
sending A!
got A, sending B twice!
got B(0), sending C twice!
got C(1)!
got C(2)!
got B(3), sending C twice!
got C(4)!
got C(5)!
```

[`World::send`]: crate::world::World::send
[`Sender`]: crate::event::Sender
[`Sender::send`]: crate::event::Sender::send
