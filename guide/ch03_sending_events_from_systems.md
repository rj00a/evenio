# Sending Events From Systems

Previously, we've seen how to send events using the [`World::send`] method. But to send events from within a system, we'll need to use the [`Sender`] system parameter:

```rust
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(Event)]
struct A;
#[derive(Event)]
struct B;
#[derive(Event)]
struct C;

world.add_system(|_: Receiver<A>, mut sender: Sender<(B, C)>| {
    sender.send(B);
    sender.send(C);
    println!("sent B and C!");
});

world.add_system(|_: Receiver<B>| println!("got B!"));
world.add_system(|_: Receiver<C>| println!("got C!"));
```

Output:

```txt
sent B and C!
got B!
got C!
```

In the parameter `Sender<(B, C)>` the `(B, C)` is the set of events the sender is allowed to send.
Attempting to send an event that is not in this set will fail.

Note that [`Sender::send`] does not immediately send the event. It is instead added to an _event queue_, which is flushed once the system returns.

Finally, note that events are evaluated recursively: Events sent by systems will finish broadcasting before the outer event has finished broadcasting.
This is similar to a call stack or pre-order tree traversal. Consider the program:

```rust
use evenio::prelude::*;

#[derive(Event)]
struct A;
#[derive(Event)]
struct B;
#[derive(Event)]
struct C;

let mut world = World::new();

world.add_system(get_a_send_b);
world.add_system(get_b_send_c);
world.add_system(get_c);

println!("sending A!");
world.send(A);

fn get_a_send_b(_: Receiver<A>, mut sender: Sender<B>) {
    sender.send(B);
    sender.send(B);
    println!("got A, sending B twice!");
}

fn get_b_send_c(_: Receiver<B>, mut sender: Sender<C>) {
    sender.send(C);
    sender.send(C);
    println!("got B, sending C twice!");
}

fn get_c(_: Receiver<C>) {
    println!("got C!");
}
```

Output:
```txt
sending A!
got A, sending B twice!
got B, sending C twice!
got C!
got C!
got B, sending C twice!
got C!
got C!
```

The control flow of the above program can be visualized with the following diagram:

```txt
        ┌─┐
    ┌───┤A├───┐
    │   └─┘   │
    │         │
   ┌▼┐       ┌▼┐
 ┌─┤B├─┐   ┌─┤B├─┐
 │ └─┘ │   │ └─┘ │
 │     │   │     │
┌▼┐   ┌▼┐ ┌▼┐   ┌▼┐
│C│   │C│ │C│   │C│
└─┘   └─┘ └─┘   └─┘
```

[`World::send`]: crate::world::World::send
[`Sender`]: crate::event::Sender
[`Sender::send`]: crate::event::Sender::send
