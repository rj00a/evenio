# Mutable Events

So far, we've only had immutable access to events from within systems.
But sometimes it's desirable to mutate events so that later systems will see the event differently.

We can achieve this by using the `ReceiverMut` system parameter.

```rust
# use evenio::prelude::*;
#
# let mut world = World::new();
#
#[derive(Event)]
struct MyEvent(i32);

world.add_system(|mut r: ReceiverMut<MyEvent>| {
    println!("Event is: {}", r.event.0);
    r.event.0 += 10;
});

world.add_system(|r: Receiver<MyEvent>| {
    println!("Event is now: {}", r.event.0);
});

world.send(MyEvent(42));
```

This prints:
```txt
Event is: 42
Event is now: 52
```

# Event Ownership

We can take this a step further and _take ownership_ of the event.
Doing so will stop the event from broadcasting and later systems will completely miss the event.

```rust
# use evenio::prelude::*;
# 
# let mut world = World::new();
#[derive(Event)]
struct MyEvent(u32);

world.add_system(|r: ReceiverMut<MyEvent>| {
    if r.event.0 % 2 == 0 {
        EventMut::take(r.event);
    }
});

world.add_system(|r: Receiver<MyEvent>| {
    println!("got event: {}", r.event.0);
});

for i in 0..10 {
    world.send(MyEvent(i));
}
```

Output:

```txt
got event: 1
got event: 3
got event: 5
got event: 7
got event: 9
```

This can be useful in a number of different scenarios.
Consider a `TakeDamage` event used to signal an entity taking damage:
With event consumption, we can take ownership of the event before it has a chance to propagate, effectively creating an "invincibility" effect.
