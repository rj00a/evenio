# The World

To begin using the library, we'll need a `World` to store all our data.

```rust
// Import the most commonly used items.
use evenio::prelude::*;

let mut world = World::new();
```

An application usually only needs a single `World`, but multiple worlds can be created if the need arises.[^1]

# Systems and Events

Systems are event handler functions that enable efficient access to the world. The majority of your application logic will likely live within systems.

Events are messages we send in order to communicate with systems.

To get started, let's create a "hello world" system which prints a message whenever a `Message` event is sent.

```rust
use evenio::prelude::*;

#[derive(Event)]
struct Message(String);

let mut world = World::new();

// Add the system to our world.
world.add_system(my_system);

// Send our event.
world.send(Message("Hello, World!".into()));

fn my_system(r: Receiver<Message>) {
    println!("The message is: \"{}\"", &r.event.0);
}
```

This prints:

```txt
The message is: "Hello, World!"
```

`Receiver<E>` is a special _system parameter_ which tells the library that we would like to listen for events of type `E`.

Note that systems must listen for exactly one event type. Adding a system with more than one `Receiver` or no receivers at all will panic.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
// Panics. (lambdas are also systems)
world.add_system(|| {});
```

## System Ordering

When multiple systems listen for the same event, we'll need to consider the order those systems should run when the event is sent.

System order is first determined by the system's [`Priority`]. This is a simple enum with three possible states: [`Priority::Before`], [`Priority::Normal`], and [`Priority::After`]. `Priority::Normal` is the default.
If systems have the same priority, then we fall back on the order the systems were added to the `World` to decide the order.

[`Priority`]: crate::system::Priority
[`Priority::Before`]: crate::system::Priority::Before
[`Priority::Normal`]: crate::system::Priority::Normal
[`Priority::After`]: crate::system::Priority::After

```rust
use evenio::prelude::*;

let mut world = World::new();

world.add_system(system_a);
world.add_system(system_b);
// Give this system the `Before` priority.
world.add_system(system_c.before());

world.send(MyEvent);

#[derive(Event)]
struct MyEvent;

fn system_a(_: Receiver<MyEvent>) {
    println!("system A");
}

fn system_b(_: Receiver<MyEvent>) {
    println!("system B");
}

fn system_c(_: Receiver<MyEvent>) {
    println!("system C");
}
```

This prints:
```txt
system C
system A
system B
```

Although `system_c` was added to the world last, it was given `Priority::Before`, so it ran first.

[^1]: Be careful when mixing identifiers from different worlds. An identifier for an item in one `World`, such as an `EntityId`, is meaningless when used in a different `World`.
