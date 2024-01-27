# The World

To begin using the library, we'll need a `World` to store all our data.

```rust
// Import the most commonly used items.
use evenio::prelude::*;

let mut world = World::new();
```

An application usually needs a single `World`, but multiple worlds can be used if the need arises.[^1]

# Systems and Events

Systems are event handler functions that enable efficient access to the world. The majority of your application logic will likely live within systems.

Events are messages that systems listen for.
A handful of events are defined by the library and have special effects when sent.
However, most events are defined by the user.

To get started, let's create a "hello world" system which prints a message whenever our `Message` event is sent.

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

Output:

```txt
The message is: "Hello, World!"
```

`Receiver<E>` is a _system parameter_ which tells the system that we would like to listen for events of type `E`.

Note that systems must listen for exactly one event type. Attempting to listen for more than one event or no events at all will panic.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
// Panics. (lambdas are also systems)
world.add_system(|| {});
```

## System Ordering

When multiple systems listen for the same event, we'll need to consider the order those systems should run when the event is sent.

System order is determined by the system's [`Dependencies`]. Those contain information about which systems should run before or after a certain system.

[`Dependencies`]: crate::system::Dependencies

```rust
use evenio::prelude::*;

let mut world = World::new();

let a = world.add_system(system_a);
// Run system B after A
world.add_system(system_b.after(a));
// Run system C before A
world.add_system(system_c.before(a));

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

Output:

```txt
system C
system A
system B
```

Although `system_c` was added to the world last, it was forced to run before `system_a`, which should run before `system_b`.

[^1]: Be careful when mixing identifiers from different worlds. An identifier for an item in one `World`, such as an `EntityId`, is meaningless when used in a different `World`.
