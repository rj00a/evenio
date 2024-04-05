# The World

To begin using the library, we'll need a `World` to store all our data.

```rust
// Import the most commonly used items.
use evenio::prelude::*;

let mut world = World::new();
```

An application usually needs a single `World`, but multiple worlds can be used if the need arises.[^1]

# Handlers and Events

`Handler`s are event handler functions that enable efficient access to the world. The majority of your application logic will likely live within handlers.

Events are messages that handlers listen for.
A handful of events are defined by the library and have special effects when sent.
However, most events are defined by the user.

To get started, let's create a "hello world" handler which prints a message whenever our `Message` event is sent.

```rust
use evenio::prelude::*;

#[derive(Event)]
struct Message(String);

let mut world = World::new();

// Add the event handler to our world.
world.add_handler(my_handler);

// Send our event.
world.send(Message("Hello, World!".into()));

fn my_handler(r: Receiver<Message>) {
    println!("The message is: \"{}\"", &r.event.0);
}
```

Output:

```txt
The message is: "Hello, World!"
```

`Receiver<E>` is a _handler parameter_ which tells the handler that we would like to listen for events of type `E`.

Note that handlers must listen for exactly one event type. Attempting to listen for more than one event or no events at all will panic.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
// Panics. (lambdas are also handlers)
world.add_handler(|| {});
```

## Handler Ordering

When multiple handlers listen for the same event, we'll need to consider the order those handlers should run when the event is sent.

Handler order is first determined by the handler's [`HandlerPriority`]. This is a enum with three states: `High`, `Medium`, and `Low`. `Medium` is the default.
If handlers have the same priority, then we fall back on the order the handlers were added to the `World` to decide the order.

[`HandlerPriority`]: crate::handler::HandlerPriority

```rust
use evenio::prelude::*;

let mut world = World::new();

world.add_handler(handler_a);
world.add_handler(handler_b);
// Give this handler the `High` priority.
world.add_handler(handler_c.high());

world.send(MyEvent);

#[derive(Event)]
struct MyEvent;

fn handler_a(_: Receiver<MyEvent>) {
    println!("handler A");
}

fn handler_b(_: Receiver<MyEvent>) {
    println!("handler B");
}

fn handler_c(_: Receiver<MyEvent>) {
    println!("handler C");
}
```

Output:
```txt
handler C
handler A
handler B
```

Although `handler_c` was added to the world last, it was given `Priority::High`, so it ran first.

[^1]: Be careful when mixing identifiers from different worlds. An identifier for an item in one `World`, such as an `EntityId`, is meaningless when used in a different `World`.
