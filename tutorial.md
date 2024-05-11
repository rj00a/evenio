# Why ECS?

Entity Component System (often shortened to 'ECS') is a programming design pattern in which the structure of the application is broken up into **Entities**, **Components**, and **Systems**.
- **Entities** are the "things" in the application, such as a player, UI widget, or scene.
  Entities are identified with simple index-based [`EntityId`] handles.
- **Components** represent the capabilities that an entity posesses. Entities are described as a set of components. For instance, an asteroid entity flying through space might have a `Position` and `Velocity` component to describe its flight path over time. In `evenio`, components are Rust data types.
- **Systems** are procedures which act on all entities with certain combinations of components. A physics system might need an entity's `Position`, `Velocity`, and `Mass` components in order to calculate the object's next position. This typically means you have a list of systems that run in sequence (or partially in parallel) every frame or time step of your application.

ECS helps solve a number of problems that often arise in traditional object-oriented software designs. Different "types" of entities are able to share components, eliminating the need for inheritance. Additionally, ECS can be very efficient due to the way that components are organized in memory.

ECS is especially useful in Rust because it can, in some sense, ergonomically sidestep Rust's aliasing rules. Concurrent entity component access is made possible so long as it conforms to Rust's aliasing XOR mutability demands.

# Why `evenio`?

Interactive applications are largely event driven. When the player attacks a monster, reduce its health. When an egg hits the ground, break it open. When the "play" button is clicked, start the game. Reifying events in code is a useful design tool because it gives downstream code something to hook into and subsequently change the behavior of the program.

However, many prior Rust ECS libraries have trouble expressing the control flow that events require. Some frameworks will suggest writing events to a temporary buffer and then handling those events in a later system. This is a "pull" style of event propagation in which event consumers must reguarly poll the buffer for work. This works well in some cases, but runs into some problems:
- Creates system ordering considerations. Consider what happens if an event is written to the buffer _after_ the event handling systems run. The event might not be handled until the next frame, or never.
- Funneling a sequence of events into separate event buffers loses the order between buffers. This is a problem when handling a stream of incoming network packets, for instance.
- Systems waste time polling the event buffer every frame even when there are no events available.

The conclusion, then, is that traditional batch-oriented run-once-per-frame systems alone are insufficient for expressing many kinds of application logic. This may not come as a surprise in hindsight, but due to Rust's constraints, it's tempting to apply this design pattern everywhere. When all you have is a hammer, everything looks like a nail.

Evenio solves these problems by introducing "push" style events as a first-class citizen. As you'll see later, all systems are generalized as event handlers and run immediately in response to events. The goal is to make it easy to write _correct_ code by making the rules around event propagation easy to understand. Events need to be fast, too!

# The World

To begin using the library, we'll need a [`World`].

```rust
use evenio::prelude::*; // Import the most commonly used items.

let mut world = World::new(); // Create an empty world.
```

The `World` is essentially a database of all the data that the ECS can access. It contains all the entities, components, and event handlers. Later, we'll see how to add data to the world and query for data within it.

An application usually only needs a single `World`, but multiple worlds can be used if the need arises.

Also note that `World`s are not [`Send`] or [`Sync`] because they are allowed to contain data that is not `Send` or `Sync`. This means that you cannot share an entire `World` across threads (but parallel data access is still possible, as we'll see later).

# Handlers and Events

[`Hander`]s are callbacks that run in response to events.

To get started, let's create a "hello world" handler that runs in response to our custom `Message` event.

```rust
use evenio::prelude::*;

#[derive(GlobalEvent)]
struct Message<'a>(&'a str);

let mut world = World::new();

// Add the event handler to our world.
world.add_handler(my_handler);

// Send our event.
world.send(Message("Hello, World!"));

fn my_handler(r: Receiver<Message>) {
    println!("The message is: \"{}\"", r.event);
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

# Handler Ordering

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

#[derive(GlobalEvent)]
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

# Event Mutation

So far, we've only had immutable access to events from within handlers.
But sometimes it's desirable to mutate events so that later handlers will see the event differently.

We can achieve this by using the [`ReceiverMut`] handler parameter. `ReceiverMut` provides a mutable reference to the event.

```rust
# use evenio::prelude::*;
#
# let mut world = World::new();
#
#[derive(Event)]
struct MyEvent(i32);

world.add_handler(|mut r: ReceiverMut<MyEvent>| {
    println!("Event is: {}", r.event.0);
    r.event.0 += 10;
});

world.add_handler(|r: Receiver<MyEvent>| {
    println!("Event is now: {}", r.event.0);
});

world.send(MyEvent(42));
```

This prints:
```txt
Event is: 42
Event is now: 52
```

You should prefer using `Receiver` whenever possible, and only use `ReceiverMut` when mutable access to the event is needed.

[`ReceiverMut`]: crate::event::ReceiverMut

## Event Ownership

We can take this a step further and entirely _take ownership_ of the event. Doing so will stop the event from broadcasting and later handlers will not receive the event. This is done with [`EventMut::take`].

```rust
# use evenio::prelude::*;
# 
# let mut world = World::new();
# 
#[derive(GlobalEvent)]
struct MyEvent(u32);

world.add_handler(|r: ReceiverMut<MyEvent>| {
    if r.event.0 % 2 == 0 {
        EventMut::take(r.event);
    }
});

world.add_handler(|r: Receiver<MyEvent>| {
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

This can be useful in a number of different scenarios. Consider a `TakeDamage` event used to signal an entity taking damage in a game. With event consumption, we can take ownership of the event before it has a chance to propagate, effectively creating an "invincibility" effect.

# Sending Events From Handlers

Previously, we've seen how to send events using the [`World::send`] method.
But to send events from _within_ a handler, we'll need to use the [`Sender`] handler parameter:

```rust
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(GlobalEvent)]
struct A;
#[derive(GlobalEvent)]
struct B;
#[derive(GlobalEvent)]
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

# Entities and Components

Entities and components are the bread and butter of any ECS framework.

Entities make up the _things_ in your application, such as monsters, players, cameras, GUI elements, or items.
By itself, an entity is just a unique identifier ([`EntityId`]). It is the entity's set of _components_ that differentiate a player from a monster, or a camera from an item. Components hold the data while handlers operate on entities with certain sets of components.

Let's see how we might model player and monster entities in a game with `evenio`.

```rust
use evenio::prelude::*;

#[derive(Component)]
struct Health(i32);

#[derive(Component)]
struct Position {
    x: f32,
    y: f32,
}

// A zero-sized component, often called a "marker" or "tag".
#[derive(Component)]
struct Player;

#[derive(Component)]
struct Monster;

let mut world = World::new();

// Spawn two entities without any components.
// `spawn()` returns a lightweight handle we can use to look up the entity later.
let player = world.spawn();
let monster = world.spawn();
let something_else = world.spawn();

// Insert the player components.
world.insert(player, Health(100));
world.insert(player, Position { x: 0.0, y: 0.0 });
world.insert(player, Player);

// Insert the monster components.
world.insert(monster, Health(20));
world.insert(monster, Position { x: 100.0, y: 100.0 });
world.insert(monster, Monster);

// Something else?
world.insert(something_else, Position { x: 42.0, y: 42.0 });
```

We can visualize our data using a table where the rows are entities and columns are components:

| Entity ID | Health | Position   | Player | Monster | 
|-----------|--------|------------|--------|---------|
| 0         | 100    | (0, 0)     | ✅     |         |
| 1         | 20     | (100, 100) |        | ✅      |
| 2         |        | (42, 42)   |        |         |

[`EntityId`]: crate::entity::EntityId

## Bundling Components

If our game has lots of monsters with lots of components, we might grow tired of inserting the components individually.
To fix this, we can create a new event and handler to do the inserting for us.

```rust
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# struct Health(i32);
# #[derive(Component)]
# struct Position { x: f32, y: f32 }
# #[derive(Component)]
# struct Player;
# #[derive(Component)]
# struct Monster;
#[derive(Event, Clone, Copy)]
struct InitMonster {
    entity: EntityId,
    pos: [f32; 2],
}

fn init_monster_handler(
    r: Receiver<InitMonster>,
    mut s: Sender<(Insert<Health>, Insert<Position>, Insert<Monster>)>
) {
    let InitMonster {
        entity,
        pos: [x, y]
    } = *r.event;

    s.insert(entity, Health(20));
    s.insert(entity, Position { x, y });
    s.insert(entity, Monster);
}

world.add_handler(init_monster_handler);

// Test our new handler:
let entity = world.spawn();
world.send(InitMonster {
    entity,
    pos: [24.0, 24.0],
});

assert!(world.get::<Monster>(entity).is_some());
```

Whenever we insert a component on an entity, what we're actually doing is sending the special [`Insert`] event for that component.
Because of this, we have to specify the correct `Insert` events in the handler's `Sender` above.

[`Insert`]: crate::event::Insert

## Cleanup

To remove an entity from the world, we send the [`Despawn`] event.
This will drop all of the entity's components and make its `EntityId` invalid.
The `EntityId` will never be reused by a new entity.

Alternatively, if we just want to remove components without deleting the entire entity, we send the [`Remove`] event for the specific component(s).

```rust
# use evenio::prelude::*;
# let mut world = World::new();
# 
#[derive(Component)]
struct A;

let e = world.spawn();
world.insert(e, A);

// Component exists.
assert!(world.get::<A>(e).is_some());

world.remove::<A>(e);

// No more component.
assert!(world.get::<A>(e).is_none());

// Sending the `Remove` event again has no effect.
world.remove::<A>(e);
assert!(world.get::<A>(e).is_none());

world.despawn(e);

// Entity no longer exists.
assert!(!world.entities().contains(e));
```

[`Despawn`]: crate::event::Despawn
[`Remove`]: crate::event

## Performance Considerations

Internally, all entities with the same set of components are organized into groups called _archetypes_.
Archetypes enable fast entity iteration and cleanup among other benefits.

However, this design means that adding or removing a component on an entity will force all components of that entity to move somewhere else in memory, which can be slow.
If components are being rapidly added and removed, consider using a `bool` or an `Option` inside the component instead.

# Fetching

In the previous chapter, we've seen how to create entities and add components to them.
But components aren't very useful unless we have some way to access them from within handlers.

This is where the [`Fetcher`] handler parameter comes in.
`Fetcher<Q>` allows for both random access entity lookups using an `EntityId` and iteration over all entities matching some [`Query`] `Q`.

```rust
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(Event)]
struct E;

#[derive(Component, Debug)]
struct A(&'static str);

#[derive(Component, Debug)]
struct B(i32);

let e1 = world.spawn();
world.insert(e1, A("foo"));
world.insert(e1, B(123));

let e2 = world.spawn();
world.insert(e2, A("bar"));

let e3 = world.spawn();
world.insert(e3, B(456));

world.add_handler(move |_: Receiver<E>, fetcher: Fetcher<&A>| {
    // Get a reference to the `A` component on entity `e2`.
    // Returns `None` if the query doesn't match.
    let a = fetcher.get(e1).unwrap();
    println!("fetcher.get(): {a:?}");
    
    // `e3` doesn't have component `A`.
    assert!(fetcher.get(e3).is_err());

    // Iterate over all entities with the `A` component.
    // Entities are visited in a deterministic but unspecified order.
    for a in fetcher.iter() {
        println!("fetcher.iter(): {a:?}");
    }
});

world.send(E);
```

Output:
```txt
fetcher.get(): A("foo")
fetcher.iter(): A("bar")
fetcher.iter(): A("foo")
```

## Queries

Queries are expressions used to filter and access data from entities.
In the previous example, we used the `&A` query to match entities with the `A` component, but there are many other queries to choose from:

| Query               | Description                                                                                       |
|---------------------|---------------------------------------------------------------------------------------------------|
| `&C`                | Matches entities with component `C` and returns an immutable reference to `C`.                    |
| `&mut C`            | Matches entities with component `C` and returns a mutable reference to `C`.                       |
| `(Q₁, Q₂, …, Qₙ)`   | Matches if all queries `Q₁` through `Qₙ` match. Returns a tuple of the results.                   |
| `()`                | The empty tuple matches all entities and returns nothing.                                         |
| `Option<Q>`         | Returns the result of query `Q` as `Some`, or `None` if `Q` does not match. Matches all entities. |
| `Or<Q₁, Q₂>`        | Matches if `Q₁` or `Q₂` matches. Returns the matched query results.                               |
| `Xor<Q₁, Q₂>`       | Matches if `Q₁` or `Q₂` matches, but not both. Returns the matched query results.                 |
| `EntityId`          | Returns the [`EntityId`] of the matched entity. Matches all entities.                             |
| `Has<Q>`            | Returns a boolean indicating whether the query `Q` matches. Matches all entities.                 |
| `With<Q>`           | Matches if `Q` matches. Returns nothing and does not access the result of `Q`.                    |
| `Not<Q>`            | Matches if `Q` does not match. Returns nothing.                                                   |

[`Fetcher`]: crate::fetch::Fetcher
[`Query`]: crate::query::Query
[`EntityId`]: crate::entity::EntityId

## Derived Queries

`Query` can be derived on structs so long as every field is a `Query`.
Derived queries act like tuples — every field must match in order for the whole query to match.

Compared to tuples, however, derived query structs offer some advantages.
Fields are named and methods can be implemented on the struct – handy for complex or frequently used queries.

```rust
# use evenio::prelude::*;
# #[derive(GlobalEvent)] struct E;
# #[derive(Component, Debug)] struct A;
# #[derive(Component, Debug)] struct B;
# #[derive(Component, Debug)] struct C;
# let mut world = World::new();
#
#[derive(Query, Debug)]
struct MyQuery<'a> {
    foo: &'a A,
    bar: &'a mut B,
    _baz: With<&'static C>,
}

impl MyQuery<'_> {
    fn print(&self) {
        println!("foo: {:?}, bar: {:?}", self.foo, self.bar);
    }
}

world.add_handler(|_: Receiver<E>, fetcher: Fetcher<MyQuery>| {
    for my_query in fetcher {
        my_query.print();
    }
});
```

## Access Conflicts

Within a query or set of queries, it is possible to create component accesses that violate Rust's aliasing rules.
Consider:

```should_panic
# use evenio::prelude::*;
# #[derive(Event)] struct E;
# #[derive(Component)] struct A;
# let mut world = World::new();
// Panics!
world.add_handler(|_: Receiver<E>, _: Fetcher<&mut A>, _: Fetcher<&A>| {});
```

Because both fetchers are capable of accessing the same entity, it is possible to create `&mut A` and `&A` references to the same component, thus triggering Undefined Behavior.
`add_handler` guards against this by panicking if the handler parameters have any possibility of causing unsoundness.

There are a number of ways to resolve this situation, but let's look at one way using `With` and `Not`:

```rust
# use evenio::prelude::*;
# #[derive(GlobalEvent)] struct E;
# #[derive(Component)] struct A;
# #[derive(Component)] struct B;
# let mut world = World::new();
// Doesn't panic.
world.add_handler(|_: Receiver<E>, _: Fetcher<(&mut A, With<&B>)>, _: Fetcher<(&A, Not<&B>)>| {});
```

The set of entities matched by both queries are now disjoint.
Therefore, there is no possibility of overlapping access to `A`, and the handler can be added to the world without panicking.

## Singleton Entities

Through the course of development, we may find ourselves wanting to store global data in a location that is easily accessed by handlers.

To facilitate this, `evenio` has the [`Single`] handler parameter.
`Single` is parameterized by a query that must match a single entity in the world.
If the query does not match exactly one entity, then a runtime panic occurs.

To create our global variable, we
1. create a component to hold the data
2. initialize the component and attach it to an entity
3. let `Single` match against it.

```rust
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Event)] struct E;
#[derive(Component)]
struct MyGlobalData {
    foo: i32,
    bar: &'static str,
}

let e = world.spawn();
world.insert(e, MyGlobalData { foo: 123, bar: "data" });

world.add_handler(|_: Receiver<E>, Single(g): Single<&MyGlobalData>| {
    println!("foo: {}, bar: {}", g.foo, g.bar);
});

world.send(E);
```

For handling the situation where `Single` fails, see [`TrySingle`].

For global data scoped to a single handler, see [`Local`].

[`Single`]: crate::fetch::Single
[`TrySingle`]: crate::fetch::TrySingle
[`Local`]: crate::handler::Local

# Targeted Events

Events in `evenio` come in two flavors: _global_ and _targeted_.
- Global events are not targeted at a particular entity.
- Targeted events are directed at a particular entity.
  For instance, the standard [`Insert`], [`Remove`], and [`Despawn`] events are targeted.
- Untargeted events are everything else.

Targeted events enable handlers to efficiently ignore events whose targets do not match a given [`Query`].

To create a targeted event using the `Event` derive macro, we mark an `EntityId` field with the `#[event(target)]` attribute.
Events without the `#[event(target)]` attribute are untargeted.

```rust
# use evenio::prelude::*;
#[derive(Event)]
struct MyTargetedEvent {
    #[event(target)] // Works on tuple struct fields as well.
    target: EntityId,
    data: i32,
}
```

To listen for a targeted event, we must use the second type parameter of `Receiver` or `ReceiverMut`.
If the target matches the query, the query data is accessible in the `query` field of the receiver.
Otherwise, the handler is not run.

```rust
# use evenio::prelude::*;
# #[derive(Event)]
# struct MyTargetedEvent {
#     #[event(target)]
#     target: EntityId,
#     data: i32,
# }
# let mut world = World::new();
#[derive(Component)]
struct Health(i32);

#[derive(Component)]
struct Stamina(i32);

world.add_handler(|mut receiver: Receiver<MyTargetedEvent, (&mut Health, &Stamina)>| {
    let (health, stamina) = receiver.query;

    if stamina.0 == 100 {
        health.0 += receiver.event.data;
    }
});

let e = world.spawn();
world.insert(e, Health(20));
world.insert(e, Stamina(100));

world.send(MyTargetedEvent {
    target: e,
    data: 10,
});

assert_eq!(world.get::<Health>(e).unwrap().0, 30);
```

[`Query`]: crate::query::Query
[`Insert`]: crate::event::Insert
[`Remove`]: crate::event::Remove
[`Despawn`]: crate::event::Despawn

# Immutable Components

When defining a component, we have the option to mark the component as immutable.
An immutable component disallows direct `&mut` access, but permits manipulation through `Insert` and `Remove` events.
This gives the component author more control over its usage.

Let's say we had a component to represent [UUID]s for entities.
UUIDs are usually not meant to change during the lifetime of the entity, so let's see how we can prevent mutation entirely.

To start, we'll mark the component as immutable.

```rust
# use evenio::prelude::*;
/// A universally unique identifier for an entity.
#[derive(Component)]
#[component(immutable)]
struct Uuid(u128);
```

Attempting to get a mutable reference to the UUID component will fail at compile time:

```compile_fail
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# #[component(immutable)]
# struct Uuid(u128);
let e = world.spawn();
world.insert(e, Uuid(12345));

// Does not compile.
world.get_mut::<Uuid>(e);
```

The UUID could still be changed using the `Insert` event, so let's raise an error if the UUID would be overwritten.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# #[component(immutable)]
# struct Uuid(u128);
world.add_handler(detect_uuid_overwrite);

let e = world.spawn();
world.insert(e, Uuid(123)); // Doesn't panic
world.insert(e, Uuid(456)); // Panics

fn detect_uuid_overwrite(r: Receiver<Insert<Uuid>, (EntityId, With<&Uuid>)>) {
    panic!("attempt to overwrite UUID on {:?}", r.query.0)
}
```

To make this completely airtight, we'll also need to protect against removing the component with `Remove`.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# #[component(immutable)]
# struct Uuid(u128);
world.add_handler(detect_uuid_remove);

let e = world.spawn();
world.remove::<Uuid>(e); // Doesn't panic
world.insert(e, Uuid(123));
world.remove::<Uuid>(e); // Panics

fn detect_uuid_remove(r: Receiver<Remove<Uuid>, (EntityId, With<&Uuid>)>) {
    panic!("attempt to remove UUID on {:?}", r.query.0)
}
```

[UUID]: https://en.wikipedia.org/wiki/Universally_unique_identifier

# Immutable Events

Like components, events can be marked as immutable.
Doing so will prevent users from mutating or consuming the event.

```compile_fail
# use evenio::prelude::*;
# let mut world = World::new();
#[derive(Event)]
#[event(immutable)]
struct MyEvent;

// Compile error.
world.add_handler(|_: ReceiverMut<MyEvent>| {});
```
