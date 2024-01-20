# Enforcing Invariants

**TODO**

## Immutable Components

When defining a component, we have the option to mark the component as immutable.
An immutable component disallows direct `&mut` access, but permits manipulation through `Insert` and `Remove` events.
This gives the component author more control over its usage.

Let's say we had a component to represet [UUID]s for entities.
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
# #![cfg(not(miri))] // TODO: miri compiles successfully when it shouldn't.
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# #[component(immutable)]
# struct Uuid(u128);
let e = world.spawn();
world.insert(e, Uuid(12345));

// Does not compile.
world.get_component_mut::<Uuid>(e);
```

The UUID could still be changed using the `Insert` event, so let's raise an error if the UUID would be overwritten.

```should_panic
# use evenio::prelude::*;
# let mut world = World::new();
# #[derive(Component)]
# #[component(immutable)]
# struct Uuid(u128);
world.add_system(detect_uuid_overwrite);

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
world.add_system(detect_uuid_remove);

let e = world.spawn();
world.remove::<Uuid>(e); // Doesn't panic
world.insert(e, Uuid(123));
world.remove::<Uuid>(e); // Panics

fn detect_uuid_remove(r: Receiver<Remove<Uuid>, (EntityId, With<&Uuid>)>) {
    panic!("attempt to remove UUID on {:?}", r.query.0)
}
```

[UUID]: https://en.wikipedia.org/wiki/Universally_unique_identifier

## Immutable Events

Like components, events can be marked as immutable.
Doing so will prevent users from mutating or consuming the event.

```compile_fail
# #![cfg(not(miri))] // TODO: miri compiles successfully when it shouldn't.
use evenio::prelude::*;
let mut world = World::new();
#[derive(Event)]
#[event(immutable)]
struct MyEvent;

// Compile error.
world.add_system(|_: ReceiverMut<MyEvent>| {});
```