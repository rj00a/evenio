# Targeted Events

Events in `evenio` come in two flavors: _targeted_ and _untargeted_.
- Targeted events are directed at a particular entity.
  For instance, the standard [`Insert`], [`Remove`], and [`Despawn`] events are targeted.
- Untargeted events are everything else.

Targeted events enable systems to efficiently ignore events whose targets do not match a given [`Query`].

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
Otherwise, the system is not run.

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

world.add_system(|mut receiver: Receiver<MyTargetedEvent, (&mut Health, &Stamina)>| {
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

assert_eq!(world.get_component::<Health>(e).unwrap().0, 30);
```

[`Query`]: crate::query::Query
[`Insert`]: crate::event::Insert
[`Remove`]: crate::event::Remove
[`Despawn`]: crate::event::Despawn
