# Entities and Components

Entities and components are the bread and butter of any ECS framework.

Entities make up the _things_ in your application, such as monsters, players, cameras, GUI elements, or items.
By itself, an entity is just a unique identifier ([`EntityId`]). It is the entity's set of _components_ that differentiate a player from a monster, or a camera from an item.
Components hold the data while handlers operate on entities with certain sets of components.

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

In the next chapter, we'll see how to operate on component data from within handlers.

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
