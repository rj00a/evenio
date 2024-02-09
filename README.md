# Evenio

`evenio` is an archetype-based [Entity Component System][ECS] framework for building event-driven programs. 
It aims to have a small but maximally expressive set of features that are easy and efficient to use.

[ECS]: https://en.wikipedia.org/wiki/Entity_component_system

## Features

- In addition to the usual Entities, Components, and Systems, `evenio` introduces _events_ as a first-class citizen.
Rather than restricting systems to run once every frame/update in a fixed order, systems are generalized as _event handlers_.
The control flow of the entire program is then defined by the flow of events between systems.
- Structural changes to the world (such as entity despawning, component additions/removals, etc.) are mediated by events, allowing systems to hook into their occurrence.
- _Targeted events_ enable systems to efficiently filter events based on queries.
- Component types, event types, and systems are identified with generational indices, allowing them to be added and removed dynamically.
- Execute systems in parallel with [Rayon].
- Core of the library does not depend on Rust's type system.
- `no_std` support.

Features such as inter-system parallelism and event batching are planned but not yet implemented.

> **For a full step-by-step introduction, please read the [tutorial book 📚](https://docs.rs/evenio/latest/evenio/tutorial/).**

## Example

Here's how we might make a simple game loop in `evenio`:

```rust
use evenio::prelude::*;

// Define position and velocity components.
#[derive(Component)]
struct Position {
    x: f32,
    y: f32,
}

#[derive(Component)]
struct Velocity {
    x: f32,
    y: f32,
}

// Events can carry data, but for this example we only need a unit struct.
#[derive(Event)]
struct Tick;

pub fn main() {
    // Create a new `World` to store all our data.
    let mut world = World::new();

    // Spawn a new entity and add the `Position` and `Velocity` components to it.
    // We'll store the entity's ID in a variable for later use.
    let e = world.spawn();
    world.insert(e, Position { x: 0.0, y: 0.0 });
    world.insert(e, Velocity { x: 1.0, y: 0.4 });

    let pos = world.get_component::<Position>(e).unwrap();
    println!("Starting position of the entity: ({}, {})", pos.x, pos.y);

    // Add our system to the world.
    world.add_system(update_positions_system);

    // Run our fake "game loop" by sending the `Tick` event every update.
    for _ in 0..50 {
        world.send(Tick);
    }

    // Get the `Position` component from the entity we added earlier.
    let pos = world.get_component::<Position>(e).unwrap();

    println!("Final position of the entity: ({}, {})", pos.x, pos.y);
}

// The `Receiver<Tick>` parameter tells our system to listen for the `Tick` event.
fn update_positions_system(_: Receiver<Tick>, entities: Fetcher<(&mut Position, &Velocity)>) {
    // Loop over all entities with both the `Position` and `Velocity` components, and update their positions.
    for (pos, vel) in entities {
        pos.x += vel.x;
        pos.y += vel.y;
    }
}
```

## Feature Flags
- `std` (_enabled by default_): Enables support for the standard library.
  Without this, `evenio` depends only on `core` and `alloc`.
- `rayon`: Adds parallel iterator support for `Fetcher`. Uses the [Rayon] library.

[Rayon]: https://github.com/rayon-rs/rayon
