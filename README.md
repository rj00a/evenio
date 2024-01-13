# Evenio

_Evenio_ is an archetype-based [Entity Component System](https://en.wikipedia.org/wiki/Entity_component_system) framework for building highly event-driven programs.

In addition to the usual cast of Entities, Components, and Systems, `evenio` introduces _events_ as a first class citizen.
Rather than restricting systems to run once every "update" in a fixed order, systems are generalized and reframed as _event handlers_.
The control flow of the entire program is then defined by the flow of events between systems.
Structural changes to the world are also mediated by events.

Events allow users to build decoupled and robust software modules that can reliably and efficiently respond to changes in the `World`.

> **For a full step-by-step introduction, please see the [guide book 📚](guide).** 

```rust
use evenio::prelude::*;

// Define some components.

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

    // Add our system to the world.
    world.add_system(update_positions);

    // Run our fake "game loop" by sending the `Tick` event every update.
    // The `Receiver<Tick>` parameter tells our system to listen for the `Tick` event.
    for _ in 0..50 {
        world.send(Tick);
    }

    // Get the `Position` component from the entity we added earlier.
    let pos = world.get_component::<Position>(e).unwrap();

    println!("Final position of the entity: ({}, {})", pos.x, pos.y);
}

fn update_positions(_: Receiver<Tick>, entities: Fetcher<(&mut Position, &Velocity)>) {
    // Loop over all entities with both the `Position` and `Velocity` components, and update their positions.
    for (pos, vel) in entities {
        pos.x += vel.x;
        pos.y += vel.y;
    }
}
```

# `#![no_std]` Support

`#![no_std]` environments are supported, but the `alloc` library is required.
You'll also need to disable the default `std` cargo feature.
