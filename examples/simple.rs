#![allow(missing_docs)]
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

// The `Receiver<Tick>` parameter tells our system to listen for the `Tick`
// event.
fn update_positions_system(_: Receiver<Tick>, entities: Fetcher<(&mut Position, &Velocity)>) {
    // Loop over all entities with both the `Position` and `Velocity` components,
    // and update their positions.
    for (pos, vel) in entities {
        pos.x += vel.x;
        pos.y += vel.y;
    }
}
