use evenio::prelude::*;

fn main() {
    let mut world = World::new();

    world.add_system(spawn_some_entities).unwrap();

    world.send(Foo);

    eprintln!("{:#?}", world.entities());
}

#[derive(Event)]
struct Foo;

fn spawn_some_entities(_: &Foo, mut sender: Sender<Spawn>) {
    println!("spawning entities");

    sender.spawn();
    sender.spawn();
    sender.spawn();
    sender.spawn();
}

