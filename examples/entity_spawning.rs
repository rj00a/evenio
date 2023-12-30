use evenio::prelude::*;

fn main() {
    let mut world = World::new();

    world.add_system(spawn_some_entities);

    world.send(Foo);

    eprintln!("{:#?}", world.archetypes());
}

#[derive(Event)]
struct Foo;

#[derive(Component)]
struct MyComponent(i32);

fn spawn_some_entities(
    _: Receiver<Foo>,
    mut sender: Sender<(Spawn, Insert<MyComponent>, Remove<MyComponent>)>,
) {
    println!("spawning entities");

    let e = sender.spawn();
    sender.send(Insert::new(e, MyComponent(123)));
    sender.send(Remove::<MyComponent>::new(e));
}
