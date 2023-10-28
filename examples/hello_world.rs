use evenio::prelude::*;

#[derive(Event)]
pub struct MyEvent;

pub fn main() {
    let mut world = World::new();

    world.add_system(my_system).unwrap();

    world.send_event(MyEvent);

    println!("\n{world:#?}");
}

fn my_system(_: &MyEvent) {
    println!("hello, world!");
}
