use evenio::*;

pub fn main() {
    let mut world = World::new();

    world.add_system(my_system).unwrap();
}

fn my_system() {}
