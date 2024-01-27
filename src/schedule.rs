//! Graph-based schedule for sorted system ordering

use petgraph::graphmap::DiGraphMap;

use crate::{prelude::SystemId, system::SystemInfo};

#[derive(Debug)]
pub(crate) struct SystemSchedule {
    /// Directed graph of [`SystemId`]s representing system dependencies.
    graph: DiGraphMap<SystemId, ()>,
    /// List of [`SystemId`]s ordered in toposort of the graph.
    cached_ordering: Vec<SystemId>,
}

unsafe impl Sync for SystemSchedule {}

impl SystemSchedule {
    pub(crate) fn new() -> Self {
        SystemSchedule {
            graph: DiGraphMap::new(),
            cached_ordering: Vec::new(),
        }
    }

    pub(crate) fn insert(&mut self, info: &SystemInfo) -> Result<(), ()> {
        let system = info.id();
        self.graph.add_node(system);

        for before in &info.dependencies().before {
            self.graph.add_edge(system, *before, ());
        }
        for after in &info.dependencies().after {
            self.graph.add_edge(*after, system, ());
        }

        self.invalidate()
    }

    pub(crate) fn remove(&mut self, system: SystemId) {
        self.graph.remove_node(system);

        // cycle errors should panic at insert, shouldn't reach here
        self.invalidate().unwrap();
    }

    pub(crate) fn systems(&self) -> &[SystemId] {
        &self.cached_ordering
    }

    fn invalidate(&mut self) -> Result<(), ()> {
        match petgraph::algo::toposort(&self.graph, None) {
            Ok(cache) => {
                self.cached_ordering = cache;
                Ok(())
            }
            Err(_) => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use evenio_macros::Event;

    use crate::{
        event::{Receiver, ReceiverMut},
        prelude::{IntoSystem, World},
    };

    #[derive(Debug, PartialEq, Eq)]
    enum MyOrder {
        First,
        Second,
        Third,
    }

    #[derive(Event)]
    struct OrderEvent(Vec<MyOrder>);

    #[test]
    fn correct_schedule_ordering() {
        let mut world = World::new();

        let second =
            world.add_system(|mut r: ReceiverMut<OrderEvent>| r.event.0.push(MyOrder::Second));
        let _first = world.add_system(
            (|mut r: ReceiverMut<OrderEvent>| r.event.0.push(MyOrder::First)).before(second),
        );
        let third = world.add_system(
            (|mut r: ReceiverMut<OrderEvent>| r.event.0.push(MyOrder::Third)).after(second),
        );
        let _check = world.add_system(
            (|r: Receiver<OrderEvent>| {
                assert_eq!(
                    r.event.0.as_slice(),
                    &[MyOrder::First, MyOrder::Second, MyOrder::Third]
                )
            })
            .after(third),
        );
    }
}
