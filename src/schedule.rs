//! Graph-based schedule for sorted system ordering

use petgraph::graphmap::DiGraphMap;

use crate::system::SystemInfoPtr;

#[derive(Debug)]
pub(crate) struct SystemSchedule {
    /// Directed graph of [`SystemInfoPtr`]s representing system dependencies.
    graph: DiGraphMap<SystemInfoPtr, ()>,
    /// List of systems ordered in toposort of the graph.
    cached_ordering: Vec<SystemInfoPtr>,
}

impl SystemSchedule {
    pub(crate) fn new() -> Self {
        SystemSchedule {
            graph: DiGraphMap::new(),
            cached_ordering: Vec::new(),
        }
    }

    pub(crate) fn insert(&mut self, system: SystemInfoPtr) -> Result<(), ()> {
        self.graph.add_node(system);

        self.invalidate()
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
