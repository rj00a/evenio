use std::cmp::Ordering;

use crate::component::ComponentId;
use crate::entity::EntityId;

#[derive(Debug, Default)]
pub(crate) struct CommandQueue {
    commands: Vec<CommandQueueEntry>,
}

impl CommandQueue {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn push(&mut self, cmd: Command) {
        let idx = self.commands.len();
        self.commands.push(CommandQueueEntry { cmd, idx });
    }

    pub(crate) fn drain(&mut self) -> impl Iterator<Item = Command> + '_ {
        self.commands.sort_unstable();
        self.commands.drain(..).map(|e| e.cmd)
    }
}

#[derive(Debug)]
struct CommandQueueEntry {
    cmd: Command,
    idx: usize,
}

impl Ord for CommandQueueEntry {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.cmd, other.cmd) {
            (
                Command::Insert {
                    entity: left_entity,
                    component_id: left_component_id,
                    ..
                },
                Command::Insert {
                    entity: right_entity,
                    component_id: right_component_id,
                    ..
                },
            ) => left_entity
                .cmp(&right_entity)
                .then(left_component_id.cmp(&right_component_id)),
            (
                Command::Insert {
                    entity: left_entity,
                    component_id: left_component_id,
                    ..
                },
                Command::Remove {
                    entity: right_entity,
                    component_id: right_component_id,
                },
            ) => left_entity
                .cmp(&right_entity)
                .then(left_component_id.cmp(&right_component_id)),
            (
                Command::Insert {
                    entity: left_entity,
                    ..
                },
                Command::Despawn {
                    entity: right_entity,
                },
            ) => left_entity.cmp(&right_entity).then(Ordering::Greater),
            (
                Command::Remove {
                    entity: left_entity,
                    component_id: left_component_id,
                },
                Command::Insert {
                    entity: right_entity,
                    component_id: right_component_id,
                    ..
                },
            ) => left_entity
                .cmp(&right_entity)
                .then(left_component_id.cmp(&right_component_id)),
            (
                Command::Remove {
                    entity: left_entity,
                    component_id: left_component_id,
                },
                Command::Remove {
                    entity: right_entity,
                    component_id: right_component_id,
                },
            ) => left_entity
                .cmp(&right_entity)
                .then(left_component_id.cmp(&right_component_id)),
            (
                Command::Remove {
                    entity: left_entity,
                    ..
                },
                Command::Despawn {
                    entity: right_entity,
                },
            ) => left_entity.cmp(&right_entity).then(Ordering::Greater),
            (
                Command::Despawn {
                    entity: left_entity,
                },
                Command::Insert {
                    entity: right_entity,
                    ..
                },
            ) => left_entity.cmp(&right_entity).then(Ordering::Less),
            (
                Command::Despawn {
                    entity: left_entity,
                },
                Command::Remove {
                    entity: right_entity,
                    ..
                },
            ) => left_entity.cmp(&right_entity).then(Ordering::Less),
            (
                Command::Despawn {
                    entity: left_entity,
                },
                Command::Despawn {
                    entity: right_entity,
                },
            ) => left_entity.cmp(&right_entity),
        }
        .then(self.idx.cmp(&other.idx))
    }
}

impl PartialOrd for CommandQueueEntry {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for CommandQueueEntry {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(&other).is_eq()
    }
}

impl Eq for CommandQueueEntry {}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Command {
    Insert {
        entity: EntityId,
        component_id: ComponentId,
        component_ptr: *mut u8,
    },
    Remove {
        entity: EntityId,
        component_id: ComponentId,
    },
    Despawn {
        entity: EntityId,
    },
}

/*
impl Command {
    fn entity(&self) -> EntityId {
        match *self {
            Command::Insert { entity, .. } => entity,
            Command::Remove { entity, .. } => entity,
            Command::Despawn { entity, .. } => entity,
        }
    }
}
*/
