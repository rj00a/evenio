use std::any::Any;
use std::error::Error;
use std::fmt;
use std::ptr::NonNull;

use slab::Slab;

use crate::util::GetDebugChecked;
use crate::{Event, EventId, Take, World};

#[derive(Debug)]
pub(crate) struct Systems {
    /// Maps event IDs to the list of all systems that handle the event.
    event_to_systems: Vec<SystemList>,
    /// Maps [`SystemId`]s to the system's location in `event_to_systems`.
    locations: Slab<SystemLocation>,
}

impl Systems {
    #[track_caller]
    pub(crate) fn add_system(&mut self, system: Box<dyn System>, mut info: SystemInfo) -> SystemId {
        if self.locations.len() >= (SystemId::NULL.0 - 1) as usize {
            panic!("too many systems added");
        }

        let system_id = SystemId(self.locations.insert(SystemLocation {
            event_id: info.event_id,
            index: u32::MAX,
        }) as u32);

        info.id = system_id;

        let event_idx = info.event_id.0 as usize;

        if event_idx >= self.event_to_systems.len() {
            self.event_to_systems.extend(
                (0..event_idx - self.event_to_systems.len() + 1).map(|_| Default::default()),
            );
        }

        let list = &mut self.event_to_systems[event_idx];

        let entry = SystemListEntry {
            system,
            info: Box::new(info),
        };

        let update_locations_start = match entry.info.priority {
            SystemPriority::Before => {
                list.entries.insert(list.before_index as usize, entry);
                list.before_index += 1;
                list.after_index += 1;
                list.before_index as usize - 1
            }
            SystemPriority::Normal => {
                list.entries.insert(list.after_index as usize, entry);
                list.after_index += 1;
                list.after_index as usize - 1
            }
            SystemPriority::After => {
                list.entries.push(entry);
                list.entries.len() - 1
            }
        };

        for list_idx in update_locations_start..list.entries.len() {
            let location_idx = list.entries[list_idx].info.id.0 as usize;
            self.locations[location_idx].index = list_idx as u32;
        }

        system_id
    }

    pub(crate) fn remove_system(&mut self, id: SystemId) -> Option<(Box<dyn System>, SystemInfo)> {
        self.locations.try_remove(id.0 as usize).map(|loc| {
            let list = &mut self.event_to_systems[loc.event_id.0 as usize];

            let entry = list.entries.remove(loc.index as usize);

            match entry.info.priority {
                SystemPriority::Before => {
                    list.before_index -= 1;
                    list.after_index -= 1;
                }
                SystemPriority::Normal => {
                    list.after_index -= 1;
                }
                SystemPriority::After => {}
            }

            for entry in &mut list.entries[loc.index as usize..] {
                self.locations[entry.info.id.0 as usize].index -= 1;
            }

            (entry.system, *entry.info)
        })
    }

    pub(crate) fn systems_for_event(
        &self,
        event_id: EventId,
    ) -> impl Iterator<Item = (&dyn System, &SystemInfo)> {
        let it = match self.event_to_systems.get(event_id.0 as usize) {
            Some(list) => list.entries.iter(),
            None => Default::default(),
        };

        it.map(|entry| (entry.system.as_ref(), entry.info.as_ref()))
    }

    pub(crate) fn systems_for_event_mut(
        &mut self,
        event_id: EventId,
    ) -> impl Iterator<Item = (&mut dyn System, &SystemInfo)> {
        let it = match self.event_to_systems.get_mut(event_id.0 as usize) {
            Some(list) => list.entries.iter_mut(),
            None => Default::default(),
        };

        it.map(|entry| (entry.system.as_mut(), entry.info.as_ref()))
    }

    #[inline]
    pub(crate) unsafe fn systems_for_event_unchecked_mut(
        &mut self,
        event_id: EventId,
    ) -> &mut [SystemListEntry] {
        &mut self
            .event_to_systems
            .get_debug_checked_mut(event_id.0 as usize)
            .entries
    }

    #[inline]
    pub(crate) fn system(&self, system_id: SystemId) -> Option<(&dyn System, &SystemInfo)> {
        self.locations.get(system_id.0 as usize).map(|loc| {
            let entry = &self.event_to_systems[loc.event_id.0 as usize].entries[loc.index as usize];
            (entry.system.as_ref(), entry.info.as_ref())
        })
    }

    #[inline]
    pub(crate) fn system_mut(
        &mut self,
        system_id: SystemId,
    ) -> Option<(&mut dyn System, &SystemInfo)> {
        self.locations.get_mut(system_id.0 as usize).map(|loc| {
            let entry =
                &mut self.event_to_systems[loc.event_id.0 as usize].entries[loc.index as usize];
            (entry.system.as_mut(), entry.info.as_ref())
        })
    }

    pub(crate) fn len(&self) -> usize {
        self.locations.len()
    }
}

impl Systems {
    pub(crate) fn new() -> Self {
        Self {
            event_to_systems: vec![],
            locations: Slab::new(),
        }
    }
}

#[derive(Debug, Default)]
struct SystemList {
    before_index: u32,
    after_index: u32,
    entries: Vec<SystemListEntry>,
}

#[derive(Debug)]
pub(crate) struct SystemListEntry {
    pub(crate) system: Box<dyn System>,
    pub(crate) info: Box<SystemInfo>,
}

/// The location of a system in [`Systems::event_to_systems`].
#[derive(Debug)]
struct SystemLocation {
    event_id: EventId,
    /// Index into the [`SystemList`].
    index: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SystemId(u32);

impl SystemId {
    pub const NULL: Self = Self(u32::MAX);

    pub fn to_bits(self) -> u64 {
        self.0 as u64
    }

    pub fn from_bits(bits: u64) -> Self {
        Self(bits as u32)
    }
}

pub trait System: Send + Sync + fmt::Debug + Any {
    fn init(&mut self, world: &mut World, info: &mut SystemInfo) -> Result<(), Box<dyn Error>>;

    unsafe fn run(&mut self, event_ptr: NonNull<u8>, moved_event: &mut bool);
}

#[derive(Debug)]
#[non_exhaustive]
pub struct SystemInfo {
    pub priority: SystemPriority,
    pub event_id: EventId,
    pub event_access: Option<Access>,
    pub id: SystemId,
}

impl SystemInfo {
    pub(crate) fn new() -> Self {
        Self {
            priority: SystemPriority::default(),
            event_id: EventId::NULL,
            event_access: None,
            id: SystemId::NULL,
        }
    }
}

// TODO: errors: ConflictingAccess, MissingEvent

#[derive(Debug)]
#[non_exhaustive]
pub struct SystemArgs<'a> {
    pub event_ptr: NonNull<u8>,
    pub moved_event: &'a mut bool,
}

/// Describes when a system will run relative to other systems that handle the
/// same event.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Default)]
pub enum SystemPriority {
    Before,
    #[default]
    Normal,
    After,
}

pub trait SystemParam {
    type State;
    type Item<'a>;

    unsafe fn init(world: &mut World, access: &mut SystemAccess) -> Self::State;

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Self::Item<'a>;
}

impl<E: Event> SystemParam for &'_ E {
    type State = ();
    type Item<'a> = &'a E;

    unsafe fn init(world: &mut World, access: &mut SystemAccess) -> Self::State {
        todo!()
    }

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Self::Item<'a> {
        todo!()
    }
}

impl<E: Event> SystemParam for &'_ mut E {
    type State = ();
    type Item<'a> = &'a mut E;

    unsafe fn init(world: &mut World, access: &mut SystemAccess) -> Self::State {
        todo!()
    }

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Self::Item<'a> {
        todo!()
    }
}

impl<E: Event> SystemParam for Take<E> {
    type State = ();

    type Item<'a> = Take<E>;

    unsafe fn init(world: &mut World, access: &mut SystemAccess) -> Self::State {
        todo!()
    }

    unsafe fn get_param<'a>(state: &'a mut Self::State) -> Self::Item<'a> {
        todo!()
    }
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct SystemAccess {
    pub event_id: Option<EventId>,
    pub event: Option<Access>,
    // TODO: archetype_columns: BTreeMap<ArchetypeColumn, Access>,
}

impl SystemAccess {
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Access {
    Read,
    ReadWrite,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct FakeSystem;

    impl System for FakeSystem {
        fn init(
            &mut self,
            _world: &mut World,
            _info: &mut SystemInfo,
        ) -> Result<(), Box<dyn Error>> {
            Ok(())
        }

        unsafe fn run(&mut self, _event_ptr: NonNull<u8>, _moved_event: &mut bool) {}
    }

    #[test]
    fn add_remove_system() {
        let mut systems = Systems::new();
        let event_id = EventId(1);

        let mut info_1 = SystemInfo::new();
        info_1.event_id = event_id;
        let id_1 = systems.add_system(Box::new(FakeSystem), info_1);

        let mut info_2 = SystemInfo::new();
        info_2.event_id = event_id;
        info_2.priority = SystemPriority::After;
        let id_2 = systems.add_system(Box::new(FakeSystem), info_2);

        let mut info_3 = SystemInfo::new();
        info_3.event_id = event_id;
        info_3.priority = SystemPriority::Before;
        let id_3 = systems.add_system(Box::new(FakeSystem), info_3);

        let mut it = systems.systems_for_event(event_id).map(|(_, info)| info.id);
        assert_eq!(it.next(), Some(id_3));
        assert_eq!(it.next(), Some(id_1));
        assert_eq!(it.next(), Some(id_2));

        assert_eq!(systems.system(id_2).map(|(_, info)| info.id), Some(id_2));

        drop(it);

        assert_eq!(
            systems.remove_system(id_3).map(|(_, info)| info.id),
            Some(id_3)
        );
        assert!(systems.remove_system(id_3).is_none());

        assert_eq!(
            systems.remove_system(id_1).map(|(_, info)| info.id),
            Some(id_1)
        );
        assert!(systems.remove_system(id_1).is_none());

        assert_eq!(
            systems.remove_system(id_2).map(|(_, info)| info.id),
            Some(id_2)
        );
        assert!(systems.remove_system(id_2).is_none());

        assert_eq!(systems.len(), 0);
    }
}
