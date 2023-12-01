use std::error::Error;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::sync::{Mutex, RwLock};

use evenio_macros::all_tuples;
use slab::Slab;

use crate::access::SystemAccess;
use crate::archetype::{Archetype, ArchetypeId};
use crate::bit_set::{BitSet, BitSetIndex};
use crate::debug_checked::GetDebugChecked;
use crate::event::{EventId, EventPtr};
use crate::exclusive::Exclusive;
use crate::world::{FromWorld, UnsafeWorldCell, World};

#[derive(Debug)]
pub(crate) struct Systems {
    /// Maps event indices to the list of all systems that handle the event.
    event_to_systems: Vec<SystemList>,
    /// Maps [`SystemId`]s to the system's location in `event_to_systems`.
    locations: Slab<SystemLocation>,
}

impl Systems {
    pub(crate) fn new() -> Self {
        Self {
            event_to_systems: vec![],
            locations: Slab::new(),
        }
    }

    #[track_caller]
    pub(crate) fn add(&mut self, system: Box<dyn System>, mut info: SystemInfo) -> SystemId {
        let id = self.locations.insert(SystemLocation {
            event: info.receive_event,
            list_idx: u32::MAX,
        });

        if id >= SystemId::NULL.0 as usize {
            panic!("too many systems added");
        }

        let system_id = SystemId(id as u32);

        info.system_id = system_id;

        self.init_event(info.receive_event);

        let list = &mut self.event_to_systems[info.receive_event.index() as usize];

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
            let location_idx = list.entries[list_idx].info.system_id.0;
            self.locations
                .get_mut(location_idx as usize)
                .unwrap()
                .list_idx = list_idx as u32;
        }

        system_id
    }

    /// Ensures `event_id` is registered in the `event_to_systems` map.
    pub(crate) fn init_event(&mut self, event_id: EventId) {
        let event_idx = event_id.index() as usize;

        if event_idx >= self.event_to_systems.len() {
            self.event_to_systems.extend(
                (0..event_idx - self.event_to_systems.len() + 1).map(|_| SystemList::default()),
            );
        }
    }

    pub(crate) fn remove(&mut self, id: SystemId) -> Option<(Box<dyn System>, SystemInfo)> {
        self.locations.try_remove(id.0 as usize).map(|loc| {
            let list = &mut self.event_to_systems[loc.event.index() as usize];

            let entry = list.entries.remove(loc.list_idx as usize);

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

            for entry in &mut list.entries[loc.list_idx as usize..] {
                self.locations
                    .get_mut(entry.info.system_id.0 as usize)
                    .unwrap()
                    .list_idx -= 1;
            }

            (entry.system, *entry.info)
        })
    }

    #[inline]
    pub(crate) unsafe fn systems_for_event_unchecked_mut(
        &mut self,
        event_id: EventId,
    ) -> &mut [SystemListEntry] {
        &mut self
            .event_to_systems
            .get_debug_checked_mut(event_id.index() as usize)
            .entries
    }

    #[inline]
    pub(crate) fn get(&self, system_id: SystemId) -> Option<(&dyn System, &SystemInfo)> {
        self.locations.get(system_id.0 as usize).map(|loc| {
            let entry =
                &self.event_to_systems[loc.event.index() as usize].entries[loc.list_idx as usize];
            (entry.system.as_ref(), entry.info.as_ref())
        })
    }

    #[inline]
    pub(crate) fn get_mut(
        &mut self,
        system_id: SystemId,
    ) -> Option<(&mut dyn System, &SystemInfo)> {
        self.locations.get_mut(system_id.0 as usize).map(|loc| {
            let entry = &mut self.event_to_systems[loc.event.index() as usize].entries
                [loc.list_idx as usize];
            (entry.system.as_mut(), entry.info.as_ref())
        })
    }

    pub(crate) fn len(&self) -> usize {
        self.locations.len()
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

#[derive(Debug)]
#[non_exhaustive]
pub struct SystemInfo {
    pub priority: SystemPriority,
    pub receive_event: EventId,
    pub access: SystemAccess,
    pub system_id: SystemId,
}

impl SystemInfo {
    pub(crate) fn new() -> Self {
        Self {
            priority: SystemPriority::default(),
            receive_event: EventId::NULL,
            access: SystemAccess::default(),
            system_id: SystemId::NULL,
        }
    }
}

/// The location of a system in [`Systems::event_to_systems`].
#[derive(Debug)]
struct SystemLocation {
    /// The event handled by this system. This is an index into
    /// [`Systems::event_to_systems`].
    event: EventId,
    /// Index into the [`SystemList`].
    list_idx: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct SystemId(u32);

impl SystemId {
    pub const NULL: Self = Self(u32::MAX);

    pub const fn to_bits(self) -> u64 {
        self.0 as u64
    }

    pub const fn from_bits(bits: u64) -> Option<Self> {
        if bits <= u32::MAX as u64 {
            Some(Self(bits as u32))
        } else {
            None
        }
    }
}

impl Default for SystemId {
    fn default() -> Self {
        Self::NULL
    }
}

impl BitSetIndex for SystemId {
    fn bit_set_index(self) -> usize {
        self.0 as usize
    }

    fn from_bit_set_index(idx: usize) -> Self {
        Self(idx as u32)
    }
}

pub trait InitSystem<Marker> {
    type System: System;

    fn init_system(
        self,
        world: &mut World,
        config: &mut SystemConfig,
    ) -> Result<Self::System, Box<dyn Error>>;
}

pub trait System: Send + Sync + fmt::Debug + 'static {
    unsafe fn run(&mut self, system_info: &SystemInfo, event_ptr: EventPtr, world: UnsafeWorldCell);
    /// Informs this system of a new archetype that it might need access to.
    /// Returns whether or not the system is interested in the new archetype.
    unsafe fn new_archetype(&mut self, id: ArchetypeId, arch: &Archetype) -> bool;
    /// Informs this system that an archetype it accesses has had its column
    /// pointers change. Any column pointers for this archetype are now invalid,
    /// and new column pointers should be acquired.
    unsafe fn refresh_archetype(&mut self, id: ArchetypeId, arch: &Archetype);
}

#[derive(Clone, Default, Debug)]
#[non_exhaustive]
pub struct SystemConfig {
    pub access: SystemAccess,
    pub priority: SystemPriority,
    pub received_event: EventId,
    pub sent_events: BitSet<EventId>,
}

#[derive(Clone, Debug)]
pub enum SystemInitError {
    InvalidEventId(EventId),
    ConflictingEventType,
    ConflictingEventAccess,
    ConflictingEventQueueAccess,
}

impl fmt::Display for SystemInitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self {
            SystemInitError::InvalidEventId(id) => {
                if *id == EventId::NULL {
                    "system did not specify an event to handle"
                } else {
                    "system specified an invalid event ID to handle"
                }
            }
            SystemInitError::ConflictingEventType => {
                "system parameters specify differing event types"
            }
            SystemInitError::ConflictingEventAccess => {
                "system parameters have conflicting access to the handled event"
            }
            SystemInitError::ConflictingEventQueueAccess => {
                "system parameters have conflicting access to the event queue"
            }
        };

        write!(f, "{msg}")
    }
}

impl Error for SystemInitError {}

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
    type State: Send + Sync + 'static;
    type Item<'s, 'a>: SystemParam<State = Self::State>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>>;

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a>;
}

macro_rules! impl_system_param_tuple {
    ($(($P:ident, $s:ident)),*) => {
        impl<$($P: SystemParam),*> SystemParam for ($($P,)*) {
            type State = ($($P::State,)*);

            type Item<'s, 'a> = ($($P::Item<'s, 'a>,)*);

            #[inline]
            fn init(_world: &mut World, _config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
                Ok((
                    $(
                        $P::init(_world, _config)?,
                    )*
                ))
            }

            #[inline]
            unsafe fn get_param<'s, 'a>(
                ($($s,)*): &'s mut Self::State,
                _system_info: &'a SystemInfo,
                _event_ptr: EventPtr<'a>,
                _world: UnsafeWorldCell<'a>,
            ) -> Self::Item<'s, 'a> {
                (
                    $(
                        $P::get_param($s, _system_info, _event_ptr, _world),
                    )*
                )
            }
        }
    }
}

all_tuples!(impl_system_param_tuple, 0, 15, P, s);

impl<Marker, F> InitSystem<Marker> for F
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    type System = FunctionSystem<Marker, F>;

    fn init_system(
        self,
        world: &mut World,
        config: &mut SystemConfig,
    ) -> Result<Self::System, Box<dyn Error>> {
        Ok(FunctionSystem {
            func: self,
            state: <F::Param as SystemParam>::init(world, config)?,
        })
    }
}

pub struct FunctionSystem<Marker, F: SystemParamFunction<Marker>> {
    func: F,
    state: <F::Param as SystemParam>::State,
}

impl<Marker, F> FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
{
    pub fn into_inner(self) -> (F, <F::Param as SystemParam>::State) {
        (self.func, self.state)
    }
}

impl<Marker, F> System for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    unsafe fn run(
        &mut self,
        system_info: &SystemInfo,
        event_ptr: EventPtr,
        world: UnsafeWorldCell,
    ) {
        let param =
            <F::Param as SystemParam>::get_param(&mut self.state, system_info, event_ptr, world);
        self.func.run(param);
    }

    unsafe fn refresh_archetype(&mut self, id: ArchetypeId, arch: &Archetype) {
        todo!()
    }

    unsafe fn new_archetype(&mut self, id: ArchetypeId, arch: &Archetype) -> bool {
        todo!()
    }
}

impl<Marker, F> fmt::Debug for FunctionSystem<Marker, F>
where
    F: SystemParamFunction<Marker>,
    <F::Param as SystemParam>::State: fmt::Debug,
    Marker: 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("FunctionSystem")
            // Functions don't usually implement Debug, so use the type name instead.
            .field("func", &std::any::type_name::<F>())
            .field("state", &self.state)
            .finish()
    }
}

pub trait SystemParamFunction<Marker>: Send + Sync + 'static {
    type Param: SystemParam;

    unsafe fn run(&mut self, param: <Self::Param as SystemParam>::Item<'_, '_>);
}

macro_rules! impl_system_param_function {
    ($(($P:ident, $p:ident)),*) => {
        impl<F, $($P: SystemParam),*> SystemParamFunction<fn($($P),*)> for F
        where
            F: FnMut($($P),*) + FnMut($($P::Item<'_, '_>),*) + Send + Sync + 'static,
        {
            type Param = ($($P,)*);

            unsafe fn run(
                &mut self,
                ($($p,)*): <Self::Param as SystemParam>::Item<'_, '_>
            ) {
                (self)($($p),*)
            }
        }
    }
}

all_tuples!(impl_system_param_function, 0, 15, P, p);

#[derive(Debug)]
pub struct Local<'s, T>
where
    T: FromWorld + Send + 'static,
{
    state: &'s mut T,
}

impl<T: FromWorld + Send + 'static> SystemParam for Local<'_, T> {
    type State = Exclusive<T>;

    type Item<'s, 'a> = Local<'s, T>;

    fn init(world: &mut World, _config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        Ok(Exclusive::new(T::from_world(world)))
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        Local {
            state: state.get_mut(),
        }
    }
}

impl<T: FromWorld + Send + 'static> Deref for Local<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.state
    }
}

impl<T: FromWorld + Send + 'static> DerefMut for Local<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl SystemParam for SystemId {
    type State = ();

    type Item<'s, 'a> = SystemId;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        system_info.system_id
    }
}

impl SystemParam for &'_ SystemInfo {
    type State = ();

    type Item<'s, 'a> = &'a SystemInfo;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        Ok(())
    }

    unsafe fn get_param<'s, 'a>(
        _state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        _event_ptr: EventPtr<'a>,
        _world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        system_info
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct Before<P = ()>(pub P);

impl<P> Deref for Before<P> {
    type Target = P;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<P> DerefMut for Before<P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<P: SystemParam> SystemParam for Before<P> {
    type State = P::State;

    type Item<'s, 'a> = Before<P::Item<'s, 'a>>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        let res = P::init(world, config)?;
        config.priority = SystemPriority::Before;
        Ok(res)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        Before(P::get_param(state, system_info, event_ptr, world))
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct After<P = ()>(pub P);

impl<P> Deref for After<P> {
    type Target = P;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<P> DerefMut for After<P> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<P: SystemParam> SystemParam for After<P> {
    type State = P::State;

    type Item<'s, 'a> = After<P::Item<'s, 'a>>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        let res = P::init(world, config)?;
        config.priority = SystemPriority::After;
        Ok(res)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        After(P::get_param(state, system_info, event_ptr, world))
    }
}

impl<P: SystemParam> SystemParam for Mutex<P> {
    type State = P::State;

    type Item<'s, 'a> = Mutex<P::Item<'s, 'a>>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        P::init(world, config)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        Mutex::new(P::get_param(state, system_info, event_ptr, world))
    }
}

impl<P: SystemParam> SystemParam for RwLock<P> {
    type State = P::State;

    type Item<'s, 'a> = RwLock<P::Item<'s, 'a>>;

    fn init(world: &mut World, config: &mut SystemConfig) -> Result<Self::State, Box<dyn Error>> {
        P::init(world, config)
    }

    unsafe fn get_param<'s, 'a>(
        state: &'s mut Self::State,
        system_info: &'a SystemInfo,
        event_ptr: EventPtr<'a>,
        world: UnsafeWorldCell<'a>,
    ) -> Self::Item<'s, 'a> {
        RwLock::new(P::get_param(state, system_info, event_ptr, world))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct FakeSystem;

    impl System for FakeSystem {
        unsafe fn run(
            &mut self,
            _system_info: &SystemInfo,
            _event_ptr: EventPtr,
            _world: UnsafeWorldCell,
        ) {
        }

        unsafe fn refresh_archetype(&mut self, _id: ArchetypeId, _arch: &Archetype) {
            unimplemented!()
        }

        unsafe fn new_archetype(&mut self, id: ArchetypeId, arch: &Archetype) -> bool {
            todo!()
        }
    }

    #[test]
    fn add_remove_system() {
        let mut systems = Systems::new();
        let event_id = EventId::from_bits(1).unwrap();

        let mut info_1 = SystemInfo::new();
        info_1.receive_event = event_id;
        let id_1 = systems.add(Box::new(FakeSystem), info_1);

        let mut info_2 = SystemInfo::new();
        info_2.receive_event = event_id;
        info_2.priority = SystemPriority::After;
        let id_2 = systems.add(Box::new(FakeSystem), info_2);

        let mut info_3 = SystemInfo::new();
        info_3.receive_event = event_id;
        info_3.priority = SystemPriority::Before;
        let id_3 = systems.add(Box::new(FakeSystem), info_3);

        let mut it = unsafe {
            systems
                .systems_for_event_unchecked_mut(event_id)
                .into_iter()
                .map(|entry| entry.info.system_id)
        };

        assert_eq!(it.next(), Some(id_3));
        assert_eq!(it.next(), Some(id_1));
        assert_eq!(it.next(), Some(id_2));

        assert_eq!(
            systems.get(id_2).map(|(_, info)| info.system_id),
            Some(id_2)
        );

        assert_eq!(
            systems.remove(id_3).map(|(_, info)| info.system_id),
            Some(id_3)
        );
        assert!(systems.remove(id_3).is_none());

        assert_eq!(
            systems.remove(id_1).map(|(_, info)| info.system_id),
            Some(id_1)
        );
        assert!(systems.remove(id_1).is_none());

        assert_eq!(
            systems.remove(id_2).map(|(_, info)| info.system_id),
            Some(id_2)
        );
        assert!(systems.remove(id_2).is_none());

        assert_eq!(systems.len(), 0);
    }
}
